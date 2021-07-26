use std::fmt::{Display,Formatter,Result};
use std::cmp::{min,max};
use std::str::FromStr;
use super::opcode::OpCode;
use regex::Regex;
use crate::parsing::feel_value::FeelValue;

/// Product of parsing a Feel expression into a postfix ordered series of operations plus a heap of strings. 
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompiledExpression {
    /// Original source code for the Feel expression. 
    pub source: String,

    /// Expression logic in postfix order as a series of operations resembling assembly language. 
    /// The normal execution order is from the first OpCode to the last, with exceptions for looping and branching. 
    pub operations: Vec<OpCode>,

    /// Strings found in the source code are stored in this heap instead of being embedded directly in an OpCode. 
    /// This permits OpCodes to implement Copy. 
    /// OpCodes that refer to strings hold a zero-based index into this List instead of the String itself. 
    pub heap: Vec<String>
}

impl CompiledExpression {
    pub fn new<S: Into<String>>(source_string: S) -> Self {
        CompiledExpression {
            source: source_string.into(),
            operations: Vec::new(),
            heap: Vec::new()
        }
    }

    /// Create a new CompiledExpression from strings that will be parsed into OpCodes. 
    /// If an OpCode string cannot be recognized assume it is a string and create a LoadString op. 
    /// If the string is wrapped in single quotes or double quotes, strip those quotes
    /// before storing the string in the heap. 
    /// 
    /// If should_resolve is false, resolve_jumps will not be called.
    /// 
    /// This method is most useful for creating unit tests or macros that expand into numerous OpCodes. 
    pub fn new_from_strings(source: &str, ops: Vec<String>, should_resolve: bool) -> Self {
        let mut expr = CompiledExpression::new(source);
        for op_string in ops {
            match OpCode::from_str(&op_string) {
                Ok(op) => { expr.push(op); },
                Err(_) => {
                    let op = expr.parse_opcode_with_string(&op_string);
                    expr.push(op);
                }
            };
        }
        if should_resolve {
            expr.resolve_jumps();
        }
        expr
    }

    fn parse_opcode_with_string(&mut self, op_string: &str) -> OpCode {
        if op_string.starts_with("type?(") && op_string.ends_with(")") {
            let type_string = op_string[6..op_string.len()-1].to_string();
            let index = self.find_or_add_to_heap(type_string);
            return OpCode::IsType(index);
        }

        let s = if op_string.starts_with("'") && op_string.ends_with("'") {
            op_string[1..op_string.len()-1].to_string()
        }
        else if op_string.starts_with("\"") && op_string.ends_with("\"") {
            op_string[1..op_string.len()-1].to_string()
        }
        else {
            op_string.to_string()
        };
        let index = self.find_or_add_to_heap(s);
        OpCode::LoadString(index)
    }

    /// Create a new CompiledExpression from a single string that will be split on spaces and parsed into OpCodes. 
    /// 
    /// If an OpCode string has single or double quotes around it, it will be converted into a LoadString op
    /// and the string will be added to the heap. Such quoted strings may contains embedded spaces but not
    /// embedded quotes.
    /// 
    /// If an OpCode string cannot be recognized also assume it is a string and create a LoadString op. 
    /// 
    /// If should_resolve is false, resolve_jumps will not be called.
    /// 
    /// This method is most useful for creating unit tests or macros that expand into numerous OpCodes. 
    pub fn new_from_string(source: &str, should_resolve: bool) -> Self {
        lazy_static! {
            static ref OPCODE_SPLITTER_RE: Regex = Regex::new(r#"[^\s"']+|"([^"]*)"|'([^']*)'"#).unwrap();
        }
        let matches: Vec<String> = OPCODE_SPLITTER_RE.find_iter(source).map(|m| m.as_str().to_string()).collect();
        Self::new_from_strings(source, matches, should_resolve)
    }

    /// Find the zero-based position of the string in the heap if it is present, or None if it is absent. 
    pub fn find_in_heap(&self, heap_string: &str) -> Option<usize> {
        self.heap.iter().position(|s| s == heap_string)
    }

    /// Find and return the zero-based position of the string in the heap, 
    /// adding the string to the end of the heap if necessary.
    pub fn find_or_add_to_heap<S: Into<String>>(&mut self, heap_string: S) -> usize {
        let s = heap_string.into();
        match self.find_in_heap(&s) {
            Some(position) => position,
            None => {
                self.heap.push(s);
                self.heap.len() - 1_usize
            }
        }
    }

    /// Given a string, add that string to the heap if not already present and return an OpCode that references the heap by position. 
    /// Does not add the new OpCode to the expression.
    pub fn new_load_string<S: Into<String>>(&mut self, string_to_load: S) -> OpCode {
        let heap_position = self.find_or_add_to_heap(string_to_load);
        OpCode::LoadString(heap_position) 
    }

    /// Get the string from the given zero-based position in the heap and create a FeelValue::String for it,
    /// unless the index is out of bounds, in which case a FeelValue::Null is returned. 
    pub fn get_from_heap(&self, index: usize) -> FeelValue {
        match self.heap.get(index) {
            Some(s) => FeelValue::String(s.clone()),
            None => FeelValue::Null
        }
    }

    /// Push an operation onto the end of the operations stack.
    pub fn push(&mut self, op: OpCode) -> &mut Self {
        self.operations.push(op);
        self
    }

    /// Find the highest value for label position in the expression.
    fn max_label(&self) -> usize {
        let mut max_position: usize = 0;
        for op in self.operations.iter() {
            match op {
                OpCode::Label(position) => {
                    max_position = max(max_position, *position); 
                },
                _ => ()
            };
        }
        max_position
    }

    /// Find the lowest value for label position in the expression, or zero if there are none.
    fn min_label(&self) -> usize {
        let mut min_position: usize = usize::MAX;
        for op in self.operations.iter() {
            match op {
                OpCode::Label(position) => {
                    min_position = min(min_position, *position); 
                },
                _ => ()
            };
        }
        if min_position == usize::MAX {
            min_position = 0;
        }
        min_position
    }

    /// Add the given value to the position in all Labels, GotoLabels, and ExitLoopLabels.
    /// This is necessary when inserting a subexpression into its enclosing expression,
    /// to ensure that there are no duplicate labels.
    fn shift_labels(&mut self, shift_by: usize) {
        for i in 0..self.operations.len() {
            let op = self.operations[i];
            match op {
                OpCode::Label(label) => {
                    self.operations[i] = OpCode::Label(label + shift_by);
                },
                OpCode::BranchExitLabel { true_label, false_label, null_label } => {
                    self.operations[i] = OpCode::BranchExitLabel { 
                        true_label: true_label + shift_by, 
                        false_label: false_label + shift_by, 
                        null_label: null_label + shift_by
                    };
                },
                OpCode::BranchToLabel { true_label, false_label, null_label } => {
                    self.operations[i] = OpCode::BranchToLabel { 
                        true_label: true_label + shift_by, 
                        false_label: false_label + shift_by, 
                        null_label: null_label + shift_by
                    };
                },
                OpCode::ExitLoopLabel(label) => {
                    self.operations[i] = OpCode::ExitLoopLabel(label + shift_by);
                },
                OpCode::GotoLabel(label) => {
                    self.operations[i] = OpCode::GotoLabel(label + shift_by);
                },
                _ => ()
            }
        }
    }

    /// Find the zero-based address (position) in the operations list that holds the Label 
    /// whose position matches the given value, or None if it is not found.
    pub fn find_label_address(&self, label_position: usize) -> Option<usize> {
        for (address, op) in self.operations.iter().enumerate() {
            match op {
                OpCode::Label(position) => {
                    if *position == label_position {
                        return Some(address);
                    }
                },
                _ => ()
            };
        }
        None
    }

    /// Insert one subexpression into an enclosing expression, renumbering OpCodes as necessary.
    /// The insertion point is after the given label. If no matching label can be found, insert at the end.
    /// 
    /// OpCodes from the subexpression that reference labels will be incremented by an amount sufficient to make them 
    /// exceed the highest label value in the enclosing expression. 
    /// 
    /// LoadString OpCodes will be reindexed to reflect their position in the larger heap that results from the
    /// merger. 
    /// 
    /// NOTE: subexpression must not have had resolve_jumps called on it, as that replaces labels with addresses 
    /// which will no longer be valid after an insert. 
    pub fn insert(&mut self, subexpression: &mut CompiledExpression, insert_after_label: usize) {
      // 1. How big an offset should we apply to labels, gotos and branches?
      let outer_max = self.max_label();
      let inner_min = subexpression.min_label();
      let shift_by = if inner_min > outer_max { 0_usize } else { outer_max - inner_min + 1_usize };
      
      // 2. Shift the labels in the subexpression in place.
      subexpression.shift_labels(shift_by);

      // 3. Find insertion point. 
      let mut insertion_index = match self.find_label_address(insert_after_label) {
          Some(address) => address + 1,
          _ => self.operations.len()
      };

      // 4. Copy OpCodes from the subexpression and insert them into the enclosing expression. 
      //    If they are OpCode::LoadString or IsType values, add the subexpression's string into the enclosing heap if it is not present.
      //    Replace their heap index with an index into the consolidated heap.
      for op in subexpression.operations.iter() {
        match op {
            OpCode::LoadString(subexpr_heap_index) => {
                let s = &subexpression.heap[*subexpr_heap_index];
                let enclosing_heap_index = self.find_or_add_to_heap(s);
                self.operations.insert(insertion_index, OpCode::LoadString(enclosing_heap_index));
            },
            OpCode::IsType(subexpr_heap_index) => {
                let s = &subexpression.heap[*subexpr_heap_index];
                let enclosing_heap_index = self.find_or_add_to_heap(s);
                self.operations.insert(insertion_index, OpCode::IsType(enclosing_heap_index));
            },
            _ => {
                self.operations.insert(insertion_index, *op);
            }
        }
        insertion_index += 1;
      }

    }

    /// Insert after the indicated label a stream of OpCodes that will setup a filter context. 
    ///   - Assume that a list and an empty context are already on top of the value stack to start with
    ///   - Set the "item index" property to zero
    ///   - Set the "item list" property to the list from the stack
    ///   - Set the "item count" property to the length of the list from the stack
    ///   - Set the "item" property to Null
    /// 
    /// The effect on the stack of executing the macro's operations: (list ctx -> ctx)
    pub fn create_filter_context(&mut self, insert_after_label: usize) {
        let filter_context_macro = "'item index' number(0) xset over len 'item count' swap xset 'item list' rot xset 'item' null xset xpush";
        let mut subexpression  = Self::new_from_string(filter_context_macro, false);
        self.insert(&mut subexpression, insert_after_label);
    }

    /// Insert after the indicated label a stream of OpCodes that will check if a filter context
    /// has any more items to iterate over. 
    /// This compares the 'item index' to the 'item count' by consulting the contexts.
    pub fn has_next(&mut self, insert_after_label: usize) {
        let has_next_macro = "'item index' xget 'item count' xget <";
        let mut subexpression  = Self::new_from_string(has_next_macro, false);
        self.insert(&mut subexpression, insert_after_label);
    }

    /// Insert after the indicated label a stream of OpCodes that will retrieve the next item 
    /// in the list being iterated over and increment the 'item index' in the filter context. 
    /// This compares the 'item index' to the 'item count' by consulting the contexts.
    pub fn get_next(&mut self, insert_after_label: usize) {
        let get_next_macro = "'item list' xget 'item index' xget index 'item' swap xup 'item index' incr";
        let mut subexpression  = Self::new_from_string(get_next_macro, false);
        self.insert(&mut subexpression, insert_after_label);
    }

    /// Replace any branching operations that refer to Labels by position
    /// with operations that refer to Labels by address (their zero-based position within the operations list). 
    /// This should be called once after all opcodes have been pushed. 
    pub fn resolve_jumps(&mut self) {
        let range = 0..self.operations.len();
        for index in range {
            let op = self.operations[index];
            match op {
                OpCode::GotoLabel(label) => {
                    if let Some(address) = self.find_label_address(label) {
                        self.operations[index] = OpCode::GotoAddress(address);
                    }
                },
                OpCode::ExitLoopLabel(label) => {
                    if let Some(address) = self.find_label_address(label) {
                        self.operations[index] = OpCode::ExitLoopAddress(address);
                    }
                },
                OpCode::BranchToLabel { true_label, false_label, null_label } => {
                    if let (Some(true_address), Some(false_address), Some(null_address)) = (
                        self.find_label_address(true_label), 
                        self.find_label_address(false_label),
                        self.find_label_address(null_label)
                    ) {
                        self.operations[index] = OpCode::BranchToAddress {
                            true_address: true_address,
                            false_address: false_address,
                            null_address: null_address
                        };
                    }
                },
                OpCode::BranchExitLabel { true_label, false_label, null_label } => {
                    if let (Some(true_address), Some(false_address), Some(null_address)) = (
                        self.find_label_address(true_label), 
                        self.find_label_address(false_label),
                        self.find_label_address(null_label)
                    ) {
                        self.operations[index] = OpCode::BranchExitAddress {
                            true_address: true_address,
                            false_address: false_address,
                            null_address: null_address
                        };
                    }
                },
                _ => ()
            }
        }
    }
}


impl Display for CompiledExpression {
    fn fmt(&self, f: &mut Formatter) -> Result {
        let mut s = String::with_capacity((self.heap.len() + self.operations.len()) * 10 + self.source.len());
        s.push_str(&format!("Compiled Expression:\n  source: {}\n  Operations: [", self.source));
        for op in self.operations.iter() {
            s.push_str(&op.to_string());
        }
        s.push_str(&format!("]\n  heap:[\n"));
        for (pos, heap_string) in self.heap.iter().enumerate() {
            s.push_str(&format!("    {}: {}\n", pos, heap_string));
        }
        s.push_str(&format!("  ]"));
        write!(f, "{}", s)
    }

}



/////////////// TESTS /////////////////

#[cfg(test)]
mod tests {
    use super::CompiledExpression;
    use super::super::opcode::OpCode;

    #[test]
    fn test_find_label_address() {
        let mut expr = CompiledExpression::new("an expression");
        expr
            .push(OpCode::Add)
            .push(OpCode::Label(5));
        let expected_position = 1_usize;
        let actual_position = expr.find_label_address(5).unwrap();
        assert_eq!(expected_position, actual_position);
    }

    #[test]
    fn test_resolve_jumps() {
        let mut expr = CompiledExpression::new("an expression");
        expr
            .push(OpCode::Add)
            .push(OpCode::Label(10))
            .push(OpCode::Subtract)
            .push(OpCode::GotoLabel(10));
        expr.resolve_jumps();
        let expected_op = OpCode::GotoAddress(1);
        assert_eq!(expected_op, expr.operations[3]);
    }

    #[test]
    fn test_find_or_add_to_heap() {
        let mut expr = CompiledExpression::new("an expression");
        assert_eq!(expr.find_or_add_to_heap("zero"), 0_usize);
        assert_eq!(expr.find_or_add_to_heap("one"), 1_usize);
        assert_eq!(expr.find_or_add_to_heap("two"), 2_usize);
        assert_eq!(expr.find_or_add_to_heap("zero"), 0_usize);
    }

    #[test]
    fn test_new_load_string() {
        let mut expr = CompiledExpression::new("an expression");
        assert_eq!(expr.new_load_string("zero"), OpCode::LoadString(0_usize));
        assert_eq!(expr.new_load_string("one"), OpCode::LoadString(1_usize));
        assert_eq!(expr.new_load_string("two"), OpCode::LoadString(2_usize));
        assert_eq!(expr.new_load_string("zero"), OpCode::LoadString(0_usize));
    }

    #[test]
    fn test_new_from_string() {
        let source = r#"'good' 'evil' !="#;
        let expr = CompiledExpression::new_from_string(source, false);
        assert_eq!(OpCode::LoadString(0), expr.operations[0]);
        assert_eq!(OpCode::LoadString(1), expr.operations[1]);
        assert_eq!(OpCode::NotEqual, expr.operations[2]);
        assert_eq!("good".to_string(), expr.heap[0]);
    }

}
