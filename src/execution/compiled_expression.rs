use std::fmt::{Display,Formatter,Result};
use super::opcode::OpCode;

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

    /// Push an operation onto the end of the operations stack.
    pub fn push(&mut self, op: OpCode) -> &mut Self {
        self.operations.push(op);
        self
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

    /// Replace any branching operations that refer to Labels by position
    /// with operations that refer to Labels by address (their zero-based position within the operations list). 
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

}
