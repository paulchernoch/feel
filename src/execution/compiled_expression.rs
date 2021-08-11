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

    /// Remove optional single or double quotes from start and end of string.
    fn unquote(s: &str) -> String {
        if s.starts_with("'") && s.ends_with("'") {
            s[1..s.len()-1].to_string()
        }
        else if s.starts_with("\"") && s.ends_with("\"") {
            s[1..s.len()-1].to_string()
        }
        else {
            s.to_string()
        }
    }

    fn strip_comments(s: &str) -> String {
        lazy_static! {
            static ref COMMENT_RE: Regex = Regex::new(r#"//.*"#).unwrap();
        }
        COMMENT_RE.replace_all(s, "").to_string()
    }

    fn parse_opcode_with_string(&mut self, op_string: &str) -> OpCode {
        if op_string.starts_with("type?(") && op_string.ends_with(")") {
            let type_string = op_string[6..op_string.len()-1].to_string();
            let index = self.find_or_add_to_heap(Self::unquote(&type_string));
            return OpCode::IsType(index);
        }
        let index = self.find_or_add_to_heap(Self::unquote(op_string));
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
        let uncommented = Self::strip_comments(source);
        let source_without_newlines = uncommented.replace("\n", " ").trim().to_string();
        lazy_static! {
            static ref OPCODE_SPLITTER_RE: Regex = Regex::new(r#"[^\s"']+|"([^"]*)"|'([^']*)'"#).unwrap();
        }
        let matches: Vec<String> = OPCODE_SPLITTER_RE.find_iter(&source_without_newlines)
            .map(|m| m.as_str().trim().to_string())
            .filter(|i| i.len() > 0) // Discard empty strings
            .collect();
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

    /// Append the given subexpression to the end of the this expression and renumber the labels of subexpression 
    /// as necessary so as not to introduce duplicate labels with the same number.
    /// 
    /// See insert for additional semantics.
    /// 
    /// NOTE: subexpression must not have had resolve_jumps called on it, as that replaces labels with addresses 
    /// which will no longer be valid after an insert. 
    pub fn append(&mut self, subexpression: &mut CompiledExpression) {
        let insert_after_label: usize = self.max_label() + 1;
        self.insert(subexpression, insert_after_label)
    }

    pub fn append_str(&mut self, source: &str) {
        let mut new_expr = Self::new_from_string(source, false);
        self.append(&mut new_expr);
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
    /// This code expects that a filter context has already been created from a list.
    /// This pushes the next item on top of the data stack.
    pub fn get_next(&mut self, insert_after_label: usize) {
        let get_next_macro = "
            'item list' xget
            'item index' xget
            index
            dup 'item' swap xup
            'item index' incr
        ";
        let mut subexpression  = Self::new_from_string(get_next_macro, false);
        self.insert(&mut subexpression, insert_after_label);
    }

    /// Create a filter expression and insert into it the following subexpressions:
    ///   - a create_filter_context expression after label(15) 
    ///   - a has_next expression after label(9)
    ///   - a get_item filter expression after label(10)
    ///   - a predicate subexpression after label(12). 
    /// The predicate performs the filtering of a list. It must push a Boolean to indicate if the list item
    /// is to be kept or not. The predicate can load the "item" property from the context
    /// and refer to it in its formula.
    /// 
    /// NOTE: Comments within code are stripped out by parsing. 
    pub fn new_filter(predicate: &mut Self) -> Self {
        let filter_macro = "
            over
            type?(list<Any>) branch(1/2/3) label(1)
            dup
            type?(number) branch(6/7/3) label(6)
            // Decrement index to convert from one-based to zero-based (as expected by index op)
            // but do not decrement a negative index, used as a relative index from the end of the list.
            dup num(0) >= branch(16/17/17) label(16)
            number(1) -
            label(17)
            index
            goto(4)
            label(7)
            dup
            type?(context<>) branch(8/3/3) label(8)
            label(15)
            // insert: create filter context
            list

            // top of loop
            label(9)
            // insert: has item?
            branch(10/11/11) 

            // List does have another item
            label(10)
            // insert: get item

            // Push a copy of the item onto contexts, because filter expression 
            // may refer to item properties without the item keyword.
            dup xpush

            // Filter the value
            label(12)
            // insert: filter expression

            // Pop the item copy from contexts and discard it. 
            xpop drop

            // Test result of filter expression
            branch(13/14/14) 

            // keep item, pushing it onto result list, then back to top of loop
            label(13)
            push
            goto(9)

            // Reject item, then back to top of loop
            label(14)
            drop
            goto(9)

            // List has no more items
            label(11)
            goto(4)

            // Indexing of non-list
            label(2)    
            dup number(0) = branch(5/3/3) 

            // Non-list with index of zero
            label(5)
            drop        
            goto(4)

            // Non-list with non-zero index (or type failure)
            label(3)
            drop drop null

            // Exit
            label(4)
        ";
        let mut filter_expression  = Self::new_from_string(filter_macro, false);
        filter_expression.create_filter_context(15);
        filter_expression.has_next(9);
        filter_expression.get_next(10); 
        filter_expression.insert(predicate, 12);
        filter_expression
    }

    /// Generate the OpCodes for a series of nested for loops that build up a list of results,
    /// one per iteration of the innermost loop. 
    /// This function does not expect that a context is on the data stack to start, hence will create one.
    /// It will add properties to that context necessary to perform the iterations. 
    /// The final result of loop execution is the pushing onto the data stack of "partial"
    /// once it is completely filled with all results from each iteration of the loop. 
    /// 
    ///    loops ........ Each pair of values represents one loop. 
    ///                   The FeelValue must be a FeelValue::Name to use as the loop variable. 
    ///                   The CompiledExpression must either resolve to a list or a range over integers. 
    ///                   If a Range, then the range may be a reversed Range, causing the iteration to be descendent.
    ///    innermost .... This expression may reference any or all of the loop variables. 
    ///                   It will be executed once per iteration.
    ///                   The result of this expression will be pushed into the special variable "partial",
    ///                   a list of results frmo all the iterations. 
    ///                   "innermost" may lookup values from previous iterations of the loop in "partial"
    ///                   and use them in computations. 
    /// 
    /// Scratch variables will be added to the context to keep track of each loop.
    /// Most variables (excluding partial) will be suffixed by a number corresponding to the loop.
    /// The outermost loop will be designated as 1, the one inside that as 2, etc.
    ///     partial ................... List of partial results from all preceding iterations. 
    ///     <variable name> ........... Holds the value of the variable for the current iteration,
    ///                                 taken from the current position in the loop context.
    ///     loop context # ............ Holds the list or range to be iterated over.
    ///     loop position # ........... Current zero-based position of the iteration for the given loop. 
    ///                                 This ranges from zero to loop count # - 1.
    ///                                 For lists, this varies from zero to length - 1. 
    ///                                 For Ranges, this varies from loop start index # to loop stop index # by loop step #.
    ///     loop start index # ........ For lists, this is always zero. 
    ///                                 For Ranges, this is either the lowest value of the range (for ascendent ranges)
    ///                                 or the highest (for descendent ranges).
    ///     loop stop index # ......... For lists, this is always loop count # - 1. 
    ///                                 For Ranges, this is either the highest value of the range (for ascendent ranges)
    ///                                 or the lowest (for descendent ranges).
    ///     loop count # .............. Number of items in the list or length of the range. 
    ///     loop step # ............... How much to add to the position per iteration. 
    ///                                 This is either one (for all lists and ascendant ranges)
    ///                                 or negative one (for descendent ranges). 
    /// The names of the first two context entries (partial and <variable name>) are dictated by the spec. 
    /// The remaining names are internal. 
    pub fn for_loops(loops: &mut Vec<(&FeelValue, &mut CompiledExpression)>, innermost: &mut CompiledExpression) -> CompiledExpression {
        // Because it is tedious to reference items on the stack other than the top three and we need to 
        // set the values of eight context entries, we will initialize all eight properties to Null
        // and store the context in contexts.
        // Subsequent access will be mediated through contexts, instead of against a context near the top of the data stack.
        //
        // Tricky: There will be two Context values in play that point to the same underlying dictionary. 
        //         One will be at or near the top of the data stack and the other inside "contexts". 
        //         This is because you can call xset only against items on the data stack, 
        //         since contexts is a NestedContext and we would not know which of its several contexts 
        //         to store the key-value pair in. 
        //         Once a variable is initialized to something via xset, it can be subsequently updated
        //         with xup, which acts against contexts and can find where the variable is defined. 
        let loop_count = loops.len();
        let innermost_label = 1_usize;
        let mut expr = CompiledExpression::new_from_string("
                // Create a single context to hold variables for all loops.
                xload 'partial' list xset
                // Make a duplicate context and push it to contexts.
                // The duplicate will share the same storage of key-value pairs as the original.
                dup xpush
        ", false);

        for loop_number in 1..=loop_count {
            // "iteration_context" below is normally an expression that yields a List or Range, not a context.
            // This is terminology borrowed from the DMN spec.
            let (param_name, &mut ref mut iteration_context) = loops[loop_number - 1];
            let null_vars = format!("
                // Initialize loop variables for {name} (loop #{num}) to null
                '{name}' null xset
                'loop context {num}' null xset
                'loop position {num}' null xset
                'loop start index {num}' null xset
                'loop stop index {num}' null xset
                'loop count {num}' null xset
                'loop step {num}' null xset
                ", 
                name = param_name.to_string(),
                num = loop_number
            );

            expr.append_str(&null_vars);

            // Add in the iteration context expression, from which we will extract values
            // about how many loop iterations to expect, start and stop positions, and iteration values. 
            expr.append(iteration_context);

            // Note that we need to store values returned by OpCode::LoopBounds (+loop) 
            // in reverse order because of the stack semantics. 
            let set_vars = format!("
                // Set loop variables for {name} (loop #{num}) to initial values.
                +loop
                'loop step {num}' !
                'loop count {num}' !
                'loop stop index {num}' !
                'loop start index {num}' !
                'loop position {num}' !
                'loop context {num}' !
                ", 
                name = param_name.to_string(),
                num = loop_number
            );
            expr.append_str(&set_vars);
        }
        // Drop the context atop the data stack - no longer needed for initialization. 
        // It is not lost as a clone was previously pushed onto contexts.
        expr.append_str(" drop ");

        // Now insert opcodes that perform the looping, 
        // altering 'loop position' and the named loop variable at each level of looping.
        let mut loop_start_ops = String::new();
        let mut loop_end_ops = String::new();
        for loop_number in 1..=loop_count {
            let (param_name, _) = loops[loop_number - 1];
            let a = loop_number * 20;
            // The logic will distinguish between iteration contexts that are Ranges versus Lists. 
            //   - For Ranges, all the information we need is already stored in the loop variables. 
            //   - For Lists, we use the "index" op. 
            // ops_before is all the operations at the top of the loop, before the start of the next loop
            // or the main body that holds innermost.
            let ops_before = format!("
                    // Top of loop {num} over variable {name}
                    // Exit loop if list or range is empty. 
                    'loop count {num}' @ 0 <= branch({exit_loop}/{loop_init}/{loop_init})

                    // Grab iteration variables from context
                    label({loop_init})
                    'loop context {num}' @                  // _ -> ctx
                    'loop stop index {num}' @               // ctx -> ctx stop
                    'loop step {num}' @                     // ctx stop -> ctx stop step
                    'loop position {num}' @                 // ctx stop step -> ctx stop step pos
                    label({loop_top})
                        // Increment the position in the correct direction, according to step.
                        // Store in contexts but also keep on data stack. 
                        over + dup 'loop position {num}' !  // ctx stop step pos -> ctx stop step newpos

                        // Treat range contexts separate from lists
                        3 pick type?(range<Any>) branch({is_range}/{is_list}/{is_list})         // ctx stop step newpos -> ctx stop step newpos
                            label({is_range})
                                // Use loop step to assess if ascending
                                over 0 > branch({is_ascending}/{is_descending}/{is_descending}) // ctx stop step newpos -> ctx stop step newpos
                                    label({is_ascending})
                                        // See if new position is higher than stop and copy newpos to be item.
                                        2 pick over <= over swap // ctx stop step newpos -> ctx stop step newpos item in-range?
                                        branch({set_variable}/{exceeds_range}/{exceeds_range}) // ctx stop step newpos item in-range? -> ctx stop step newpos item
                                    label({is_descending})
                                        // See if new position is lower than stop and copy newpos to be item.
                                        2 pick over >= over swap // ctx stop step newpos -> ctx stop step newpos item in-range?
                                        branch({set_variable}/{exceeds_range}/{exceeds_range}) // ctx stop step newpos item in-range? -> ctx stop step newpos item
                                    label({exceeds_range})
                                        drop drop drop drop drop // ctx stop step newpos item -> _
                                        goto({exit_loop})
                            label({is_list})  // ctx stop step newpos
                                2 pick over >= branch({get_list}/{exceeds_list}/{exceeds_list}) // ctx stop step newpos => ctx stop step newpos
                                    label({get_list})
                                        // Get the item from the list at newpos.
                                        3 pick over index // ctx stop step newpos -> ctx stop step newpos item
                                        goto({set_variable})
                                    label({exceeds_list})
                                        drop drop drop drop // ctx stop step newpos -> _
                                        goto({exit_loop})
                            label({set_variable})
                                // Sets the named loop variable. 
                                '{name}' !  // ctx stop step newpos item -> ctx stop step newpos
                            label({inside})
                            // Next loop inserted here, or the innermost expression.
                ",
                name = param_name.to_string(),
                num = loop_number,
                loop_init = a,
                loop_top = a + 1,
                is_range = a + 2,
                is_ascending = a + 3,
                is_descending = a + 4,
                is_list = a + 5,
                exceeds_range = a + 6,
                get_list = a + 7,
                exceeds_list = a + 8,
                set_variable = a + 9,
                inside = a + 10,
                exit_loop = a + 11
            );
            loop_start_ops.push_str(&ops_before);

            // ops_after is all the operations at the bottom of the loop, after the end of the next inner loop
            // or the main body that holds innermost.
            let mut ops_after = format!("
                    goto({loop_top})
                    label({exit_loop})
                ",
                // Parameters below must equal values used above in ops_before format! statement.
                loop_top = a + 1,  
                exit_loop = a + 11
            );
            // Push the loop ending operations on in reverse order. 
            ops_after.push_str(&loop_end_ops);
            loop_end_ops = ops_after;
        }

        let mut loop_ops = String::new();
        loop_ops.push_str(&loop_start_ops);
        loop_ops.push_str(&format!("
                label({innermost})
                // Assume that the innermost expression creates a value and leaves it on the stack. 
                // Retrieve partial, add that value to the list, and set partial to the new list.
                'partial' @ swap push 'partial' !  // value -> _
            ",
            innermost = innermost_label
        ));
        loop_ops.push_str(&loop_end_ops);
        let mut loop_expr = CompiledExpression::new_from_string(&loop_ops, false);
        loop_expr.insert(innermost, innermost_label);
        expr.append(&mut loop_expr);

        // The last thing to be done is pull out the result and ditch the context.
        expr.append_str(" 
            'partial' @
            xpop drop
        ");

        expr
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
        for (address, op) in self.operations.iter().enumerate() {
            if address % 5 == 0 {
                s.push_str("\n    ");
            }
            s.push_str(&format!(" [{:0>4}] {:20}", address, op.to_string()));
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

    /// Silly, but testing that newline replacement works.
    #[test]
    fn test_newline_removal() {
        let s = "
swap dup drop
label(1)
number(1)
number(2)
+
";
        let expected = "swap dup drop label(1) number(1) number(2) +";
        let actual = s.replace("\n", " ").trim().to_string();
        assert_eq!(expected, actual);
    }

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
