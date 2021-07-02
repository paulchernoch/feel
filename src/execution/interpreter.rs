use super::opcode::OpCode;
use super::compiled_expression::CompiledExpression;
use crate::parsing::nested_context::NestedContext;

/*
  Execution engine that interprets a stream of OpCodes in the presence of a given context and produces a result.

  The interpreter maintains several stacks:

    - instruction stack (OpCodes from the CompiledExpression)
    - heap (literal Strings from the CompiledExpression)
    - contexts (a NestedContext) taht can be pushed an popped
    - data stack (FeelValues that are intermediate valus of calculations)

  Execution of the operations is facilitated by an instruction pointer, an index into the instruction stack.

*/
pub struct Interpreter {
    pub instructions: CompiledExpression,
    /// Zero-based index into the operations list within the instructions of the next operation to be executed. 
    instruction_pointer: usize,

    /// Variables and functions available to be referenced by expressions
    pub contexts: NestedContext,

    /// Intermediate values generated as part of execution of the expression. 
    pub data: Vec<FeelValue>,

    /// No more than this many instructions will be executed. 
    /// This guards against loop contexts whose cartesian products become immense. 
    pub limit: u64,

    step_count: u64
}

impl Interpreter {
    pub fn new(expr: CompiledExpression, ctx: NestedContext) -> Interpreter {
        Interpreter {
            instructions: expr,
            instruction_pointer: 0,
            contexts: ctx,
            data: Vec::new(),
            limit: std::u64::MAX
        }
    }

    /// Zero-based address of the next instruction to be executed, or None if execution has completed. 
    pub fn next_address(&self) -> Option<usize> {
      if self.instruction_pointer >= self.instructions.operations.len() {
          None
      }
      else if self.instructions[self.instruction_pointer] == OpCode::Return {
          None
      }
      else {
          Some(self.instruction_pointer)
      }
    }

    

    /// Execute a single instruction and adjust the instruction_pointer to point to the next instruction.
    /// This takes into account looping and branching. 
    /// Returns true if execution proceeded, false if execution is already complete. 
    /// If the last instruction of the expression was executed, returns true.
    pub fn step(&mut self) -> bool {
        // TODO: This must be implemented for all OpCodes. 
        match self.next_address() {
            Some(address) => {
                match self.instructions[address] {
                    OpCode::LoadBoolean(b) => {
                        self.instruction_pointer += 1;
                        self.push_data(FeelValue::Boolean(b));
                    },
                    _ => { return false; }
                }
                true
            },
            None => false
        }
    }

    /// Remove the top of the data stack and return it, or return Null if empty. 
    fn pop_data(&mut self) -> FeelValue {
        self.data.pop().unwrap_or_default(FeelValue::Null)
    }

    fn push_data(&mut self, value: FeelValue) {
        self.data.push(value)
    }

    fn reset(&mut self) {
        self.data.clear();
        self.instruction_pointer = 0;
        self.step_count = 0;
        // TODO: Do we need to pop any contexts?
    }

    /// Execute the expression and return the result. 
    pub fn execute(&mut self) -> FeelValue {
        self.reset();
        while self.step() {
            self.step_count += 1;
            if self.step_count >= self.limit {
                return FeelValue::Null;
            }
        }
        // When all operations have been executed, assume that the top of the data stack is the answer. 
        // If the stack has more than one value left, it is an error, so return Null. 
        let popped = self.pop_data();
        if self.data.len() != 0 {
            FeelValue::Null
        }
        else {
            popped
        }
    }
}
