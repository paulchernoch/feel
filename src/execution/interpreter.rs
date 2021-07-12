use std::cmp::{Ord, PartialOrd, Ordering};
use std::ops::{Add,Sub,Mul,Div};
use super::opcode::OpCode;
use super::compiled_expression::CompiledExpression;
use crate::parsing::execution_log::ExecutionLog;
use crate::parsing::feel_value::{FeelValue,FeelType};
use crate::parsing::feel_value_ops;
use crate::execution::builtins::Builtins;
use crate::parsing::nested_context::NestedContext;
use crate::parsing::context::Context;

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
            limit: std::u64::MAX,
            step_count: 0
        }
    }

    /// Zero-based address of the next instruction to be executed, or None if execution has completed. 
    pub fn next_address(&self) -> Option<usize> {
      if self.instruction_pointer >= self.instructions.operations.len() {
          None
      }
      else if self.instructions.operations[self.instruction_pointer] == OpCode::Return {
          None
      }
      else {
          Some(self.instruction_pointer)
      }
    }

    fn advance(&mut self) -> usize {
        self.instruction_pointer += 1;
        self.instruction_pointer
    }

    /// Execute a single instruction and adjust the instruction_pointer to point to the next instruction.
    /// This takes into account looping and branching. 
    /// Returns true if execution proceeded, false if execution is already complete. 
    /// If the last instruction of the expression was executed, returns true.
    pub fn step(&mut self) -> bool {
        // TODO: This must be implemented for all OpCodes. 
        match self.next_address() {
            Some(address) => {
                match self.instructions.operations[address] {
                    // TODO: Semantics of FEEL addition, subtraction, etc may be tighter than the operator implementation. Verify and adjust.

                    OpCode::Add => {
                        self.advance();
                        let (lower, higher) = self.pop_two();
                        self.push_data(&lower + &higher);
                    },
                    OpCode::Subtract => {
                        self.advance();
                        let (lower, higher) = self.pop_two();
                        self.push_data(&lower - &higher);
                    },
                    OpCode::Negate => {
                        self.advance();
                        let value = self.pop_data();
                        self.push_data(- value);
                    },
                    OpCode::Multiply => {
                        self.advance();
                        let (lower, higher) = self.pop_two();
                        self.push_data(&lower * &higher);
                    },
                    OpCode::Divide => {
                        self.advance();
                        let (lower, higher) = self.pop_two();
                        self.push_data(&lower / &higher);
                    },
                    OpCode::Exponentiate => {
                        self.advance();
                        let (base, exponent) = self.pop_two();
                        let args = FeelValue::new_list(vec![base, exponent]);
                        let result = Builtins::power(args, &Context::new());
                        self.push_data(result);
                    },
                    OpCode::Not => {
                        self.advance();
                        let result = ! self.pop_data();
                        self.push_data(result);
                    },
                    OpCode::Or => {
                        self.advance();
                        let (lower, higher) = self.pop_two();
                        let result: FeelValue = match (lower.get_type(), higher.get_type()) {
                            (FeelType::Boolean, FeelType::Boolean) => (lower.is_true() || higher.is_true()).into(),
                            _ => {
                                ExecutionLog::log(
                                    &format!("Cannot compare {} with {} using logical or", lower.get_type().to_string(), higher.get_type().to_string())
                                );
                                FeelValue::Null
                            }
                        };
                        self.push_data(result);
                    },
                    OpCode::And => {
                        self.advance();
                        let (lower, higher) = self.pop_two();
                        let result: FeelValue = match (lower.get_type(), higher.get_type()) {
                            (FeelType::Boolean, FeelType::Boolean) => (lower.is_true() && higher.is_true()).into(),
                            _ => {
                                ExecutionLog::log(
                                    &format!("Cannot compare {} with {} using logical and", lower.get_type().to_string(), higher.get_type().to_string())
                                );
                                FeelValue::Null
                            }
                        };
                        self.push_data(result);
                    },
                    OpCode::LessThan => {
                        self.advance();
                        let (lower, higher) = self.pop_two();
                        self.push_data((&lower < &higher).into());
                    },
                    OpCode::LessThanOrEqual => {
                        self.advance();
                        let (lower, higher) = self.pop_two();
                        self.push_data((&lower <= &higher).into());
                    },
                    OpCode::NotEqual => {
                        self.advance();
                        let (lower, higher) = self.pop_two();
                        self.push_data((&lower != &higher).into());
                    },
                    OpCode::Equal => {
                        self.advance();
                        let (lower, higher) = self.pop_two();
                        self.push_data((&lower == &higher).into());
                    },
                    OpCode::GreaterThan => {
                        self.advance();
                        let (lower, higher) = self.pop_two();
                        self.push_data((lower > higher).into());
                    },
                    OpCode::GreaterThanOrEqual => {
                        self.advance();
                        let (lower, higher) = self.pop_two();
                        self.push_data((&lower >= &higher).into());
                    },
                    OpCode::Between => {
                        self.advance();
                        let (value, range_start_inclusive, range_end_inclusive) = self.pop_three();
                        self.push_data((&value >= &range_start_inclusive && &value <= &range_end_inclusive).into());
                    },

                    /*
                    OpCode::In => {},
                    OpCode::Filter => {},
                    OpCode::InstanceOf => {},
                    OpCode::CreateList => {},
                    OpCode::PushList => {},
                    OpCode::LoadFromContext => {},
                    OpCode::AddEntryToContext => {},
                    OpCode::PushContext => {},
                    OpCode::PopContext => {},
                    OpCode::CreateLoopContext(dimensions) => {},
                    OpCode::CreatePredicateContext(dimensions) => {},
                    OpCode::CreateFilterContext => {},
                    OpCode::LoadContext => {},
                    OpCode::CreateRange { lower, upper } => {},
                    OpCode::CreateDate => {},
                    OpCode::CreateTime => {},
                    OpCode::CreateDateTime => {},
                    OpCode::CreateYearsAndMonthsDuration => {},
                    OpCode::CreateDayAndTimeDuration => {},

*/

                    OpCode::LoadString(index) => {
                        self.advance();
                        self.push_data(self.instructions.get_from_heap(index));
                    },

/*
                    OpCode::CreateName => {},

*/
                    OpCode::LoadNumber(wrapped_num) => {
                      self.advance();
                      self.push_data(FeelValue::Number(wrapped_num.into_inner()));
                    },
                    OpCode::LoadBoolean(b) => {
                      self.advance();
                      self.push_data(FeelValue::Boolean(b));
                    },
                    OpCode::LoadNull => {
                      self.advance();
                      self.push_data(FeelValue::Null);
                    },
/*
                    OpCode::CallFunction => {},
                    OpCode::GetProperty => {},
                    OpCode::GotoLabel(label) => {},
                    OpCode::GotoAddress(address) => {},
                    OpCode::BranchToLabel { true_label, false_label, null_label } => {},
                    OpCode::BranchToAddress { true_address, false_address, null_address } => {},
                    OpCode::Label(position) => {},
                    OpCode::ExitLoopLabel(label) => {},
                    OpCode::ExitLoopAddress(address) => {},
                    OpCode::BranchExitLabel { true_label, false_label, null_label } => {},
                    OpCode::BranchExitAddress { true_address, false_address, null_address } => {},
                    OpCode::HasNext => {},
                    OpCode::PushNext => {},
                    OpCode::Return  => {},
*/
                    _ => { return false; }
                }
                true
            },
            None => false
        }
    }

    /// Remove the top of the data stack and return it, or return Null if empty. 
    fn pop_data(&mut self) -> FeelValue {
        self.data.pop().unwrap_or(FeelValue::Null)
    }

    /// Remove the top two values of the data stack and return as a tuple, replacing missing values with null. 
    /// The first value in the tuple is the one lower on the stack and teh second value is the one higher. 
    /// Thus if the stack is this:
    ///      <bottom> 5 2 <top>
    /// the returned tuple is (5 2). 
    fn pop_two(&mut self) -> (FeelValue, FeelValue) {
        let higher = self.pop_data();
        let lower = self.pop_data();
        (lower, higher)
    }

    fn pop_three(&mut self) -> (FeelValue, FeelValue, FeelValue) {
        let higher = self.pop_data();
        let middle = self.pop_data();
        let lower = self.pop_data();
        (lower, middle, higher)
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

#[cfg(test)]
mod tests {

  use crate::parsing::feel_value::{FeelValue};
  use super::super::opcode::OpCode;
  use super::super::compiled_expression::CompiledExpression;
  use super::Interpreter;
  use crate::parsing::nested_context::NestedContext;

  #[test]
  fn test_addition() {
    let mut i = make_interpreter(vec![
        OpCode::load_number(10_f64),
        OpCode::load_number(20_f64),
        OpCode::Add
      ], 
      Vec::new()
    );
    let actual = i.execute();
    let expected: FeelValue = 30.into();
    assert_eq!(actual, expected);
  }

  fn make_interpreter(ops: Vec<OpCode>, heap: Vec<String>) -> Interpreter {
    let ctx = NestedContext::new();
    let mut expr = CompiledExpression::new("test");
    for s in heap {
        expr.find_or_add_to_heap(s);
    }
    for op in ops {
        expr.push(op);
    }
    Interpreter::new(expr, ctx)
  }

}
