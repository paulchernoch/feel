// use std::cmp::{Ord, PartialOrd, Ordering};
use super::opcode::{OpCode,RangeBoundType};
use super::compiled_expression::CompiledExpression;
use crate::parsing::execution_log::ExecutionLog;
use crate::parsing::feel_value::{FeelValue,FeelType};
// use crate::parsing::feel_value_ops;
use crate::execution::builtins::Builtins;
use crate::parsing::nested_context::NestedContext;
use crate::parsing::range::Range;
use crate::parsing::qname::QName;

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

    fn jump(&mut self, target_address: usize) -> usize {
        self.instruction_pointer = target_address;
        self.instruction_pointer
    }

    /// Create a FeelValue::List from values popped from the data stack such taht it can be used 
    /// as arguments for a call to a Builtin function.
    /// Arguments are reversed in order as they are popped fron the stack. 
    /// E.g., Whatever was top of the data stack becomes the last argument. 
    fn make_args(&mut self, arity: usize) -> FeelValue {
        match arity {
            1 => FeelValue::new_list(vec![self.pop_data()]),
            2 => {
                let (lower, higher) = self.pop_two();
                FeelValue::new_list(vec![lower, higher])
            },
            3 => {
                let (lower, middle, higher) = self.pop_three();
                FeelValue::new_list(vec![lower, middle, higher])
            },
            _ => FeelValue::Null
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
                        let result = Builtins::power(self.make_args(2), &self.contexts);
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

                    OpCode::In => {
                        self.advance();
                        let result = Builtins::in_operator(self.make_args(2), &self.contexts);
                        self.push_data(result);
                    },
                    /*
                    OpCode::Filter => {},
                    OpCode::InstanceOf => {},
                    */
                    OpCode::CreateList => {
                        self.advance();
                        self.push_data(FeelValue::new_list(Vec::new()));
                    },
                    OpCode::PushList => {
                        self.advance();
                        let (list, item) = self.pop_two();
                        match list.clone() {
                            FeelValue::List(rr_list) => {
                                rr_list.borrow_mut().push(item);
                                self.push_data(list);
                            },
                            _ => {
                                ExecutionLog::log(
                                    &format!("Cannot push value onto {}", list.get_type().to_string())
                                );
                                self.push_data(FeelValue::Null);
                            }
                        };
                    },
                    /*
                    OpCode::LoadFromContext => {},
                    OpCode::AddEntryToContext => {},
                    OpCode::PushContext => {},
                    OpCode::PopContext => {},
                    OpCode::CreateLoopContext(dimensions) => {},
                    OpCode::CreatePredicateContext(dimensions) => {},
                    OpCode::CreateFilterContext => {},
                    OpCode::LoadContext => {},
                    */

                    OpCode::CreateRange { lower, upper } => {
                        self.advance();
                        self.create_range(lower, upper);
                    },

                    OpCode::CreateDate => {
                        self.advance();
                        let args = self.make_args(1);
                        self.push_data(Builtins::date(args, &self.contexts));
                    },
                    OpCode::CreateTime => {
                        self.advance();
                        let args = self.make_args(1);
                        // NOTE: Though the "time" builtin can take one argument or three to four,
                        //       this opcode will only expect a single argument. 
                        //       To build a time from parts, you must call it using the CallFunction opcode. 
                        self.push_data(Builtins::time(args, &self.contexts));
                    },
                    OpCode::CreateDateTime => {
                        self.advance();
                        let args = self.make_args(1);
                        self.push_data(Builtins::date_and_time(args, &self.contexts));
                    },
/*
                    OpCode::CreateYearsAndMonthsDuration => {},
                    OpCode::CreateDayAndTimeDuration => {},

*/

                    OpCode::LoadString(index) => {
                        self.advance();
                        self.push_data(self.instructions.get_from_heap(index));
                    },
                    OpCode::CreateName => {
                        self.advance();
                        let name_string = self.pop_data();
                        match name_string {
                            FeelValue::String(s) => {
                                self.push_data(FeelValue::Name(QName::new(&s)));
                            },
                            FeelValue::Name(_) => {
                                self.push_data(name_string);
                            },
                            _ => {
                                ExecutionLog::log(
                                    &format!("Cannot create a Qualified name from a {}", name_string.get_type().to_string())
                                );
                                self.push_data(FeelValue::Null);
                            }
                        }
                    },
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
*/
                    // All GotoLabel's should have been replaced with GotoAddress via a call to resolve_jumps.
                    OpCode::GotoLabel(_label) => unreachable!(),

                    OpCode::GotoAddress(address) => {
                        self.jump(address);
                    },

                    // All BranchToLabel's should have been replaced with BranchToAddress via a call to resolve_jumps.
                    OpCode::BranchToLabel { .. } => unreachable!(),

                    OpCode::BranchToAddress { true_address, false_address, null_address } => {
                        let condition = self.pop_data();
                        match condition {
                            FeelValue::Boolean(true) => { self.jump(true_address); }
                            FeelValue::Boolean(false) => { self.jump(false_address); }
                            _ => {
                                ExecutionLog::log(
                                    &format!("Taking null branch because condition is not Boolean, but {}", condition.get_type().to_string())
                                );
                                self.jump(null_address); 
                                // The compiler should make sure that the null branch jump goes to a label followed by a CreateNull. 
                            }
                        }
                    },

                    OpCode::Label(_position) => {
                        self.advance();
                    },
/*
                    OpCode::ExitLoopLabel(label) => {},
                    OpCode::ExitLoopAddress(address) => {},
                    OpCode::BranchExitLabel { true_label, false_label, null_label } => {},
                    OpCode::BranchExitAddress { true_address, false_address, null_address } => {},
                    OpCode::HasNext => {},
                    OpCode::PushNext => {},
*/
                    OpCode::Return  => (),
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

    /// Pop one or two items from the data stack, construct a Range with the appropriate lower and upper bounds, 
    /// and push it onto the data stack. 
    /// Returns true on success, false on failure. 
    /// If unable to create a Range, push a Null instead.
    fn create_range(&mut self, lower: RangeBoundType, upper: RangeBoundType) -> bool {
        let r:Range = match (lower, upper) {
            (RangeBoundType::Exclusive, RangeBoundType::Exclusive) => {
                let (lower_bound, upper_bound) = self.pop_two();
                Range::new(&lower_bound, &upper_bound, false, false)
            },
            (RangeBoundType::Exclusive, RangeBoundType::Inclusive) => {
                let (lower_bound, upper_bound) = self.pop_two();
                Range::new(&lower_bound, &upper_bound, false, true)
            },
            (RangeBoundType::Inclusive, RangeBoundType::Exclusive) => {
                let (lower_bound, upper_bound) = self.pop_two();
                Range::new(&lower_bound, &upper_bound, true, false)
            },
            (RangeBoundType::Inclusive, RangeBoundType::Inclusive) => {
                let (lower_bound, upper_bound) = self.pop_two();
                Range::new(&lower_bound, &upper_bound, true, true)
            },
            (RangeBoundType::Open, RangeBoundType::Exclusive) => {
                let upper_bound = self.pop_data();
                Range::new_with_high(&upper_bound, false)
            },
            (RangeBoundType::Open, RangeBoundType::Inclusive) => {
                let upper_bound = self.pop_data();
                Range::new_with_high(&upper_bound, true)
            },
            (RangeBoundType::Inclusive, RangeBoundType::Open) => {
                let lower_bound = self.pop_data();
                Range::new_with_low(&lower_bound, true)
            },
            (RangeBoundType::Exclusive, RangeBoundType::Open) => {
                let lower_bound = self.pop_data();
                Range::new_with_low(&lower_bound, false)
            },
            (RangeBoundType::Open, RangeBoundType::Open) => {
                ExecutionLog::log(&format!("Unrestricted Ranges are not supported"));
                self.push_data(FeelValue::Null);
                return false;
            }
        };
        self.push_data(FeelValue::Range(r));
        true
    }
}

#[cfg(test)]
mod tests {
  #![allow(non_snake_case)]
  use chrono::{ NaiveDate };
  use crate::parsing::feel_value::{FeelValue};
  use super::super::opcode::OpCode;
  use super::super::compiled_expression::CompiledExpression;
  use super::Interpreter;
  use crate::parsing::nested_context::NestedContext;
  use crate::parsing::qname::QName;
  use std::str::FromStr;

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

  #[test]
  fn test_arithmetic() {
    let actual = parse_and_execute(
      vec!["num(1)", "num(2)", "*", "num(3)", "*", "num(4)", "+", "num(0.1)", "/", "num(58)", "-"], 
      Vec::new()
    );
    let expected: FeelValue = 42.into();
    assert_eq!(actual, expected);
  }

  #[test]
  fn test_inequalities() {
    let FALSE: FeelValue = false.into();
    let TRUE: FeelValue = true.into();
    assert_eq!(FALSE, parse_and_execute(vec!["num(1)", "num(2)", ">"], Vec::new()));
    assert_eq!(TRUE, parse_and_execute(vec!["num(1)", "num(2)", "<"], Vec::new()));
    assert_eq!(TRUE, parse_and_execute(vec!["num(1)", "num(1)", ">="], Vec::new()));
    assert_eq!(TRUE, parse_and_execute(vec!["num(1)", "num(2)", "<="], Vec::new()));
    assert_eq!(TRUE, parse_and_execute(vec!["num(2)", "num(2)", "="], Vec::new()));
    assert_eq!(FALSE, parse_and_execute(vec!["num(1)", "num(2)", "="], Vec::new()));
    assert_eq!(TRUE, parse_and_execute(vec!["num(1)", "num(2)", "!="], Vec::new()));
    assert_eq!(FALSE, parse_and_execute(vec!["num(2)", "num(2)", "!="], Vec::new()));
  }

  #[test]
  fn test_logical_and() {
    let FALSE: FeelValue = false.into();
    let TRUE: FeelValue = true.into();
    assert_eq!(TRUE, parse_and_execute(vec!["true", "true", "and"], Vec::new()));
    assert_eq!(FALSE, parse_and_execute(vec!["true", "false", "and"], Vec::new()));
    assert_eq!(FALSE, parse_and_execute(vec!["false", "true", "and"], Vec::new()));
    assert_eq!(FALSE, parse_and_execute(vec!["false", "false", "and"], Vec::new()));
    assert_eq!(FeelValue::Null, parse_and_execute(vec!["num(2)", "false", "and"], Vec::new()));
  }

  #[test]
  fn test_logical_or() {
    let FALSE: FeelValue = false.into();
    let TRUE: FeelValue = true.into();
    assert_eq!(TRUE, parse_and_execute(vec!["true", "true", "or"], Vec::new()));
    assert_eq!(TRUE, parse_and_execute(vec!["true", "false", "or"], Vec::new()));
    assert_eq!(TRUE, parse_and_execute(vec!["false", "true", "or"], Vec::new()));
    assert_eq!(FALSE, parse_and_execute(vec!["false", "false", "or"], Vec::new()));
    assert_eq!(FeelValue::Null, parse_and_execute(vec!["num(2)", "false", "or"], Vec::new()));
  }

  #[test]
  fn test_exponentiation() {
    assert_eq!(FeelValue::Number(8.0), parse_and_execute(vec!["num(2)", "num(3)", "^"], Vec::new()));
  }

  #[test]
  fn test_between() {
    let FALSE: FeelValue = false.into();
    let TRUE: FeelValue = true.into();
    assert_eq!(TRUE, parse_and_execute(vec!["num(2)", "num(1)", "num(3)", "between"], Vec::new()));
    assert_eq!(TRUE, parse_and_execute(vec!["num(1)", "num(1)", "num(3)", "between"], Vec::new()));
    assert_eq!(FALSE, parse_and_execute(vec!["num(0)", "num(1)", "num(3)", "between"], Vec::new()));
    assert_eq!(FALSE, parse_and_execute(vec!["num(4)", "num(1)", "num(3)", "between"], Vec::new()));
  }

  #[test]
  fn test_load_string() {
    let FALSE: FeelValue = false.into();
    let TRUE: FeelValue = true.into();
    let duck: FeelValue = "Duck".into();
    let goose: FeelValue = "Goose".into();
    let heap: Vec<String> = vec!["Duck".to_string(), "Goose".to_string()];
    assert_eq!(duck, parse_and_execute(vec!["string(0)"], heap.clone()));
    assert_eq!(goose, parse_and_execute(vec!["string(1)"], heap.clone()));
    assert_eq!(TRUE, parse_and_execute(vec!["string(0)", "string(0)", "="], heap.clone()));
    assert_eq!(FALSE, parse_and_execute(vec!["string(0)", "string(1)", "="], heap.clone()));
  }

  #[test]
  fn test_create_qname() {
    let ddg = "duck duck goose";
    let heap: Vec<String> = vec![ddg.to_string()];
    let expected = FeelValue::Name(QName::new(&ddg));
    assert_eq!(expected, parse_and_execute(vec!["string(0)", "name"], heap));
  }

  #[test]
  fn test_list_push() {
    let duck: FeelValue = "Duck".into();
    let goose: FeelValue = "Goose".into();
    let heap: Vec<String> = vec!["Duck".to_string(), "Goose".to_string()];
    let expected = FeelValue::new_list(vec![duck, goose]);
    assert_eq!(expected, parse_and_execute(vec!["list", "string(0)", "push", "string(1)", "push"], heap.clone()));
  }

  #[test]
  fn test_in_list() {
    let heap: Vec<String> = vec!["Duck".to_string(), "Goose".to_string()];
    assert!(parse_and_execute(vec!["string(1)", "list", "string(0)", "push", "string(1)", "push", "in"], heap).is_true());
  }

  #[test]
  fn test_in_range() {
    let FALSE: FeelValue = false.into();
    let TRUE: FeelValue = true.into();
    assert_eq!(TRUE, parse_and_execute(vec!["num(10)", "num(1)", "num(10)", "[lo,hi]", "in"], Vec::new()));
    assert_eq!(FALSE, parse_and_execute(vec!["num(10)", "num(1)", "num(10)", "[lo,hi)", "in"], Vec::new()));
    assert_eq!(TRUE, parse_and_execute(vec!["num(5)", "num(3)", "[lo,..]", "in"], Vec::new()));
  }

  #[test]
  fn test_create_date() {
    let heap: Vec<String> = vec!["2020-02-15".to_string()];
    let expected = FeelValue::Date(NaiveDate::from_ymd(2020, 2, 15));
    assert_eq!(expected, parse_and_execute(vec!["string(0)", "date"], heap));
  }

  #[test]
  fn test_create_time() {
    let heap: Vec<String> = vec!["12:34:56z".to_string()];
    let expected = FeelValue::new_time("12:34:56z").unwrap();
    assert_eq!(expected, parse_and_execute(vec!["string(0)", "time"], heap));
  }

  #[test]
  fn test_create_date_and_time() {
    let dt = "2021-05-04T12:34:56z";
    let heap: Vec<String> = vec![dt.to_string()];
    let expected = FeelValue::new_date_and_time(dt).unwrap();
    assert_eq!(expected, parse_and_execute(vec!["string(0)", "dt"], heap));
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

  fn make_interpreter_from_strings(ops: Vec<String>, heap: Vec<String>) -> Interpreter {
    let ctx = NestedContext::new();
    let mut expr = CompiledExpression::new("test");
    for s in heap {
        expr.find_or_add_to_heap(s);
    }
    for op_string in ops {
        let op = OpCode::from_str(&op_string).unwrap();
        expr.push(op);
    }
    expr.resolve_jumps();
    Interpreter::new(expr, ctx)
  }

  fn parse_and_execute(ops: Vec<&str>, heap: Vec<String>) -> FeelValue {
    let op_strings: Vec<String> = ops
      .iter()
      .map(|s| s.to_string())
      .collect();
    let mut interpreter = make_interpreter_from_strings(op_strings, heap);
    interpreter.execute()
  }

}
