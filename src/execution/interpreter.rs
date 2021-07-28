// use std::cmp::{Ord, PartialOrd, Ordering};
use super::opcode::{OpCode,RangeBoundType};
use super::compiled_expression::CompiledExpression;
use crate::parsing::feel_value::{FeelValue,FeelType};
// use crate::parsing::feel_value_ops;
use crate::execution::builtins::Builtins;
use crate::parsing::{execution_log::ExecutionLog,nested_context::NestedContext,range::Range,qname::QName,duration::Duration};
use crate::parsing::{context::ContextReader,context::ContextIncrement};
use crate::execution::value_properties::ValueProperties;

/*
  Execution engine that interprets a stream of OpCodes in the presence of a given context and produces a result.

  The interpreter maintains several stacks:

    - instruction stack (OpCodes from the CompiledExpression)
    - heap (literal Strings from the CompiledExpression)
    - contexts (a NestedContext) that can be pushed an popped
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

    /// Counts how many operations have been executed so far. 
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

    /// Advance the instruction pointer by a single position.
    fn advance(&mut self) -> usize {
        self.instruction_pointer += 1;
        self.instruction_pointer
    }

    /// Jump the instruction pointer to the given position.
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

    /// Log an error and optionally push a Null onto the data stack. 
    fn error(&mut self, error_message: String, do_push: bool) -> FeelValue {
        ExecutionLog::log(&error_message);
        if do_push {
            self.push_data(FeelValue::Null);
        }
        FeelValue::Null
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
                    OpCode::Swap => {
                        self.advance();
                        let (lower, higher) = self.pop_two();
                        self.push_data(higher);
                        self.push_data(lower);
                    },
                    OpCode::Over => {
                        self.advance();
                        let (lower, higher) = self.pop_two();
                        self.push_data(lower.clone());
                        self.push_data(higher);
                        self.push_data(lower);
                    },
                    OpCode::Rot => {
                        // (a b c => b c a)
                        self.advance();
                        let (b, c) = self.pop_two();
                        let a = self.pop_data();
                        self.push_data(b);
                        self.push_data(c);
                        self.push_data(a);
                    },
                    OpCode::Dup => {
                        self.advance();
                        let top = self.pop_data();
                        self.push_data(top.clone());
                        self.push_data(top);
                    },                    
                    OpCode::Drop => {
                        self.advance();
                        self.pop_data();
                    },

                    // TODO: Semantics of FEEL addition, subtraction, etc may be tighter than the operator implementation. Verify and adjust.
                    //  - Inequalities and between operator have been adapted to deal with Null properly.

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
                                self.error(
                                    format!("Cannot compare {} with {} using logical or", lower.get_type().to_string(), higher.get_type().to_string()),
                                    false
                                )
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
                                self.error(
                                    format!("Cannot compare {} with {} using logical and", lower.get_type().to_string(), higher.get_type().to_string()),
                                    false
                                )
                            }
                        };
                        self.push_data(result);
                    },
                    OpCode::LessThan => {
                        self.advance();
                        let (lower, higher) = self.pop_two();
                        if lower.is_null() || higher.is_null() { self.push_data(FeelValue::Null); }
                        else { self.push_data((&lower < &higher).into()); }
                    },
                    OpCode::LessThanOrEqual => {
                        self.advance();
                        let (lower, higher) = self.pop_two();
                        if lower.is_null() || higher.is_null() { self.push_data(FeelValue::Null); }
                        else { self.push_data((&lower <= &higher).into()); }
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
                        if lower.is_null() || higher.is_null() { self.push_data(FeelValue::Null); }
                        else { self.push_data((&lower > &higher).into()); }
                    },
                    OpCode::GreaterThanOrEqual => {
                        self.advance();
                        let (lower, higher) = self.pop_two();
                        if lower.is_null() || higher.is_null() { self.push_data(FeelValue::Null); }
                        else { self.push_data((&lower >= &higher).into()); }
                    },
                    OpCode::Between => {
                        self.advance();
                        let (value, range_start_inclusive, range_end_inclusive) = self.pop_three();
                        if value.is_null() || range_start_inclusive.is_null() || range_end_inclusive.is_null() { self.push_data(FeelValue::Null); }
                        else { self.push_data((&value >= &range_start_inclusive && &value <= &range_end_inclusive).into()); }
                    },

                    OpCode::In => {
                        self.advance();
                        let result = Builtins::in_operator(self.make_args(2), &self.contexts);
                        self.push_data(result);
                    },
/*
                    OpCode::Filter => {},
*/
                    OpCode::InstanceOf => {
                        self.advance();
                        let result = Builtins::instance_of(self.make_args(2), &self.contexts);
                        self.push_data(result);
                    },
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
                                self.error(
                                    format!("Cannot push value onto {}", list.get_type().to_string()),
                                    true
                                );
                            }
                        };
                    },

                    OpCode::Index => {
                        // NOTE: Feel semantics is to treat a scalar first operand as though it were 
                        //       a list of one element. That will be handled not here but by wrapping
                        //       OpCodes with conditional logic around the inputs to test their types.
                        self.advance();
                        let (list, index) = self.pop_two();
                        let list_type = list.get_type().to_string();
                        match (list, index.clone()) {
                            (FeelValue::List(rr_list), FeelValue::Number(i)) if index.is_integer() && i >= 0.0 => {
                                let u_index = i as usize;
                                let size = rr_list.borrow().len();
                                if size > u_index {
                                    self.push_data(rr_list.borrow()[u_index].clone());
                                }
                                else {
                                    self.error(
                                        format!("List index out of range: {}", u_index),
                                        true
                                    );
                                }
                            },
                            _ => {
                                self.error(
                                    format!("Cannot index {} using given index", list_type),
                                    true
                                );
                            }

                        }
                    },

                    OpCode::LoadFromContext => {
                        self.advance();
                        let key = self.pop_data();
                        match key.clone() {
                            FeelValue::Name(qname) => {
                                match self.contexts.get(qname) {
                                    Some(value) => {
                                        self.push_data(value);
                                    },
                                    None => {
                                        self.error(
                                            format!("key '{}' not present in context", key.to_string()),
                                            true
                                        );   
                                    }
                                }
                            },
                            FeelValue::String(str_name) => {
                                let qname = QName::new(&str_name);
                                match self.contexts.get(qname) {
                                    Some(value) => {
                                        self.push_data(value);
                                    },
                                    None => {
                                        self.error(
                                            format!("key '{}' not present in context", key.to_string()),
                                            true
                                        );   
                                    }
                                }
                            },
                            _ => {
                                self.error(
                                    format!("Cannot load key from context because its type is {}", key.get_type().to_string()),
                                    true
                                );   
                            }
                        }
                    },

                    OpCode::AddEntryToContext => {
                        // Leaves the context on the data stack when finished.
                        self.advance();
                        let (ctx, key, value) = self.pop_three();
                        match (ctx.clone(), key.clone()) {
                            (FeelValue::Context(rc_ctx), FeelValue::Name(qname)) => {
                                (*rc_ctx).insert(qname, value);
                                self.push_data(ctx);
                            },
                            (FeelValue::Context(rc_ctx), FeelValue::String(name)) => {
                                (*rc_ctx).insert(name, value);
                                self.push_data(ctx);
                            },
                            _ => {
                                // TODO: Should we also push the context back on?
                                self.error(
                                    format!("Cannot add key to context because its type is {}", key.get_type().to_string()),
                                    true
                                ); 
                            }                            
                        }
                    },

                    OpCode::PushContext => {
                        self.advance();
                        // TODO: Review error handling.
                        // For now, if top of value stack is not a Context, 
                        // make an empty Context to push onto the contexts stack.
                        let ctx = self.pop_data();
                        match ctx {
                            FeelValue::Context(_) => { self.contexts.push(ctx); },
                            _ => { 
                                self.contexts.push(FeelValue::new_context()); 
                                self.error(
                                    format!("Top of value stack is a {} instead of a context", ctx.get_type().to_string()),
                                    false
                                ); 
                            }
                        };
                    },
                    OpCode::PopContext => {
                        self.advance();
                        match self.contexts.pop() {
                            Some(ctx) => {
                                self.push_data(ctx);
                            },
                            None => {
                                self.push_data(FeelValue::new_context());
                                self.error(
                                    format!("Contexts stack is empty"),
                                    false
                                ); 
                            }
                        };
                    },

                    OpCode::UpdateContext => {
                        self.advance();
                        let (key, value) = self.pop_two();
                        match key {
                            FeelValue::Name(qname) => {
                                self.contexts.update(qname, value);
                            },
                            FeelValue::String(sname) => {
                                self.contexts.update(sname, value);
                            },
                            _ => {
                                self.error(
                                    format!("Key for context update may not be {}", key.get_type().to_string()),
                                    false
                                ); 
                            }
                        }
                    },

                    OpCode::Increment => {
                        self.advance();
                        let key = self.pop_data();
                        match key {
                            FeelValue::Name(qname) => {
                                self.contexts.increment(qname, None);
                            },
                            FeelValue::String(sname) => {
                                self.contexts.increment(sname, None);
                            },
                            _ => {
                                self.error(
                                    format!("Key for increment may not be {}", key.get_type().to_string()),
                                    false
                                ); 
                            }
                        }
                    },
/*
                    OpCode::CreateLoopContext(dimensions) => {},
                    OpCode::CreatePredicateContext(dimensions) => {},
*/                    
                    OpCode::LoadContext => {
                        self.advance();
                        self.push_data(FeelValue::new_context());
                    },

                    OpCode::IsType(index) => {
                        self.advance();
                        if index >= self.instructions.heap.len() {
                            self.error(
                                format!("String heap index {} is out of range.", index),
                                true
                            ); 
                        }
                        else {
                            let type_name = FeelValue::String(self.instructions.heap[index].clone());
                            self.push_data(type_name);
                            let result = Builtins::instance_of(self.make_args(2), &self.contexts);
                            self.push_data(result);
                        }
                    },

                    OpCode::ListLength => {
                        self.advance();
                        match self.pop_data() {
                            FeelValue::List(rr_list) => {
                                let length = rr_list.borrow().len() as f64;
                                self.push_data(length.into());
                            },
                            _ => {
                                self.push_data(1.0.into());
                            }
                        };
                    },

                    OpCode::CreateRange { lower, upper } => {
                        self.advance();
                        self.create_range(lower, upper);
                        // push_data called by create_range above
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
                    OpCode::CreateYearsAndMonthsDuration => {
                        self.advance();
                        let value = self.pop_data();
                        match value {
                            FeelValue::String(duration_string) => {
                                match Duration::try_year_month(&duration_string) {
                                    Ok(duration) => {
                                        self.push_data(FeelValue::YearMonthDuration(duration));
                                    },
                                    Err(_) => {
                                        self.error(
                                            format!("Cannot create Year Month Duration from {}", duration_string),
                                            true
                                        );
                                    }
                                };
                            },
                            _ => {
                                self.error(
                                    format!("Cannot create Year Month Duration from a {}", value.get_type().to_string()),
                                    true
                                );
                            }
                        };
                    },

                    OpCode::CreateDayAndTimeDuration => {
                        self.advance();
                        let value = self.pop_data();
                        match value {
                            FeelValue::String(duration_string) => {
                                match Duration::try_day_time(&duration_string) {
                                    Ok(duration) => {
                                        self.push_data(FeelValue::DayTimeDuration(duration));
                                    },
                                    Err(_) => {
                                        self.error(
                                            format!("Cannot create Day Time Duration from {}", duration_string),
                                            true
                                        );
                                    }
                                };
                            },
                            _ => {
                                self.error(
                                    format!("Cannot create Day Time Duration from a {}", value.get_type().to_string()),
                                    true
                                );
                            }
                        };
                    },

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
                                self.error(
                                    format!("Cannot create a Qualified name from a {}", name_string.get_type().to_string()),
                                    true
                                );
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
*/
                    OpCode::GetProperty => {
                        self.advance();
                        let (target, property) = self.pop_two();
                        let value = target.get_property(&property, &self.contexts);
                        self.push_data(value);
                    },

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

    fn next_operation(&self) -> Option<OpCode> {
        match self.next_address() {
            Some(address) => Some(self.instructions.operations[address]),
            None => None
        }
    }

    /// Execute the expression and return the result 
    /// while logging changes to the data stack for diagnostic purposes. 
    /// Returns a tuple with the computed result and a String message that lists each OpCode executed
    /// and the changes to the data stack.
    pub fn trace(&mut self) -> (FeelValue, String) {
        let mut message = String::new();
        self.reset();
        let mut next_op = self.next_operation();
        while self.step() {
            let stack: Vec<String> = self.data.iter().rev().map(|val| val.to_string()).collect();
            let stack_string = if stack.len() <= 1 {
                format!("[ {}]\n", stack.join(" "))
            } 
            else {
                format!("[\n  TOP-> {}\n]\n", stack.join("\n        "))
            };
                
            message.push_str(&format!("{}. {} {}", self.step_count, next_op.unwrap(), stack_string));
            self.step_count += 1;
            if self.step_count >= self.limit {
                return (FeelValue::Null, message);
            }
            next_op = self.next_operation();
        }
        // When all operations have been executed, assume that the top of the data stack is the answer. 
        // If the stack has more than one value left, it is an error, so return Null. 
        let popped = self.pop_data();
        message.push_str(&format!("\nExecution Log:\n  {}", ExecutionLog::get().join("\n  ")));
        if self.data.len() != 0 {
            (FeelValue::Null, message)
        }
        else {
            (popped, message)
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
  use std::rc::Rc;
  use chrono::{ NaiveDate };
  use crate::parsing::feel_value::{FeelValue};
  use super::super::opcode::OpCode;
  use super::super::compiled_expression::CompiledExpression;
  use super::Interpreter;
  use crate::parsing::{nested_context::NestedContext,context::Context,qname::QName,duration::Duration};
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

  /// Test uses a goto to jump past the plus and minus operators to perform a multiply, then hits a return that causes the final zero to not be pushed. 
  #[test]
  fn test_goto_address() {
    assert_eq!(
        FeelValue::Number(42.0), 
        exec_string("num(6) num(7) goto(1) + - label(1) * return num(0)", Vec::new())
    );
  }

  /// Simulate an "if" statement, with branch jumps for true, false and null. 
  #[test]
  fn test_branch_address() {
    let good = "Good".to_string();
    let bad = "Bad".to_string();
    let ugly = "Ugly".to_string();
    let heap: Vec<String> = vec![good.clone(), bad.clone(), ugly.clone()];
    assert_eq!(
        FeelValue::String(good.clone()), 
        exec_string("num(87) num(80) >= branch(0/1/2) label(0) string(0) goto(3) label(1) string(1) goto(3) label(2) null label(3) return", heap.clone())
    );
    assert_eq!(
        FeelValue::String(bad.clone()), 
        exec_string("num(77) num(80) >= branch(0/1/2) label(0) string(0) goto(3) label(1) string(1) goto(3) label(2) null label(3) return", heap.clone())
    );
    assert_eq!(
        FeelValue::Null, 
        exec_string("num(77) null >= branch(0/1/2) label(0) string(0) goto(3) label(1) string(1) goto(3) label(2) null label(3) return", heap.clone())
    );
  }

  #[test]
  fn test_instance_of() {
    let heap: Vec<String> = vec!["number".to_string(), "boolean".to_string(), "Any".to_string()];
    assert_eq!(
        FeelValue::Boolean(true), 
        exec_string("num(6) string(0) is true string(1) is and false string(2) is and", heap.clone())
    );
  }

  #[test]
  fn test_ym_duration() {
    let heap: Vec<String> = vec!["P2Y8M".to_string()];
    assert_eq!(
        FeelValue::YearMonthDuration(Duration::new_year_month(true, 2, 8)), 
        exec_string("string(0) ym-duration", heap.clone())
    );
  }

  #[test]
  fn test_dt_duration() {
    let heap: Vec<String> = vec!["P1DT2H".to_string()];
    assert_eq!(
        FeelValue::DayTimeDuration(Duration::new_day_time(true, 1, 2, 0, 0.0)), 
        exec_string("string(0) dt-duration", heap.clone())
    );
  }

  #[test]
  fn test_get_property() {
    let heap: Vec<String> = vec!["2021-03-22".to_string(), "year".to_string()];
    assert_eq!(
        FeelValue::Number(2021.0), 
        exec_string("string(0) date string(1) .", heap.clone())
    );
  }

  #[test]
  fn test_load_from_context() {
    let heap: Vec<String> = vec!["the answer".to_string(), "missing key".to_string()];
    assert_eq!(
        FeelValue::Number(42.0), 
        exec_string("string(0) name xget", heap.clone())
    );
    assert_eq!(
        FeelValue::Null, 
        exec_string("string(1) name xget", heap.clone())
    );
  }

  // ///////////////////////////////////// //
  //                                       //
  //          Test Helper methods          //
  //                                       //
  // ///////////////////////////////////// //

  /// Ensure there is a contaxt variable named "the answer".
  fn setup_test_context(nest: &mut NestedContext) {
    let ctx = Context::new();
    ctx.insert("the answer", FeelValue::Number(42.0));
    nest.push(FeelValue::Context(Rc::new(ctx)));
  }

  fn make_interpreter(ops: Vec<OpCode>, heap: Vec<String>) -> Interpreter {
    let mut ctx = NestedContext::new();
    setup_test_context(&mut ctx);
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
    let mut ctx = NestedContext::new();
    setup_test_context(&mut ctx);
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

  fn exec_string(op_string: &str, heap: Vec<String>) -> FeelValue {
      let ops = split_into_ops(op_string);
      parse_and_execute(ops, heap)
  }

  fn split_into_ops(op_string: &str) -> Vec<&str> {
    let split = op_string.split(" ");
    let vec: Vec<&str> = split.collect();
    vec
  }

    #[test]
    fn test_type_of_context() {
        let mut expr = CompiledExpression::new_from_string("xload type?(context<>)", false);
        expr.resolve_jumps();
        let ctx = NestedContext::new();
        // println!("Expression\n{}", expr);
        let mut interpreter = Interpreter::new(expr, ctx);
        let (actual, message) = interpreter.trace();
        // println!("{}", message);
        assert!(actual.is_true());
    }

    /// Test an OpCode stream that creates a list, makes a filter context,
    /// then tests if there is are any more items to iterate over in the list.
    /// Try for an empty list which should return False.
    #[test]
    fn test_has_next_for_empty_list() {
        let mut expr = CompiledExpression::new_from_string("list xload", false);
        expr.create_filter_context(100); // Insert at end - there is no label(100).
        expr.has_next(200);              // Insert at end - there is no label(200).
        expr.resolve_jumps();
        let ctx = NestedContext::new();
        // println!("Expression\n{}", expr);
        let mut interpreter = Interpreter::new(expr, ctx);
        let (actual, message) = interpreter.trace();
        // println!("{}", message);
        assert!(actual.is_false());
    }

    /// Test an OpCode stream that creates a list, makes a filter context,
    /// then tests if there is are any more items to iterate over in the list.
    /// Try for a list that is not empty, hence should return True.
    #[test]
    fn test_has_next_for_nonempty_list() {
        let mut expr = CompiledExpression::new_from_string("list number(3) push xload", false);
        expr.create_filter_context(100); // Insert at end - there is no label(100).
        expr.has_next(200);              // Insert at end - there is no label(200).
        expr.resolve_jumps();
        let ctx = NestedContext::new();
        // println!("Expression\n{}", expr);
        let mut interpreter = Interpreter::new(expr, ctx);
        let (actual, message) = interpreter.trace();
        // println!("{}", message);
        assert!(actual.is_true());
    }

    /// Filter a list of numbers, retaining all values that are over 10.
    /// 
    /// What makes it a filter operation and not a simple list indexing is
    /// that the stack is started with a list and a context,
    /// not a list and an integer.
    #[test]
    fn test_filter_list() {
        let mut expr = CompiledExpression::new_from_string("
list 
number(3) push 
number(12) push 
number(5) push 
number(11) push 
number(10) push 
number(20) push
xload
"
        , false);
        let mut predicate = CompiledExpression::new_from_string("dup num(10) >", false);
        let mut filter = CompiledExpression::new_filter(&mut predicate);
        expr.insert(&mut filter, 100);
        expr.resolve_jumps();
        let ctx = NestedContext::new();
        // println!("Expression\n{}", expr);
        let mut interpreter = Interpreter::new(expr, ctx);
        let (actual, message) = interpreter.trace();
        let numbers: Vec<FeelValue> = vec![12.into(),11.into(),20.into()];
        let expected = FeelValue::new_list(numbers);
        // println!("{}", message);
        assert_eq!(expected, actual);
    }

    /// Index a list of numbers, returning a single value.
    /// FEEL expects one-based indexing, but the "index" op performs
    /// zero-based indexing, so this checks that the generated code subtracts one from the index beforehand.
    /// 
    /// What makes it an index operation and not a filtering operation is
    /// that the stack is started with a list and a number,
    /// not a list and a context.
    #[test]
    fn test_index_list() {
        let mut expr = CompiledExpression::new_from_string("
// Initialize list
list 
number(3) push 
number(12) push 
number(5) push 
number(11) push 
number(10) push 
number(20) push
// List index (one-based)
num(3)
"
        , false);
        // The predicate will be ignored, so use true.
        let mut predicate = CompiledExpression::new_from_string("true", false);
        let mut filter = CompiledExpression::new_filter(&mut predicate);
        expr.insert(&mut filter, 100);
        expr.resolve_jumps();
        let ctx = NestedContext::new();
        // println!("Expression\n{}", expr);
        let mut interpreter = Interpreter::new(expr, ctx);
        let (actual, message) = interpreter.trace();
        let numbers: Vec<FeelValue> = vec![12.into(),11.into(),20.into()];
        let expected = FeelValue::Number(5.0); // One-based index inot the list
        // println!("{}", message);
        assert_eq!(expected, actual);
    }

}
