// use std::cmp::{Ord, PartialOrd, Ordering};
use super::opcode::{OpCode,RangeBoundType};
use super::compiled_expression::CompiledExpression;
use crate::parsing::feel_value::{FeelValue,FeelType};
use crate::parsing::range_access::RangeAccess;
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
                    OpCode::Pick => {
                        // 0 pick is a dup:    a -> a a' 
                        // 1 pick is an over:  a b -> a b a'
                        // 2 pick does this:   a b c -> a b c a'
                        self.advance();
                        match self.pop_data() {
                            FeelValue::Number(n) if n >= 0.0 => {
                                let position: i32 = self.data.len() as i32 - (n as i32) - 1;
                                if position < 0 {
                                    // TODO: Log error
                                    self.push_data(FeelValue::Null);
                                }
                                else {
                                    self.push_data(self.data[position as usize].clone());
                                }
                            },
                            _ => {
                                // TODO: Log error
                                self.push_data(FeelValue::Null);
                            }
                        }
                    },
                    OpCode::Roll => {
                        // 0 roll is a no-op: a -> a 
                        // 1 roll is a swap:  a b -> b a
                        // 2 roll is a rot:   a b c -> b c a
                        // 3 roll does this:  a b c d -> b c d a
                        self.advance();
                        match self.pop_data() {
                            FeelValue::Number(n) if n == 0.0 => {
                                // 0 roll does nothing.
                                ()
                            },
                            FeelValue::Number(n) if n >= 0.0 => {
                                let position: i32 = self.data.len() as i32 - (n as i32) - 1;
                                if position < 0 {
                                    // TODO: Log error
                                    self.push_data(FeelValue::Null);
                                }
                                else {
                                    let value = self.data.remove(position as usize);
                                    self.push_data(value);
                                }
                            },
                            _ => {
                                // TODO: Log error
                                self.push_data(FeelValue::Null);
                            }
                        }
                    },

                    OpCode::Store => {
                        // Arguments in the reverse order from xup.
                        self.advance();
                        let (value, key) = self.pop_two();
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
                            (FeelValue::List(rr_list), FeelValue::Number(i)) if index.is_integer() && i < 0.0 => {
                                // A negative index means relative to the end of the list.
                                let i_index: i32 = rr_list.borrow().len() as i32 + (i as i32);
                                if i_index >= 0 {
                                    self.push_data(rr_list.borrow()[i_index as usize].clone());
                                }
                                else {
                                    self.error(
                                        format!("List index out of range: {}", i),
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
                                    format!("Cannot add key to context because the key type is {} and context type is {}", 
                                        key.get_type().to_string(), ctx.get_type().to_string()),
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
                        // This makes it easier to deal with filtering by expressions
                        // that use a property name of the "item" and do not 
                        // reference "item" at by name.
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
                    OpCode::LoopBounds => {
                        self.advance();
                        // Iteration_context should be a list or a Range, not a context!
                        // The term "iteration context" is used in the DMN spec to describe
                        // the object that a loop iterates over.
                        // However, FEEL is lenient in that in some places
                        // where a list is expected, a scalar may be used
                        // and it is automatically wrapped in a list in order to facilitate further processing. 
                        // We will do that here, even for Nulls. 

                        // Order of values that are pushed (from nearer to stack bottom to stack top): 
                        //    loop context #
                        //    loop position # 
                        //    loop start index # 
                        //    loop stop index #
                        //    loop count # 
                        //    loop step #
                        // (Those variable names are not set here - the loop initialization code will 
                        // call xup for each of the variables.)
                        let iteration_context = self.pop_data();
                        let (start, stop, count, step) = iteration_context.loop_bounds(&self.contexts);
                        match iteration_context.get_type() {
                            FeelType::List => {
                                self.push_data(iteration_context);
                            },
                            FeelType::Range => {
                                self.push_data(iteration_context);
                            },
                            _ => {
                                // Promote to a list
                                let list_context = FeelValue::new_list(vec![iteration_context]);
                                self.push_data(list_context);
                            }
                        }
                        // Loop position will start out such that by adding step to it, you are at the beginning of iteration. 
                        self.push_data(&start - &step); // loop position
                        self.push_data(start); // loop start index
                        self.push_data(stop); // loop stop index - is inclusive
                        self.push_data(count); // loop count
                        self.push_data(step); // loop step - for ascending order
                    },
/*
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

                    OpCode::CallFunction => {
                        self.advance();
                        let (function_name, argument_list) = self.pop_two();
                        let mut error_reported = false;
                        let function_opt = match function_name.clone() {
                            FeelValue::Name(qname) => self.contexts.get(qname),
                            FeelValue::String(sname) => self.contexts.get(sname),
                            _ => {
                                self.error(
                                    format!("Cannot call function where name is a {}", function_name.get_type().to_string()),
                                    true
                                );
                                error_reported = true;
                                None
                            }
                        };
                        match (function_opt, error_reported) {
                            (Some(FeelValue::Function(function)), _) => {
                                let result = function(&argument_list, &mut self.contexts);
                                self.push_data(result);
                            },
                            (Some(_), _) => {
                                self.error(
                                    format!("Value in context named {} is not a function", function_name),
                                    true
                                );
                            },
                            (None, true) => {},
                            (None, false) => {
                                self.error(
                                    format!("No function in context named {}", function_name),
                                    true
                                );
                            }
                        }
                    },

                    // Get the property value, with three main cases, based on the type of the source: 
                    //   1. Scalar: Usually a date/time/datetime/duration. Get the special property value. 
                    //   2. List: Return a list that maps the result of get property to each element of the list. 
                    //   3. Context: Use the property name as a key and get the corresponding value from the context.
                    OpCode::GetProperty => {
                        self.advance();
                        let (source, property) = self.pop_two();
                        let value = self.map_by_property(&source, &property);
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

    // Get the source's property value, with three main cases, based on the type of the source: 
    //   1. Scalar: Usually a date/time/datetime/duration. Get the special property value. 
    //   2. List: Return a list that maps the result of get_property to each element of the list. 
    //   3. Context: Use the property name as a key and get the corresponding value from the context.
    fn map_by_property(&self, source: &FeelValue, property: &FeelValue) -> FeelValue {
        let qname: QName = match property {
            FeelValue::String(s) => {
                 match (&s).parse() {
                     Ok(q) => q,
                     Err(_) => {
                        ExecutionLog::log(&format!("String {} is not a valid property Name", property.to_string()));
                        return FeelValue::Null;
                     }
                 }
            },
            FeelValue::Name(q) => q.clone(),
            _ => {
                ExecutionLog::log(&format!("Property is {}, not a name or a string", property.get_type().to_string()));
                return FeelValue::Null;
            }
        };
        
        match source {
            FeelValue::List(rr_list) => {
                // List. Map each item to its key or property value.
                let list: Vec<FeelValue> = rr_list
                    .borrow()
                    .iter()
                    .map(|item| -> FeelValue
                        {
                            match item {
                                FeelValue::Context(rc_ctx) => {
                                    // TODO: Bad: cloning in a loop. Should change signature of get to expect something else. 
                                    match (*rc_ctx).get(qname.clone()) {
                                        Some(value) => value,
                                        None => FeelValue::Null
                                    }
                                },
                                _ => item.get_property(property, &self.contexts)
                            }
                        }
                    )
                    .collect();
                FeelValue::new_list(list)
            },
            FeelValue::Context(rc_ctx) => {
                // Context. Get value of key.
                match (*rc_ctx).get(qname) {
                    Some(value) => value,
                    None => FeelValue::Null
                }
            },
            _ => {
                // Scalar. Use get_value.
                source.get_property(property, &self.contexts)
            }
        }
    }
}

// ///////////////////////////////////// //
//                                       //
//               Tests                   //
//                                       //
// ///////////////////////////////////// //

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

  fn print_diagnostics() -> bool {
      true
  }

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
    //      Complex Interpreter tests        //
    //                                       //
    // ///////////////////////////////////// //

    #[test]
    fn test_type_of_context() {
        let mut expr = CompiledExpression::new_from_string("xload type?(context<>)", false);
        expr.resolve_jumps();
        let ctx = NestedContext::new();
        if print_diagnostics() { println!("Expression\n{}", expr); } 
        let mut interpreter = Interpreter::new(expr, ctx);
        let (actual, message) = interpreter.trace();
        if print_diagnostics() { println!("{}", message); }
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
        if print_diagnostics() { println!("Expression\n{}", expr); }
        let mut interpreter = Interpreter::new(expr, ctx);
        let (actual, message) = interpreter.trace();
        if print_diagnostics() { println!("{}", message); }
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
        if print_diagnostics() { println!("Expression\n{}", expr); }
        let mut interpreter = Interpreter::new(expr, ctx);
        let (actual, message) = interpreter.trace();
        if print_diagnostics() { println!("{}", message); }
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
            // This xload is to create a blank context. 
            // Some use cases will supply a real context. 
            xload
        "
        , false);
        let mut predicate = CompiledExpression::new_from_string("dup num(10) >", false);
        let mut filter = CompiledExpression::new_filter(&mut predicate);
        expr.insert(&mut filter, 100);
        expr.resolve_jumps();
        let ctx = NestedContext::new();
        if print_diagnostics() { println!("Expression\n{}", expr); }
        let mut interpreter = Interpreter::new(expr, ctx);
        let (actual, message) = interpreter.trace();
        let numbers: Vec<FeelValue> = vec![12.into(),11.into(),20.into()];
        let expected = FeelValue::new_list(numbers);
        if print_diagnostics() { println!("{}", message); }
        assert_eq!(expected, actual);
    }

    /// Test that a filter of a list of contexts that refers to a property of the
    /// item works. The keyword "item" is not part of the expression.
    #[test]
    fn test_filter_list_of_contexts_without_item() {
        let mut expr = CompiledExpression::new_from_string("
            list 
                xload
                    'name'  'Sam'   xset
                    'score' num(75) xset
                    push
                xload
                    'name'  'Jill'  xset
                    'score' num(90) xset
                    push
                xload
                    'name'  'Sal'   xset
                    'score' num(85) xset
                    push
                // Obligatory blank context
                xload
        "
        , false);

        // Predicate will filter based on score property of items being over 80
        let mut predicate = CompiledExpression::new_from_string(
            "'score' xget num(80) >", 
            false
        );
        let mut filter = CompiledExpression::new_filter(&mut predicate);
        expr.insert(&mut filter, 100);
        expr.resolve_jumps();
        let ctx = NestedContext::new();
        if print_diagnostics() { println!("Expression\n{}", expr); }
        let mut interpreter = Interpreter::new(expr, ctx);
        let (actual, message) = interpreter.trace();

        // Compose the expected result
        let ctx1 = Context::new();
        ctx1.insert("name", "Jill".into());
        ctx1.insert("score", 90.0.into());
        let ctx2 = Context::new();
        ctx2.insert("name", "Sal".into());
        ctx2.insert("score", 85.0.into());
        let contexts: Vec<FeelValue> = vec![
            ctx1.into(),
            ctx2.into()
        ];
        let expected = FeelValue::new_list(contexts);

        if print_diagnostics() { println!("{}", message); }
        assert_eq!(expected, actual);
    }

    /// Test that a filter of a list of contexts that refers to the keyword "item" and one of its properties.
    #[test]
    fn test_filter_list_of_contexts_using_item() {
        let mut expr = CompiledExpression::new_from_string("
            list 
                xload
                    'name'  'Sam'   xset
                    'score' num(75) xset
                    push
                xload
                    'name'  'Jill'  xset
                    'score' num(90) xset
                    push
                xload
                    'name'  'Sal'   xset
                    'score' num(85) xset
                    push
                // Obligatory blank context
                xload
        "
        , false);

        // Predicate will filter based on score property of items being over 80
        let mut predicate = CompiledExpression::new_from_string(
            "'item' xget xpush 'score' xget num(80) > xpop drop", 
            false
        );
        let mut filter = CompiledExpression::new_filter(&mut predicate);
        expr.insert(&mut filter, 100);
        expr.resolve_jumps();
        let ctx = NestedContext::new();
        if print_diagnostics() { println!("Expression\n{}", expr); }
        let mut interpreter = Interpreter::new(expr, ctx);
        let (actual, message) = interpreter.trace();

        // Compose the expected result
        let ctx1 = Context::new();
        ctx1.insert("name", "Jill".into());
        ctx1.insert("score", 90.0.into());
        let ctx2 = Context::new();
        ctx2.insert("name", "Sal".into());
        ctx2.insert("score", 85.0.into());
        let contexts: Vec<FeelValue> = vec![
            ctx1.into(),
            ctx2.into()
        ];
        let expected = FeelValue::new_list(contexts);

        if print_diagnostics() { println!("{}", message); }
        assert_eq!(expected, actual);
    }

    /// Extract the 'score' property from a list of contexts to create a list of score values.
    #[test]
    fn test_map_property_to_list_of_contexts() {
        let expr = CompiledExpression::new_from_string("
            list 
                xload
                    'name'  'Sam'   xset
                    'score' num(75) xset
                    push
                xload
                    'name'  'Jill'  xset
                    'score' num(90) xset
                    push
                xload
                    'name'  'Sal'   xset
                    'score' num(85) xset
                    push
            'score'
            .
        "
        , true);

        if print_diagnostics() { println!("Expression\n{}", expr); }

        let mut interpreter = Interpreter::new(expr, NestedContext::new());
        let (actual, message) = interpreter.trace();
        let expected = FeelValue::new_list(vec![75.0.into(), 90.0.into(), 85.0.into()]);

        if print_diagnostics() { println!("{}", message); }

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_instance_of_complex_case() {
        let expr = CompiledExpression::new_from_string("
            list 
                xload
                    'name'  'Sam'   xset
                    'score' num(75) xset
                    push
                xload
                    'name'  'Jill'  xset
                    'score' num(90) xset
                    push
                xload
                    'name'  'Sal'   xset
                    'score' num(85) xset
                    push
            'list<context<name:string,score:number>>'
            is
        "
        , true);

        if print_diagnostics() { println!("Expression\n{}", expr); }

        let mut interpreter = Interpreter::new(expr, NestedContext::new());
        let (actual, message) = interpreter.trace();
        let expected = FeelValue::Boolean(true);

        if print_diagnostics() { println!("{}", message); }

        assert_eq!(expected, actual);
    }

    /// Get the 'score' property from a single context.
    #[test]
    fn test_context_key_lookup() {
        let expr = CompiledExpression::new_from_string("
            xload
                'name'  'Sam'   xset
                'score' num(75) xset
            'score'
            .
        "
        , true);

        if print_diagnostics() { println!("Expression\n{}", expr); }

        let mut interpreter = Interpreter::new(expr, NestedContext::new());
        let (actual, message) = interpreter.trace();
        let expected: FeelValue = 75.0.into();

        if print_diagnostics() { println!("{}", message); }

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_in_operator_for_list_of_scalars() {
        let expr = CompiledExpression::new_from_string("
            num(5)
            list num(3) push num(4) push num(5) push
            in  
        "
        , true);

        if print_diagnostics() { println!("Expression\n{}", expr); }

        let mut interpreter = Interpreter::new(expr, NestedContext::new());
        let (actual, message) = interpreter.trace();

        if print_diagnostics() { println!("{}", message); }

        assert!(actual.is_true());
    }

    #[test]
    fn test_in_operator_for_qname_pointing_to_scalar() {
        let expr = CompiledExpression::new_from_string("
            xload
                'name'  'Sam'   xset
                'score' num(75) xset
            xpush
            num(75)
            'score' name
            in  
        "
        , true);

        if print_diagnostics() { println!("Expression\n{}", expr); }

        let mut interpreter = Interpreter::new(expr, NestedContext::new());
        let (actual, message) = interpreter.trace();

        if print_diagnostics() { println!("{}", message); }

        assert!(actual.is_true());
    }

    #[test]
    fn test_in_operator_for_qname_pointing_to_list() {
        let expr = CompiledExpression::new_from_string("
            xload
                'name'  'Sam'   xset
                'scores' list num(75) push num(80) push num(90) push xset
            xpush
            num(80)
            'scores' name
            in  
        "
        , true);

        if print_diagnostics() { println!("Expression\n{}", expr); }

        let mut interpreter = Interpreter::new(expr, NestedContext::new());
        let (actual, message) = interpreter.trace();

        if print_diagnostics() { println!("{}", message); }

        assert!(actual.is_true());
    }

    #[test]
    fn test_in_operator_for_range() {
        let expr = CompiledExpression::new_from_string("
            num(10)
            num(0) num(10) [lo,hi]
            in  
        "
        , true);

        if print_diagnostics() { println!("Expression\n{}", expr); }

        let mut interpreter = Interpreter::new(expr, NestedContext::new());
        let (actual, message) = interpreter.trace();

        if print_diagnostics() { println!("{}", message); }

        assert!(actual.is_true());
    }


    /// Index a list of numbers using a positive index, returning a single value.
    /// FEEL expects one-based indexing, but the "index" op performs
    /// zero-based indexing, so this checks that the generated code subtracts one from the index beforehand.
    /// 
    /// What makes it an index operation and not a filtering operation is
    /// that the stack is started with a list and a number,
    /// not a list and a context.
    #[test]
    fn test_index_list_positive() {
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
        if print_diagnostics() { println!("Expression\n{}", expr); }
        let mut interpreter = Interpreter::new(expr, ctx);
        let (actual, message) = interpreter.trace();
        let expected = FeelValue::Number(5.0); // One-based index into the list
        if print_diagnostics() {  println!("{}", message); }
        assert_eq!(expected, actual);
    }

    /// Index a list of numbers using a negative index, returning a single value.
    /// Negative index means relative to the end of the list.
    /// What makes it an index operation and not a filtering operation is
    /// that the stack is started with a list and a number,
    /// not a list and a context.
    #[test]
    fn test_index_list_negative() {
        let mut expr = CompiledExpression::new_from_string("
// Initialize list
list 
    num(1) push 
    num(2) push 
    num(3) push 
    num(4) push 

// List index (relative to end)
num(-2)
"
        , false);
        // The predicate will be ignored, so use true.
        let mut predicate = CompiledExpression::new_from_string("true", false);
        let mut filter = CompiledExpression::new_filter(&mut predicate);
        expr.insert(&mut filter, 100);
        expr.resolve_jumps();
        let ctx = NestedContext::new();
        if print_diagnostics() { println!("Expression\n{}", expr); }
        let mut interpreter = Interpreter::new(expr, ctx);
        let (actual, message) = interpreter.trace();
        let expected = FeelValue::Number(3.0);
        if print_diagnostics() {  println!("{}", message); }
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_function_call() {
        let expr = CompiledExpression::new_from_string(
            "'sum' list num(1) push num(2) push num(3) push call", 
            true // resolve the jumps
        );

        // We need the builtins added so that we have functions to call.
        let ctx = NestedContext::new_with_builtins();

        if print_diagnostics() { println!("Expression\n{}", expr); }
        let mut interpreter = Interpreter::new(expr, ctx);
        let (actual, message) = interpreter.trace();
        if print_diagnostics() { println!("{}", message); }
        let expected: FeelValue = 6.0.into();
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_between2() {
        let expr = CompiledExpression::new_from_string(
            "num(5) num(1) num(10) between", 
            true // resolve the jumps
        );
        if print_diagnostics() { println!("Expression\n{}", expr); }
        let mut interpreter = Interpreter::new(expr, NestedContext::new_with_builtins());
        let (actual, message) = interpreter.trace();
        if print_diagnostics() { println!("{}", message); }
        assert!(actual.is_true());
    }

    #[test]
    fn test_pick() {
        let expr = CompiledExpression::new_from_string(
            "1 2 3 4 3 pick swap drop swap drop swap drop swap drop", 
            true // resolve the jumps
        );
        if print_diagnostics() { println!("Expression\n{}", expr); }
        let mut interpreter = Interpreter::new(expr, NestedContext::new_with_builtins());
        let (actual, message) = interpreter.trace();
        if print_diagnostics() { println!("{}", message); }
        assert_eq!(FeelValue::Number(1.0), actual);
    }

    #[test]
    fn test_for_loop_over_single_list() {
        let expected = FeelValue::new_list(vec![1.0.into(), 4.0.into(), 9.0.into(), 16.0.into()]);

        let mut square = CompiledExpression::new_from_string(" 'n' @ 'n' @ * ", false);
        let loop_variable = FeelValue::Name(QName::from_str("n").unwrap());
        let mut loop_context = CompiledExpression::new_from_string(" list 1 push 2 push 3 push 4 push ", false);
        let mut loops: Vec<(&FeelValue, &mut CompiledExpression)> = vec![(&loop_variable, &mut loop_context)];
        let mut for_expression = CompiledExpression::for_loops(&mut loops, &mut square);
        for_expression.resolve_jumps();

        if print_diagnostics() { println!("Expression\n{}", for_expression); }
        let mut interpreter = Interpreter::new(for_expression, NestedContext::new_with_builtins());
        let (actual, message) = interpreter.trace();
        if print_diagnostics() { println!("{}", message); }
        assert_eq!(expected, actual);        
    }

    #[test]
    fn test_for_loop_over_two_lists() {
        let expected = FeelValue::new_list(vec![
            1.0.into(), 2.0.into(), 
            4.0.into(), 8.0.into(), 
            9.0.into(), 18.0.into(), 
            16.0.into(), 32.0.into()]
        );

        let mut square_times = CompiledExpression::new_from_string(" 'n' @ 'n' @ * 'm' @ *", false);
        let outer_loop_variable = FeelValue::Name(QName::from_str("n").unwrap());
        let inner_loop_variable = FeelValue::Name(QName::from_str("m").unwrap());
        let mut outer_loop_context = CompiledExpression::new_from_string(" list 1 push 2 push 3 push 4 push ", false);
        let mut inner_loop_context = CompiledExpression::new_from_string(" list 1 push 2 push ", false);
        let mut loops: Vec<(&FeelValue, &mut CompiledExpression)> = vec![
            (&outer_loop_variable, &mut outer_loop_context),
            (&inner_loop_variable, &mut inner_loop_context)
        ];
        let mut for_expression = CompiledExpression::for_loops(&mut loops, &mut square_times);
        for_expression.resolve_jumps();

        if print_diagnostics() { println!("Expression\n{}", for_expression); }
        let mut interpreter = Interpreter::new(for_expression, NestedContext::new_with_builtins());
        let (actual, message) = interpreter.trace();
        if print_diagnostics() { println!("{}", message); }
        assert_eq!(expected, actual);        
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


}

