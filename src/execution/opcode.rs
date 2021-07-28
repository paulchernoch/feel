use std::fmt::{Display,Formatter,Result};
use std::str::FromStr;
use lazy_static::lazy_static;
use regex::{Regex,Captures};
use ordered_float::OrderedFloat;

/// A bound (lower or upper) for a Range may be open-ended, inclusive or exclusive.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum RangeBoundType {
    Open,
    Inclusive, 
    Exclusive
}

/// An OpCode is a virtual machine instruction, like an assembly language operation. 
/// All Feel expressions may be translated into a series of such instructions,
/// with the exception of Strings, which will be loaded from the Engine's heap by index,
/// as well as some things related to Contexts. 
/// 
/// Feel permits the definition of new functions, but none of that is exposed 
/// via OpCodes. The compiler divides the code for a new function from the rest of the expression
/// and creates it in a separate Engine, then links them via the context. 
/// 
/// Each operator has a comment that describes the effect on the data stack. 
///   - b is for boolean
///   - n is for number
///   - s is for string
///   - Q is for a QName 
///   - r is for range
///   - l is for list
///   - E is for end of list
///   - c is for context
///   - d is for date
///   - t is for time
///   - D is for datetime
///   - y is for year month duration
///   - z is for day time duration
///   - ? is for one value of unspecified type
///   - + is for one or more values of unspecified type
///   - <x>+ is for one or more values of a specified type x
///   - * is for zero or more values of unspecified type
///   - _ represents the unchanged portion of the stack
///   - 0 is for Null
/// 
/// The comments may miss some rare but permitted cases and do not
/// address nulls. If the arguments are of incompatible types or 
/// outside of the proper range, the correct number of items will be removed
/// from the stack and replaced with a null. 
/// 
/// If multiple stacks or stacks other than the value stack are affected,
/// prefix with "ctx:" for the context stack and "val:" for the data stack. 
/// 
/// Notes: 
///    1. OrderedFloat is used to box floats for LoadNumber so that we can implement Eq using derive. 
/// 
///    2. Contexts appear in both the value (aka data) and context stacks. 
///       LoadContext creates a new context with no keys and pushes it on the value stack. 
///       create_filter_context (a macro of multiple ops) pops a List and a Context from the value stack to make a filter context 
///       that associates the list with the key "items", plus the key "next item index" is set to zero. 
///       Then it pushes the new context onto the value stack. 
///       PushContext pops a context from the value stack and pushes it onto the context stack.
///       PopContext pops the context from the context stack and pushes it onto the value stack. 
///       Drop allows you to finally pop the context from the value stack.
///    
///    3. FEEL lists are indexed by one-based indices, 
///       but the Index OpCode is zero-based.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum OpCode {
  // Stack manipulation (ala Forth programming language)
  Swap, // - swaps the top two items on the stack: (a b -> b a)
  Over, // - copies the second item from the top and pushes it: (a b -> a b a') 
  Rot,  // - rotates the third item to the top: (a b c -> b c a) 
  Dup,  // - duplicates the top of the stack: (a -> a a')
  Drop, // - pops one item off the top of the stack (a b -> a)

  // Arithmetic
  Add,                        // ? ? -> ?   Details: n n -> n OR d y -> d OR y d -> d OR t z -> t OR z t -> t
  Subtract,                   // ? ? -> ?   Details: n n -> n OR d y -> d OR t z -> t
  Negate,                     //   ? -> ?   Details:   n -> n OR   y -> y OR   z -> z
  Multiply,                   // ? ? -> ?   Details: n n -> n OR y n -> y OR n y -> y OR z n -> z OR n z -> z
  Divide,                     // ? ? -> ?   Details: n n -> n OR y n -> y OR z n -> z
  Exponentiate,               // ? ? -> ?   Details: n n -> n

  // Logical
  Not,                        //   b -> b
  Or,                         // b b -> b
  And,                        // b b -> b

  // Relational
  LessThan,                   // ? ? -> b
  LessThanOrEqual,            // ? ? -> b
  NotEqual,                   // ? ? -> b
  Equal,                      // ? ? -> b
  GreaterThan,                // ? ? -> b
  GreaterThanOrEqual,         // ? ? -> b

  // Miscellaneous operators
  Between,                    // ? ? ? -> b
  In,                         // ? l -> ?
  InstanceOf,                 // ? Q -> b OR ? s -> b
  
  // Lists
  CreateList,                 //   * -> l
  PushList,                   // l ? -> l
  Index,                      // l # => ?   Zero-based index of a list.

  // Contexts
  LoadFromContext,            //     Q -> ? OR s -> ?
  AddEntryToContext,          // c Q ? -> c
  PushContext,                // val: _ c -> _  ctx:   _ -> _ c
  PopContext,                 // val: _ -> _ c  ctx: _ c -> _
  /// A loop context forms a cartesian product of one or more lists. 
  /// The usize value counts how many lists to include. 
  CreateLoopContext(usize),      // val: _ l+ -> _  ctx: _ -> _ c
  CreatePredicateContext(usize), // val: _ l+ -> _  ctx: _ -> _ c
  LoadContext,                   // val:    _ -> _ c

  // Filtering

  // NOTE: +filter, item and items? will be macros composed of multiple OpCodes, hence not appear here
  IsType(usize), // Gets the type of the top of the data stack, compares it to the heap string referenced by the opcode using "instance of" and pushes True or False.
  ListLength,    // Gets the length of the next item on the value stack. If it is a list, it is the list length, otherwise 1. Consumes the list.
  Increment,     // Increment the value of the property whose name is atop the value stack in the topmost context that defines it and push the new value onto the value stack.
  UpdateContext, // Update a key-value pair in the contexts stack. Different from xset, which operates on a context that is on the value stack. 


  // Create Literals and push them onto the value stack

  CreateRange { lower: RangeBoundType, upper: RangeBoundType }, // ? ? -> _ r OR ? -> r
  CreateDate,                    // s -> d
  CreateTime,                    // s -> t
  CreateDateTime,                // s -> D
  CreateYearsAndMonthsDuration,  // s -> y
  CreateDayAndTimeDuration,      // s -> z
  /// Load a string from the heap given its zero-based index and push onto the data stack.
  LoadString(usize),             // _ -> _ s
  CreateName,                    // s -> Q
  LoadNumber(OrderedFloat<f64>), // _ -> n
  LoadBoolean(bool),             // _ -> b
  LoadNull,                      // _ -> _ 0

  /// Call a function 
  CallFunction,                  // Q L -> ? OR s L -> ?

  /// Get the value of a named property
  GetProperty,                   // ? Q -> ?

  // Branching and labels
  GotoLabel(usize),              // _ -> _
  GotoAddress(usize),            // _ -> _
  BranchToLabel { true_label: usize, false_label: usize, null_label: usize },
  BranchToAddress { true_address: usize, false_address: usize, null_address: usize },
  Label(usize),                  // _ -> _
  ExitLoopLabel(usize),          // Uncertain. 
  ExitLoopAddress(usize),        // Uncertain. 
  BranchExitLabel { true_label: usize, false_label: usize, null_label: usize },         // Uncertain.
  BranchExitAddress { true_address: usize, false_address: usize, null_address: usize }, // Uncertain.

  Return                         // ? -> <empty>
}

impl OpCode {
  /// Create a LoadNumber OpCode.
  pub fn load_number(num: f64) -> OpCode {
    OpCode::LoadNumber(OrderedFloat(num))
  }

  /// If the OpCode is a Label, get its position, otherwise None.
  pub fn get_label_position(&self) -> Option<usize> {
    match self {
      OpCode::Label(position) => Some(*position),
      _ => None
    }
  }

}

impl Display for OpCode {
  fn fmt(&self, f: &mut Formatter) -> Result {
    let tmp: String;
    let msg = match self {
        OpCode::Swap => "swap",
        OpCode::Over => "over", 
        OpCode::Rot => "rot", 
        OpCode::Dup => "dup",
        OpCode::Drop => "drop",

        OpCode::Add => "+",
        OpCode::Subtract => "-",
        OpCode::Negate => "neg",
        OpCode::Multiply => "*",
        OpCode::Divide => "/",
        OpCode::Exponentiate => "^",
        OpCode::Not => "!",
        OpCode::Or => "or",
        OpCode::And => "and",
        OpCode::LessThan => "<",
        OpCode::LessThanOrEqual => "<=",
        OpCode::NotEqual => "!=",
        OpCode::Equal => "=",
        OpCode::GreaterThan => ">",
        OpCode::GreaterThanOrEqual => ">=",
        OpCode::Between => "between",
        OpCode::In => "in ",
        OpCode::InstanceOf => "is",
        OpCode::CreateList => "list",
        OpCode::Index => "index",
        OpCode::PushList => "push",
        OpCode::LoadFromContext => "xget",
        OpCode::AddEntryToContext => "xset",
        OpCode::PushContext => "xpush",
        OpCode::PopContext => "xpop",
        OpCode::CreateLoopContext(dimensions) => { tmp = format!("+loop({})", dimensions); &tmp },
        OpCode::CreatePredicateContext(dimensions) => { tmp = format!("+pred({})", dimensions); &tmp },
        OpCode::LoadContext => "xload",

        OpCode::IsType(index) => { tmp = format!("type?({})", index); &tmp }
        OpCode::ListLength => "len",
        OpCode::Increment => "incr",
        OpCode::UpdateContext => "xup", 

        OpCode::CreateRange { lower, upper } => {
          match (lower, upper) {
            (RangeBoundType::Exclusive, RangeBoundType::Exclusive) => "(lo,hi)",
            (RangeBoundType::Exclusive, RangeBoundType::Inclusive) => "(lo,hi]",
            (RangeBoundType::Inclusive, RangeBoundType::Exclusive) => "[lo,hi)",
            (RangeBoundType::Inclusive, RangeBoundType::Inclusive) => "[lo,hi]",
            (RangeBoundType::Open, RangeBoundType::Exclusive) => "[..,hi)",
            (RangeBoundType::Open, RangeBoundType::Inclusive) => "[..,hi]",
            (RangeBoundType::Inclusive, RangeBoundType::Open) => "[lo,..]",
            (RangeBoundType::Exclusive, RangeBoundType::Open) => "(lo,..]",
            (RangeBoundType::Open, RangeBoundType::Open) => "[..,..]"
          }
        },
        OpCode::CreateDate => "date",
        OpCode::CreateTime => "time",
        OpCode::CreateDateTime => "dt",
        OpCode::CreateYearsAndMonthsDuration => "ym-duration",
        OpCode::CreateDayAndTimeDuration => "dt-durarion",
        OpCode::LoadString(index) => { tmp = format!("string({})", index); &tmp },
        OpCode::CreateName => "name",
        OpCode::LoadNumber(num) => { tmp = format!("num({})", num); &tmp },
        OpCode::LoadBoolean(b) => { tmp = format!("{}", b); &tmp },
        OpCode::LoadNull => "null",
        OpCode::CallFunction => "call",
        OpCode::GetProperty => ".",
        OpCode::GotoLabel(label) => { tmp = format!("goto({})", label); &tmp },
        OpCode::GotoAddress(address) => { tmp = format!("goto#({})", address); &tmp },
        OpCode::BranchToLabel { true_label, false_label, null_label } => { tmp = format!("branch({}/{}/{})", true_label, false_label, null_label); &tmp },
        OpCode::BranchToAddress { true_address, false_address, null_address } => { tmp = format!("branch#({}/{}/{})", true_address, false_address, null_address); &tmp },
        OpCode::Label(position) => { tmp = format!("label({})", position); &tmp },
        OpCode::ExitLoopLabel(label) => { tmp = format!("exit-label({})", label); &tmp }, 
        OpCode::ExitLoopAddress(address) => { tmp = format!("exit-addr({})", address); &tmp },  
        OpCode::BranchExitLabel { true_label, false_label, null_label } => { tmp = format!("branch-exit({}/{}/{})", true_label, false_label, null_label); &tmp },
        OpCode::BranchExitAddress { true_address, false_address, null_address } => { tmp = format!("branch-exit#({}/{}/{})", true_address, false_address, null_address); &tmp },
        OpCode::Return  => "return"
      };
    write!(f, "{}", msg)
  }
}

/// Error returned when parsing an OpCode from a string.
#[derive(Debug, Clone)]
pub struct OpCodeParseError {
    pub string_to_parse: String
}

impl Display for OpCodeParseError {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "OpCode may not be parsed from '{}'", self.string_to_parse)
    }
}


impl FromStr for OpCode {
    type Err = OpCodeParseError;

    /// Parse an OpCode from a string.
    /// This makes it convenient to create test data for unit tests. 
    /// 
    /// Note: Not every OpCode can be parsed in this way. Actual strings for use in LoadString require a CompiledEdxpression
    ///       as part of the parsing, as does "type?(type-string)". 
    ///       Such codes will be parsed in CompiledExpression if this returns an Err.
    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        let op = match s {
            "swap" => OpCode::Swap,
            "over" => OpCode::Over, 
            "rot" => OpCode::Rot, 
            "dup" => OpCode::Dup,
            "drop" => OpCode::Drop,

            "+" => OpCode::Add,
            "-" => OpCode::Subtract,
            "neg" => OpCode::Negate,
            "*" => OpCode::Multiply,
            "/" => OpCode::Divide,
            "^" => OpCode::Exponentiate,
            "!" => OpCode::Not,
            "or" => OpCode::Or,
            "and" => OpCode::And,
            "<" => OpCode::LessThan,
            "<=" => OpCode::LessThanOrEqual,
            "!=" => OpCode::NotEqual,
            "=" => OpCode::Equal,
            ">" => OpCode::GreaterThan,
            ">=" => OpCode::GreaterThanOrEqual,
            "between" => OpCode::Between,
            "in" => OpCode::In,
            "is" => OpCode::InstanceOf,
            "list" => OpCode::CreateList,
            "index" => OpCode::Index,
            "push" => OpCode::PushList,
            "xget" => OpCode::LoadFromContext,
            "xset" => OpCode::AddEntryToContext,
            "xpush" => OpCode::PushContext,
            "xpop" => OpCode::PopContext,
            "xload" => OpCode::LoadContext,

            "len" => OpCode::ListLength,
            "incr" => OpCode::Increment,
            "xup" => OpCode::UpdateContext, 

            "(lo,hi)" => OpCode::CreateRange { lower: RangeBoundType::Exclusive, upper: RangeBoundType::Exclusive },
            "(lo,hi]" => OpCode::CreateRange { lower: RangeBoundType::Exclusive, upper: RangeBoundType::Inclusive },
            "[lo,hi)" => OpCode::CreateRange { lower: RangeBoundType::Inclusive, upper: RangeBoundType::Exclusive },
            "[lo,hi]" => OpCode::CreateRange { lower: RangeBoundType::Inclusive, upper: RangeBoundType::Inclusive },
            "[..,hi)" => OpCode::CreateRange { lower: RangeBoundType::Open, upper: RangeBoundType::Exclusive },
            "[..,hi]" => OpCode::CreateRange { lower: RangeBoundType::Open, upper: RangeBoundType::Inclusive },
            "[lo,..]" => OpCode::CreateRange { lower: RangeBoundType::Inclusive, upper: RangeBoundType::Open },
            "(lo,..]" => OpCode::CreateRange { lower: RangeBoundType::Exclusive, upper: RangeBoundType::Open },
            "[..,..]" => OpCode::CreateRange { lower: RangeBoundType::Open, upper: RangeBoundType::Open }, 
              
            "date" => OpCode::CreateDate,
            "time" => OpCode::CreateTime,
            "dt" => OpCode::CreateDateTime,
            "ym-duration" => OpCode::CreateYearsAndMonthsDuration,
            "dt-duration" => OpCode::CreateDayAndTimeDuration,

            "name" => OpCode::CreateName,
            "true" => OpCode::LoadBoolean(true),
            "false" => OpCode::LoadBoolean(false),
            "null" => OpCode::LoadNull,
            "call" => OpCode::CallFunction,
            "." => OpCode::GetProperty,
            "return" => OpCode::Return,
            _ => {
                lazy_static! {
                    static ref FANCY_OPCODE_RE: Regex = Regex::new(r"(?x)
                        ^                              # Match start of string
                        (?P<opname>[-+?\#a-zA-Z]+)     # Match abbreviated name of OpCode as 'opname'
                        \(                             # Open parentheses
                        (?P<arg1>-?[0-9]+([.][0-9]+)?) # Match first number as 'arg1'
                        (/                             # Delimiter
                        (?P<arg2>[0-9]+)               # Optionally match second number as 'arg2'
                        /                              # Delimiter
                        (?P<arg3>[0-9]+))?             # Optionally match third number as 'arg3'
                        \)                             # Closing parenthesis
                        $                              # Match end of string
                    ").unwrap();
                }

                let opname: &str; 
                let mut arg1_float = 0.0_f64;
                let arg1: usize;
                let arg2: usize;
                let arg3: usize;
                fn get_named_match<'a>(name: &str, cap: &Captures<'a>, default_value: &'static str) -> &'a str {
                    match cap.name(name) {
                        Some(m) => m.as_str(),
                        None => default_value
                    }
                }
                fn parse_number(num_string: &str) -> usize {
                    num_string.parse::<usize>().unwrap_or_default()
                }
                match FANCY_OPCODE_RE.captures(s) {
                    Some(cap) => { 
                        opname = get_named_match("opname", &cap, "");
                        let arg1_string = get_named_match("arg1", &cap, "0");
                        arg1_float = arg1_string.parse::<f64>().unwrap_or_default();
                        arg1 = parse_number(arg1_string);
                        arg2 = parse_number(get_named_match("arg2", &cap, "0"));
                        arg3 = parse_number(get_named_match("arg3", &cap, "0"));
                    },
                    None => {
                      opname = "";
                      arg1 = 0;
                      arg2 = 0;
                      arg3 = 0;
                    }
                };
                match opname {
                    "number" | "num" => OpCode::load_number(arg1_float),
                    "+loop" => OpCode::CreateLoopContext(arg1),
                    "+pred" => OpCode::CreatePredicateContext(arg1),
                    "string" => OpCode::LoadString(arg1), 
                    "type?" => OpCode::IsType(arg1),
                    "num" => OpCode::load_number(arg1_float),
                    "goto" => OpCode::GotoLabel(arg1),
                    "goto#" => OpCode::GotoAddress(arg1),
                    "branch" => OpCode::BranchToLabel { true_label: arg1, false_label: arg2, null_label: arg3 },
                    "branch#" => OpCode::BranchToAddress { true_address: arg1, false_address: arg2, null_address: arg3 },
                    "label" => OpCode::Label(arg1),
                    "exit-label" => OpCode::ExitLoopLabel(arg1), 
                    "exit-addr" => OpCode::ExitLoopAddress(arg1),  
                    "branch-exit" => OpCode::BranchExitLabel { true_label: arg1, false_label: arg2, null_label: arg3 },
                    "branch-exit#" => OpCode::BranchExitAddress { true_address: arg1, false_address: arg2, null_address: arg3 },
                    _ => { 
                        return Err(OpCodeParseError{string_to_parse: s.to_string()}); 
                    }
                }
                
            }
        };
        Ok(op)
    }
}


/////////////// TESTS /////////////////

#[cfg(test)]
mod tests {
    use std::str::FromStr;
    use super::OpCode;
    use super::RangeBoundType;

    #[test]
    fn test_display() {
        assert_eq!(OpCode::Add.to_string(), "+".to_string());
        assert_eq!(OpCode::CreateRange { lower: RangeBoundType::Exclusive, upper: RangeBoundType::Inclusive }.to_string(), "(lo,hi]".to_string());
        assert_eq!(OpCode::CreateRange { lower: RangeBoundType::Inclusive, upper: RangeBoundType::Exclusive }.to_string(), "[lo,hi)".to_string());
        assert_eq!(OpCode::CreateLoopContext(2).to_string(), "+loop(2)".to_string());
        assert_eq!(OpCode::load_number(3.14_f64).to_string(), "num(3.14)".to_string());
        assert_eq!(OpCode::LoadBoolean(true).to_string(), "true".to_string());
    }

    #[test]
    fn test_get_label_position() {
        assert_eq!(OpCode::Label(5).get_label_position().unwrap(), 5);
        assert_eq!(OpCode::PopContext.get_label_position(), None);
    }
    

    #[test]
    fn test_from_str() {
        assert_eq!(OpCode::CreateLoopContext(2), OpCode::from_str("+loop(2)").unwrap(), "Parsing +loop");
        assert_eq!(OpCode::load_number(4.0), OpCode::from_str("number(4)").unwrap(), "Parsing number");
        assert_eq!(
            OpCode::BranchToAddress{ true_address: 0, false_address: 1, null_address: 2 }, 
            OpCode::from_str("branch#(0/1/2)").unwrap(),
            "Parsing branch"
        );

    }
}
