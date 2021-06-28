

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
///   - ? is for one value of unspefied type
///   - + is for one or more values of unspecified type
///   - <x>+ is for one or more values of a specified type x
///   - * is for zero or more values of unspecified type
///   - _ represents the unchanged portion of the stack
/// 
/// The comments may miss some rare but permitted cases and do not
/// address nulls. If the arguments are of incompatible types or 
/// outside of the proper range, the correct number of items will be removed
/// from the stack and replaced with a null. 
/// 
/// If multiple stacks or stacks other than the value stack are affected,
/// prefix with "ctx:" for the context stack and "val:" for the data stack. 
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum OpCode {
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
  And                         // b b -> b

  // Relational
  LessThan,                   // ? ? -> b
  LessThanOrEqual,            // ? ? -> b
  NotEqual,                   // ? ? -> b
  Equal,                      // ? ? -> b
  GreaterThan,                // ? ? -> b
  GreaterThanOrEqual          // ? ? -> b

  // Miscellaneous operators
  Between,                    // ? ? ? -> b
  In,                         // ? l -> ?
  Filter,                     //   l -> l
  InstanceOf,                 // ? Q -> b OR ? s -> b
  
  // Lists
  CreateList,                 //   * -> l
  PushList,                   // l ? -> l

  // Contexts
  LoadFromContext,            //     Q -> ? OR s -> ?
  AddEntryToContext,          // _ Q ? -> _
  PushContext,                // ctx:   _ -> _ c
  PopContext,                 // ctx: _ c -> _
  /// A loop context forms a cartesian product of one or more lists. 
  /// The usize value counts how many lists to include. 
  CreateLoopContext(usize),      // val: _ l+ -> _  ctx: _ -> _ c
  CreatePredicateContext(usize), // val: _ l+ -> _  ctx: _ -> _ c
  CreateFilterContext,           // val: _ l  -> _  ctx: _ -> _ c
  LoadContext,                   // val:    _ -> _ c

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
  LoadNumber(f64),               // _ -> n
  LoadBoolean(bool),             // _ -> b

  /// Call a function 
  CallFunction,                  // L Q -> ? OR L s -> ?

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

  /// For iterators, this probes the context to see if there are more items to iterate
  /// and pushes a True or False onto the data stack. 
  HasNext,                       // _ -> _ b
  /// For iterators, this gets the next value from the loop context and pushes it
  /// onto the data stack.
  PushNext,                      // _ -> _ ?

  Return                         // ? -> <empty>
}


