# Execution Engine Design

The Execution Engine `Interpreter` will be a virtual machine which stores: 
  - **instruction stack** (holds `OpCode`s, Enums which are like assembly language instructions)
  - **instruction pointer** (integer index to the currently executing `OpCode`)
  - **data stack** (stack of `FeelValue` objects, which are intermediate values used in the calculations and from which the result of the expression will be plucked.)
  - **context** (a `NestedContext` onto which frames can be pushed or popped)
  - **heap** (Strings, which are of variable size. An `OpCode` can reference one of these Strings and be used to construct a `FeelValue::String`)

The parser will translate the source code text into a `CompiledExpression`, used as the basis of an `Interpreter`. All literal Strings  will be added to the `heap` and pointed to by `OpCode`s that reference them by integer index. Other literals will be represented by a series of instructions that can be used to construct them from strings or other `FeelValue`s. All logic (arithmetic operations, function calls, value lookups, for-loops, etc) will be translated into `OpCodes` that perform actions, in postfix order, like the **Forth** language.

## Caching: Send and Sync

To implement a service that handles multiple users, we must maintain a cache of compiled expressions. For them to be useable by multiple threads, Rust requires them to implement the `Send` and `Sync` traits. Since `FeelValue` does not and cannot, we need to segregate parts of the type that employ interior mutability 
(List and Context) that cannot implement Send and Sync. Thus the parser will generate a `CompiledExpression`, which only has the `InstructionStack` and a `Heap`. This will be immutable, can be cached and will implement `Send` and `Sync`. When it is time to be executed, an `Interpreter` is created from the `CompiledExpression` and is used to compute the result.

## Structs

The key structs and traits for the engine will be:

  - `Interpreter` will hold a `CompiledExpression`, maybe delegate to it using `Ambassador`
  - `OpCode`
  - `CompiledExpression` will implement `Send` and `Sync`
  - `FeelValue`
  - `ContextReader` trait and the structs that implement it:
  
    * `Context` (used for user defined dictionaries, the builtin functions context)
    * `NestedContext` (where you can push and pop child contexts)
    * `FunctionInvocationContext` (binds parameter names to arguments by position)
    * `LoopIterationContext` (for nested for-loops)
    * `PredicateIterationContext` (for `every` and `some` statements that check if all or any elements of a list are `FeelValue::Boolean(true)`)
    * `FilterIterationContext` (for filtering a list using a Boolean expression)

## User Defined Functions

User defined functions are tricky, as when they are called, we must push a new `Context` for its parameters and direct execution to a separate `Interpreter` for that function. When the function exits, we resume execution of the previous `Interpreter`.

## Iteration Contexts

The three iteration contexts are used when iterating over lists. They maintain special variables in their context that can be used in expressions.

`LoopIterationContext`: 
  - Has names for the iteration variables for each of the loops
  - Associates each iteration variable with a list of items or a `range<int>`
  - Maintains a list of results computed so far accessible by the name `partial`
  - Knows how many iterations in total will be performed and which iteration we are on
  - Can read the Nth value into each iteration variable

`PredicateIterationContext`:
  - Has names for the iteration variables for each of the loops
  - Associates each iteration variable with a list of items or a `range<int>`
  - Knows maximum number of iterations that may be performed and which iteration we are on
  - Can read the Nth value into each iteration variable
  - Exits when the answer is known, either true or false, possibly before the maximum number of iterations is reached (short-circuit)

`FilterIterationContext`:
  - The iteration variable name is always `item`
  - Stores the list to be iterated over 
  - Knows maximum number of iterations that may be performed (length of list) and which iteration we are on
  - Can read the Nth value into the `item` iteration variable
  - Mainains a list of items that satisfy the filter expression
  - Returns the list when complete
  
## Data Stack

Intermediate values are pushed and popped from this stack. At the end of function execution, the final value that remains on the stack is the result.
If the stack is empty, a Null is returned instead.

## Instruction Pointer

The currently executing instruction is an integer index into the instruction stack. Execution proceeds in two ways:

  - The instruction ponter is incremented by one and the next instruction is executed
  - A branch causes the the instruction pointer to skip forward or backward.

## Operation Codes

We need `OpCode` variants to support the following:

  - Arithmetic operators (+ - * / **)
  - Logical Operator (not or and)
  - Relational Operators (< <= != = > >=)
  - `between` operator
  - `in` operator (single or multiple tests)
  - Filter
  - `instance of` operator
  - Create List (empty to start)
  - Push List (push one value onto List)
  - Load from Context (access context by key name)
  - Start Context
  - Add Entry to Context
  - End Context
  - Pop Context
  - Create Loop Context
  - Create Predicate Context
  - Create Filter Context
  - Create Range (variants for inclusive, exclusive, open-ended for upper or lower)
  - Create Date
  - Create Time
  - Create DateTime
  - Create Years and Months Duration
  - Create Day and Time Duration
  - Load String (from heap)
  - Load Number
  - Load Boolean
  - Call Fixed Function (with given number of arguments)
  - Call Variable Function (with variable number of arguments, terminated by End Argument List)
  - End Argument List
  - GotoLabel (refers to a single label)
  - GotoAddress (refers to a single address)
  - BranchToLabel (refers to three labels, for if-else, for true, false and Null cases)
  - BranchToAddress (refers to two addresses, for if-else, for true, false and Null cases)
  - Label
  - Branch Exit Loop Label (for `some` or `every` loops)
  - Branch Exit Loop Address (for `some` or `every` loops)
  - Exit Loop Label
  - Exit Loop Address
  - HasNext (check if loop context has any more items over which to iterate)
  - PushNext (push next item from loop context)
  - Return

## Lists

Lists are created empty on the 

## Branching and Labels

During compilation, Labels are inserted into the instruction stream. The label will have a number which is the character position in the source code after which the label points. 

During the first phase of compilation, simple Goto operations are inserted as `GotoLabel` instructions and they name the Label to which they should jump.

During the second phase of compilation, `GotoLabel` `OpCode`s are replaced with `GotoAddress` `OpCode`s which identify the address (absolute position) in the instruction stack.

Analogously, BranchToLabel OpCodes are inserted during the first phase and replaced with BranchToAddress OpCodes. These operations have two labels/addresses, because they represent an if-else or part of an iterator loop check. One label/address points to the spot where the True branch should go, and the other to the false.

**Problem**: What if the if-else expression evaluates to Null? 

**Solution**: The `BranchToLabel` and `BranchToAddress` `OpCode`s must have a third branch for Null, which will jump to a Label after the if-else.

Loops over lists will be created with a Label for the start of the loop (for continuing, when there are more items left to iterate over). They will also have a Label for the end of the loop, to jump to when the last item is visited.

For Some or Every expressions, we need a break that exits the loop and another op that pops the iteration context for the short-circuiting. We can use the `BranchToAddress` and `BranchToLabel` ops. 

## CompiledExpression

A compiled expression must be a recursive structure. If one function is defined within another, it must hold a `CompiledExpression` for that inner function. Thus we need it to have an id, and link the id to a name when we learn it.

  - Opcodes: a Vec of Opcode in RPN order (Reverse Polish Notation, a postfix ordering)
  - Heap: a Vec of Strings
  - Definitions: a Vec of other CompiledExpressions (Boxed?)

As Strings are discovered, Opcodes are created that point to them by integer index into the Heap. The same is true for CompiledExpressions of inner functions which are linked to their position in Definitions.


## Boxed Expression

  - What are they? 
  - How should they be executed?