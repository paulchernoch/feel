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

## List Indexing and Filtering

Indexing or filtering a list has multiple use cases in Feel:

  - A non-list may be indexed if the index is zero: scalar[0]
  - A list may be indexed if the index is a positive number: list[+number]
  - A list may be indexed if the index is a negative number (count from the end of the list): list[-number]
  - A list may be filtered if the index is a context.

The first case returns the scalar unchanged. 

Because of the different semantics, this logic will be implemented as multiple OpCodes.
In the "code" below, "left" is the left operand, normally a list, and "right" is the right operand, normally a number,
but maybe a filter expression.

This will necessitate creating more OpCodes, inspired by the Forth language. 
In the descriptions below, the rightmost item is the top item on the stack. 

  - swap - swaps the top two items on the stack: (a b -> b a)
  - over - copies the second item from the top and pushes it: (a b -> a b a') 
  - rot  - rotates the third item to the top: (a b c -> b c a) 
  - dup  - duplicates the top of the stack: (a -> a a')
  - drop - pops one item off the top of the stack

Other OpCodes will be needed for the looping over the list, beyond the original design:

  - item - Gets the next item in the list from the filter context and pushes it onto the value stack. This tracks properties "item", "item index", "item count", and "item list".
  - items? - Returns True if the list has any more items, False otherwise. 
  - +filter - Pops a list from the data stack, creates a filter context and pushes that onto the context stack.
  - type?(t) - Gets the type of the top of the data stack, compares it to the heap string referenced by the opcode using "instance of" and pushes True or False.
  - len - Gets the length of the next item on the value stack. If it is a list, it is the list length, otherwise 1. Consumes the list.

The "type?(t)" OpCode is equivalent to pushing a string on the data stack and then an "is".

In the cases below, the changes to the stack are shown, and OpCodes that are skipped by branching are indicated by a hyphen.
"L" means the left argument, usually a List.
"I" means the right argument, usually a numeric index.
"S" is a string. In these cases, it is a type LatticeType string.
"b" is a Boolean.
"t" is a Boolean True.
"f" is a Boolean False.
"F" is a Filter Context.
"l" is a list.
"0" is number(0).
"#" is a number.
"a", "b", "c" etc are numbers indicating labels.
"+filter" must be replaced by the the actual filter expression, which consists of multiple opcodes. 
"c" is a context
"N" is a Null
"x" is a complex subexpression compised of many OpCodes that must result in pushing a Boolean or Null onto the value stack.

The code generator must recognize that a filtering operation is intended by searching the AST for:
  - references to the "item" variable in the subexpression
  - the top level expression contained in the filtering brackets is a boolean operator (<=, <, =, >=, >, and, or, not)
  - the top level expression calls a builtin function that returns a Boolean.
  
When any is found, it will cause the addition of codes that create an empty context. 
Of course, if the subexpression contains its own filter operation that 
references "item", that must be excluded!

Note: Section 10.3.2.5 of the DMN FEEL 1.3 spec identifies an edge case. If the values in the lefthand list
      are contexts that have the key "item", then item must refer to that context's value, not the filter context.

**Case 1: scalar[0]**
        In this case, L is not a list and I is number(0).

```
  over               (L I -> L I L')
  type?(list<Any>)   (L I L' -> L I f)
  branch(a/b/c)      (L I f -> L I)          Test if left argument is a list.

  label(a)                                   L is a list.
  type?(number)
  branch(f/g/c)                              L is a list, test if I is a number

  label(f)                                   L is a list and I is a number: 
  index                                      Perform a list index operation.
  goto(d)

  label(g)                                   L is a list and I is not a number: 
  type?(context<>)                           
  branch(h/c/c)                              Test if L is a context, indicating a filter operation

  label(h)                                   Assume it is a filter context - insert more opcodes here
  +filter                                    
  goto(d)

  label(b)           (L I -> L I)            L is a scalar.
  dup                (L I -> L I I')
  number(0)          (L I I' -> L I I' 0)
  =                  (L I S -> L I t)
  branch(e/c/c)      (L I t -> L I)          Test if right index argument is a zero.

  label(e)           (L I -> L I)            L is scalar and I = 0 branch.
  drop               (L I -> L)
  goto(d)            (L -> L)

  label(c)                                   Many error cases.
  drop
  drop
  null

  label(d)           (L -> L)                Done with indexing/filtering operation.

```

**Case 2: list[number] **
        In this case, L is a list and I is a number and indexing of the list is performed.

```
  over               (L I -> L I L')
  type?(list<Any>)   (L I L' -> L I t)
  branch(a/b/c)      (L I t -> L I)          Test if left argument is a list.

  label(a)           (L I -> L I)            L is a list.
  type?(number)      (L I -> L I t)
  branch(f/g/c)      (L I t -> L I)          L is a list, test if I is a number

  label(f)           (L I -> L I)            L is a list and I is a number: we will perform a list index operation.
  index              (L I -> A)              
  goto(d)            (A -> A)

  label(g)                                   L is a list and I is not a number: 
  type?(context<>)                           
  branch(h/c/c)                              Test if L is a context, indicating a filter operation

  label(h)                                   Assume it is a filter context - insert more opcodes here
  +filter                                    
  goto(d)

  label(b)                                   L is a scalar.
  dup                
  number(0)          
  =                  
  branch(e/c/c)                              Test if right index argument is a zero.

  label(e)                                   L is scalar and I = 0 branch.
  drop                   
  goto(d)             

  label(c)                                   Many error cases.
  drop
  drop
  null

  label(d)           (A -> A)                Done with indexing/filtering operation.

```

**Case 3: list[expression] **
        In this case, L is a list and I is a context, indicating that we will be 
        filtering the list and creating a new list of the items that pass the filter test.


```
  over               (L I -> L I L')
  type?(list<Any>)   (L I L' -> L I f)
  branch(a/b/c)      (L I t -> L I)          Test if left argument is a list.

  label(a)           (L I -> L I)            L is a list.
  type?(number)      (L I -> L I f) 
  branch(f/g/c)      (L I f -> L I)          L is a list, test if I is a number

  label(f)                                   L is a list and I is a number: we will perform a list index operation.
  index                            
  goto(d)            

  label(g)           (L I -> L I)            L is a list and I is not a number: 
  type?(context<>)   (L I -> L I t)          
  branch(h/c/c)      (L I t -> L I)          Test if L is a context, indicating a filter operation

  label(h)           (L I -> L I)            Assume it is a filter context - insert more opcodes here
  +filter            (L I -> F)
  xpush              (_ F -> _)
  list               (_ -> l)
  ...more opcodes...
  goto(d)            (A -> A)

  label(b)                                   L is a scalar.
  dup                
  number(0)          
  =                  
  branch(e/c/c)                              Test if right index argument is a zero.

  label(e)                                   L is scalar and I = 0 branch.
  drop                   
  goto(d)             

  label(c)                                   Many error cases.
  drop
  drop
  null

  label(d)           (A -> A)                Done with indexing/filtering operation.

```

## The item and item? OpCodes

When iterating through a list in a filter operation, two contexts must be pushed.

  - Filter context - has properties "item", "item index", and "item list"
  - Item as context or empty context

Since the filter expression may reference properties in the list item,
we need to push each list item onto the contexts stack as it is encountered, but only if it is a context.
If it is not a context, we will push an empt context.

Thus when "item?" is executed, it must bypass the top context and look in the next context on the stack.
Likewise, when "item" is executed, it must bypass the top context also.

When the iteration completes, we must "xpop" and "drop" twice, to remove both contexts.

At the beginning of iteration, we need a fake extra context so item? and item do not
xpop something that is not there.

  - xload
  - xpush

To accomplish this for "item?":

  - xpop
  - item?
  - swap
  - xpush

To perform "item":

  - xpop
  - drop
  - item
  - type?(context<>)
  - branch(a/b/b)
  - label(a)
  - dup               : Duplicates the item, to prepare for pushing it onto the context stack at the end
  - goto(c)
  - label(b)
  - xload             : Makes an empty context to be pushedat the end
  - label(c)
  - xpush


**Breaking +filter into smaller operations:**

The work that "+filter" has to perform:

  - Assume that a list and an empty context are already on top of the value stack to start with
  - Set the "item index" property to zero
  - Set the "item list" property to the list from the stack
  - Set the "item count" property to the length of the list from the stack
  - Set the "item" property to Null

The OpCodes:

  - string(item index)     (l c -> l c s)
  - number(0)              (l c s -> l c s 0)
  - xset                   (l c s 0 -> l c)         Sets "item index" in context
  - over                   (l c -> l c l')
  - len                    (l c l' -> l c #)
  - string(item count)     (l c # -> l c # s)
  - swap                   (l c # s -> l c s #)
  - xset                   (l c s # -> l c)         Sets "item count" in context                
  - string(item list)      (l c -> l c s)
  - rot                    (l c s -> c s l)
  - xset                   (c s l -> c)             sets "item list" in context
  - string(item)           (c -> c s)
  - null                   (c s -> c s N)
  - xset                   (c s N -> c)             Sets "item" in context
  - xpush                  (_ c -> _)

**Breaking item into smaller operations:**

The work that "item" has to perform:

  - Get the current "item index" from the context and push a clone on value stack
  - Get the "item list" from the context and push a clone on value stack 
  - Get the item from the list at the given index and push a clone on the value stack
  - Increment index
  - Store the new index value in "item index"

There will be some swaps or rots involved. Maybe add an "incr" op that performs the read, increment, and write in one go.

The OpCodes:

  

## Boxed Expression

  - What are they? 
  - How should they be executed?