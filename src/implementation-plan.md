# Implementation Plan

## Grammar Recognizer

Add grammar rules for new features not supported by the Javascript PEG Grammar 
that was the basis of this project.

 - [ ] Duration literals like @"PT5H"
 - [ ] Date literals
 - [ ] Time Literals
 - [ ] Datetime literals
 - [ ] Other things I missed

## FeelValue

 - [x] Enum Defined
 - [x] Equality
 - [x] Add, Sub, Mul, Div
 - [ ] Contexts
    - [x] Context
    - [x] NestedContext
    - [ ] LoopIterationContext (for one or more nested for-loops)
    - [ ] PredicateIterationContext (for "every" and "some", which exits as soon 
          as predicate is known to be true or false) 
    - [ ] ListFilterIterationContext (each item in list assigned to 'item' key in the context)
 - [x] Inequality Operators
 - [x] Range
    - [x] Range includes
    - [x] Range added to FeelValue
    - [x] 14 HL7 CQL Range functions => implemented in Builtins instead.
 - [x] FeelFunction
 - [x] FeelFunction added to FeelValue
 - [x] Execution Log
 - [x] Arguments & validation
 - [x] **82** Built-in functions / operators:
    - [x] 3 Miscellaneous conversion
    - [x] 10 Numeric
    - [x] 1 Boolean
    - [x] 12 String
    - [x] 6 Date and Time conversion
    - [x] 3 Identity and Equality
    - [x] 4 Temporal
    - [x] 2 Duration
    - [x] 22 List
    - [x] 14 Range
    - [x] 2 Context
    - [x] 1 Sort
    - [x] 2 Operators
 - [x] 29 Special Properties of six data types (Date, Time, Duration, Range)
 - [x] LatticeType - To encapsulate all the type logic
 - [ ] User Defined Functions
    - [x] ParameterBinding 

## Execution Logging

Do not want to pass a logger object around to every function, so I need a way to store a logger with each thread and free it when the thread exits. The logger will be available to every function without needing to pass it in as an argument. 

 - [x] A thread-based contextual logger

## Execution Engine

The `Interpreter` class will execute the `OpCode`s and return a result.

 - [ ] FeelRuntime struct: heap stack (literals like strings), data stack, code stack (opcodes), contexts stack, iterations (IterationContext)
 - [ ] FeelReference enum
 - [x] `Opcode` enum
 - [x] `Interpreter` struct
 - [ ] `IterationContext` (for loops)
 - [x] if - then - else
 - [ ] Create a `Context` holding all the Builtins as keyed references to `FeelValue::Function`
 - [ ] list access (negative indices are from the end)
 - [ ] filter expressions (using the special variable "item" in lists, or field names without "item" in contexts)
 - [ ] context selection (using dot name after a list of contexts yields a list of values of the name property taken from all the contexts)
 - [ ] quantified expressions (some name in expression satisfies expression, every name in expression satisfies expression),
 - [ ] expression between expression and expression
 - [ ] expression in positive unary test
 - [ ] expression in ( positive unary tests )
 - [ ] instance of expression
 - [ ] function definition
 - [ ] Function calls
 - [ ] context key lookup
 - [ ] automatic conversions (§ 10.3.2.9.4) to and from singleton lists.
 - [ ] Serialization (Send/Receive)
 - [ ] Convert FeelOpcode to FeelValue
 - [ ] Execute method

## Simple Expression Tree Walk

 - [ ] Additive
 - [ ] Multiplicative
 - [ ] Relational
 - [ ] Logical
 - [ ] Conditional

## Properties special to each type

Must add a `get_property` method to FeelValue that can return these property values (or null, if undefined).

These 29 properties are listed in Table 65 and Table 66 of §10.3.2.15 of the DMN version 1.3 spec.

  - [x] **date**: year, month, day, weekday
  - [x] **date and time**: year, month, day, weekday, 
    hour, minute, second, time offset, timezone
  - [x] **time**: hour, minute, second, time offset, timezone
  - [x] **years and months duration**: years, months
  - [x] **days and time duration**: days, hours, minutes, seconds
  - [x] **range**: start, end, start included, end included

## Complex Expressions

 - [ ] Assembling Lists
 - [ ] Assembling Contexts
 - [ ] Assembling Ranges
 - [ ] Sorting
 - [ ] Filtering
 - [ ] Rule Chaining
 - [ ] Defining Functions
 - [ ] Calling Functions
 - [ ] Iteration contexts (lists, for loops)

## Caching and other Performance Enhancements

 - [ ] LRU Rule Cache
 - [ ] Redis Cache
 - [ ] Memoization
 - [ ] Regex Cache
 - [ ] `FeelValue::NumberList` - to reduce boxing/unboxing overhead make a new variant of FeelValue similar to FeelValue::List.
       Instead of holding Boxed FeelValue::Numbers in the Vec, just hold raw f64 values.
       Add branches for all the Builtins that can handle this new variant more efficiently.
 - [ ] Detect if the Sort `precedes` function is a simple < or > operator comparison of the two values. 
       If it is, sort using the Ord implementation.  

## Service

 - [ ] REST Service
 - [ ] Create Rule
 - [ ] Read Rule
 - [ ] Execute Rule
 - [ ] Delete Rule
 - [ ] Create State Machine
 - [ ] Read State Machine Definition
 - [ ] Read State Machine Current State
 - [ ] Post Event to State Machine
 - [ ] Delete State Machine
 - [ ] Support Rule Consequents in Create Rule

## Consequents

 - [ ] Asynchronous REST API Calls
 - [ ] Synchronous REST API Calls

## State Machines

 - [ ] Guards
 - [ ] Transitions
 - [ ] States
 - [ ] Push/Pop Stack
 - [ ] Error State

## List of all builtin functions:

This is an exhaustive (I hope) list of the builtin functions
defined in the spec. I count seventy-eight built-ins and two operators that must be implemented as `FeelFunction`s.

**Any**:

Three miscellaneous conversion functions.

  - [x] **type(anything)** -> `String`: Get the name of the data type of the object
  - [x] **string(value)** -> `String`: Convert to a string any value other than null
  - [x] **number(from, grouping separator, decimal separator)** -> `Number`: Convert into a number a string which has a thousands separator and a decimal character specified.

**Numbers**:

Ten numeric functions.

  - [x] **decimal(number, places)** -> `Number`
  - [x] **floor(number)** -> `Number`
  - [x] **ceiling(number)** -> `Number`
  - [x] **abs(number)** -> `Number`
  - [x] **modulo(dividend,divisor)** -> `Number`
  - [x] **sqrt(number)** -> `Number`
  - [x] **log(number)** -> `Number`
  - [x] **exp(number)** -> `Number`
  - [x] **odd(number)** -> `Boolean`
  - [x] **even(number)** -> `Boolean`

**Booleans**:

  - [x] **not(anything)** -> bool or null: If given a non-boolean, returns null.

**Strings**:

Twelve string functions.

  - [x] **substring(string, start position, length?)** -> `String`: Return a portion of the input string, where start position may be negative to count from the end of the string and length is optional, meaning to include all remaining characters.
  - [x] **string length(string)** -> `Number`: return the number of characters in the string
  - [x] **upper case(string)** -> `String`
  - [x] **lower case(string)** -> `String`
  - [x] **substring before(string, match)** -> `String`: Return all the string that comes before the match, or an empty string if no match.
  - [x] **substring after(string, match)** -> `String`: Return all the string that comes after the match, or an empty string if no match.
  - [x] **replace(input, pattern, replacement, flags?)** -> `String`: Regular expression pattern matching and replacement with optional flags.
  - [x] **contains(string, match)** -> `Boolean`: Does the string contain the match?
  - [x] **starts with(string, match)** -> `Boolean`: Does the string start with the match?
  - [x] **ends with(string, match)** -> `Boolean`: Does the string end with the match?
  - [x] **matches(input, pattern, flags?)** -> `Boolean`: Does the input match the regexp pattern?
  - [x] **split(string, delimiter)** -> `List`: Splits the string into a list of substrings, breaking at each occurrence of the delimiter pattern.

**Date, Time and Date and Time Conversion functions**:

Note that the `date` function and others have different use cases with different types and numbers of arguments. We will make a single builtin function for
each named builtin that interprets the data types and chooses the
appropriate semantics.

  - [x] **date(from)** -> `Date`: Convert into a date from a _string_ or a _date and time_
  - [x] **date(year, month, day)** -> `Date`: Convert into a date from its parts
  - [x] **date and time(from)** -> `DateAndTime`: Convert into a _date and time_ from a _string_
  - [x] **date and time(date, time)** -> `DateAndTime`: Convert into a _date and time_ from its parts
  - [x] **time(from)** -> `Time`: Convert into a _time_ from a _string_ or a _date and time_
  - [x] **time(hour, minute, second, offset?)** -> `Time`: Convert into a _time_ from parts, where offset is optional.

**Equality and Identity functions**

  - [x] **is(value1, value2)** -> `Boolean`
  - [x] **is_not(value1, value2)** -> `Boolean`
  - [x] **equals(value1, value2)** -> `Boolean`: Not a Builtin in the spec, but used to implement = operator

**Temporal Built-in functions**

Four temporal functions (in addition to the conversion functions).

  - [x] **day of year(date or date and time)** -> `Number`
  - [x] **day of week(date or date and time)** -> `String`
  - [x] **month of year(date or date and time)** -> `String`
  - [x] **week of year(date or date and time)** -> `Number`

**Durations**:

  - [x] **duration(duration_string)** -> `Any`: Convert to either a _days and time_ or _years and months_ duration a _string_.
  - [x] **years and months duration(from, to)** -> `YearsAndMonthsDuration`: Convert to a _years and months duration_ the difference between `from` and `to`, both of which must be of the matching types, either _date_ or _date and time_.

**Lists**:

Twenty-two list functions. Some of the functions can take either a list or a variable number of arguments (all numbers) that will be treated as if it were a list. These functions will be noted as (list or varargs).

  - [x] **list contains(list, element)** -> `Boolean`: Does the list contain the element? Can even find nulls.
  - [x] **count(list)** -> `Number`: return size of list, or zero if list is empty
  - [x] **min(list or varargs)** -> `Any`
  - [x] **max(list or varargs)** -> `Any`
  - [x] **sum(list or varargs)** -> `Number`
  - [x] **mean(list or varargs)** -> `Number`
  - [x] **all(list or varargs)** -> `Boolean`
  - [x] **any(list or varargs)** -> `Boolean`
  - [x] **sublist(list, start position, length?)** -> `List`
  - [x] **append(list, item...)** -> `List`: Append one or more items to the list, returning a new list.
  - [x] **concatenate(list...)** -> `List`: Concatenate one or more lists to form a new list.
  - [x] **insert before(list, position, newItem)** -> `List`
  - [x] **remove(list, position)** -> `List`
  - [x] **reverse(list)** -> `List`
  - [x] **index of(list, match)** -> `Number`
  - [x] **union(list...)** -> `List`: concatenate with duplicate removal.
  - [x] **distinct values(list)** -> `List`: Duplicate removal.
  - [x] **flatten(list)** -> `Any`: Flatten nested lists.
  - [x] **product(list or varargs)** -> `Number`: Returns the product of the numbers.
  - [x] **median(list or varargs)** -> `Any`
  - [x] **stddev(list or varargs)** -> `Number`
  - [x] **mode(list or varargs)** -> `Any`

**Ranges**:

Fourteen range functions, most with multiple cases involving points and ranges.
Refer to Table 78 in the DMN version 1.3 spec for the semantics.
The functions marked as "four ways" can take any of these argument lists:

  - point1, point2
  - point, range
  - range, point
  - range1, range2

Here are the functions:

  - [x] **before(four ways)** -> `Boolean`
  - [x] **after(four ways)** -> `Boolean`
  - [x] **meets(range1, range2)** -> `Boolean`
  - [x] **met by(range1, range2)** -> `Boolean`
  - [x] **overlaps(range1, range2)** -> `Boolean`
  - [x] **overlaps before(range1, range2)** -> `Boolean`
  - [x] **overlaps after(range1, range2)** -> `Boolean`
  - [x] **finishes(point, range) or (range1, range2)** -> `Boolean`
  - [x] **finished by(range, point) or (range1, range2)** -> `Boolean`
  - [x] **includes(range, point) or (range1, range2)** -> `Boolean`
  - [x] **during(point, range) or (range1, range2)** -> `Boolean`
  - [x] **starts(point, range) or (range1, range2)** -> `Boolean`
  - [x] **started by(range, point) or (range1, range2)** -> `Boolean`
  - [x] **coincides(point1, point2) or (range1, range2)** -> `Boolean`

The spec calls the points "scalars". That certainly means numbers, but we will take it to also refer to dates and date and times.

**Contexts**

  - [x] **get value(context, key)** -> `Any`
  - [x] **get entries(context)** -> `List`

**Sort**

  - [x] **sort(list, precedes)** -> `List`: The precedes function defines the ordering.

**Operators not supported by Rust**:

Feel has operators not supported by Rust, so they will be implemented as 
FeelFunctions.

  - [x] **power(number, exponent)** -> `Number`
  - [x] **instance of(value, type)** -> `Boolean`




