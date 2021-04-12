# Implementation Plan

## Grammar Recognizer

 - [ ] Add support for Duration literals like @"PT5H"

## FeelValue

 - [x] Enum Defined
 - [x] Equality
 - [x] Add, Sub, Mul, Div
 - [x] Nested Context
 - [x] Inequality Operators
 - [x] Range
    - [x] Range includes
    - [x] Range added to FeelValue
    - [!] 14 HL7 CQL Range functions => implemented in Builtins instead.
 - [x] FeelFunction
 - [x] FeelFunction added to FeelValue
 - [x] Execution Log
 - [x] Arguments & validation
 - [ ] **80** Built-in functions / operators:
    - [ ] 3 Miscellaneous conversion
    - [x] 10 Numeric
    - [x] 1 Boolean
    - [ ] 12 String
    - [ ] 6 Date and Time conversion
    - [ ] 1 Date and time
    - [ ] 4 Temporal
    - [ ] 2 Duration
    - [ ] 22 List
    - [x] 14 Range
    - [ ] 2 Context
    - [ ] 1 Sort
    - [ ] 2 Operators
 - [ ] 29 Special Properties of six data types
 - [ ] User Defined Functions
    - [x] ParameterBinding 

## Execution Logging

Do not want to pass a logger object around to every function, so I need a way to store a logger with each thread and free it when the thread exits. The logger will be available to every function without needing to pass it in as an argument. 

 - [ ] A thread-based contextual logger
## Simple Expression Tree Walk

 - [ ] Additive
 - [ ] Multiplicative
 - [ ] Relational
 - [ ] Logical
 - [ ] Conditional

## Properties special to each type

Must add a `get_property` method to FeelValue that can return these property values (or null, if undefined).

These properties are listed in Table 65 and Table 66 of §10.3.2.15 of the DMN version 1.3 spec.

  - [ ] **date**: year, month, day, weekday
  - [ ] **date and time**: year, month, day, weekday, 
    hour, minute, second, time offset, timezone
  - [ ] **time**: hour, minute, second, time offset, timezone
  - [ ] **years and months duration**: years, months
  - [ ] **days and time duration**: days, hours, minutes, seconds
  - [ ] **range**: start, end, start included, end included

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

## Caching

 - [ ] LRU Rule Cache
 - [ ] Redis Cache
 - [ ] Memoization

## Service


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

Three miscellaneous functions.

  - [ ] **type(anything)**: Get the name of the data type of the object
  - [ ] **string(value)**: Convert to a string any value other than null
  - [ ] **number(from, grouping separator, decimal separator)**: Convert into a number a string which has a thousands separator and a decimal character specified.

**Numbers**:

Ten numeric functions.

  - [x] **decimal(number, places)**
  - [x] **floor(number)**
  - [x] **ceiling(number)**
  - [x] **abs(number)**
  - [x] **modulo(dividend,divisor)**
  - [x] **sqrt(number)**
  - [x] **log(number)**
  - [x] **exp(number)**
  - [x] **odd(number)**
  - [x] **even(number)**

**Booleans**:

  - [x] **not(anything)** -> bool or null: If given a non-boolean, returns null.

**Strings**:

Twelve string functions.

  - [x] **substring(string, start position, length?)**: Return a portion of the input string, where start position may be negative to count from the end of the string and length is optional, meaning to include all remaining characters.
  - [x] **string length(string)**: return the number of characters in the string
  - [x] **upper case(string)**
  - [x] **lower case(string)**
  - [x] **substring before(string, match)**: Return all the string that comes before the match, or an empty string if no match.
  - [x] **substring after(string, match)**: Return all the string that comes after the match, or an empty string if no match.
  - [ ] **replace(input, pattern, replacement, flags?)**: Regular expression pattern matching and replacement with optional flags.
  - [x] **contains(string, match)**: Does the string contain the match?
  - [x] **starts with(string, match)**: Does the string start with the match?
  - [x] **ends with(string, match)**: Does the string end with the match?
  - [ ] **matches(input, pattern, flags?)**: Does the input match the regexp pattern?
  - [ ] **split(string, delimiter)**: Splits the string into a list of substrings, breaking at each occurrence of the delimiter pattern.

**Date, Time and Date and Time Conversion functions**:

Note that the `date` function and others have different use cases with different types and numbers of arguments. We will make a single builtin function for
each named buuiltin that interprets the data types and chooses the
appropriate semantics.

  - [ ] **date(from)**: Convert into a date from a _string_ or a _date and time_
  - [ ] **date(year, month, day)**: Convert into a date from its parts
  - [ ] **date and time(from)**: Convert into a _date and time_ from a _string_
  - [ ] **date and time(date, time)**: Convert into a _date and time_ from its parts
  - [ ] **time(from)**: Convert into a _time_ from a _string_ or a _date and time_
  - [ ] **time(hour, minute, second, offset?)**: Convert into a _time_ from parts, where offset is optional.

**Date and time functions**

  - [ ] **is(value1, value2)**

**Temporal Built-in functions**

Four temporal functions (in addition to the conversion functions).

  - [ ] **day of year(date or date and time)**
  - [ ] **day of week(date or date and time)**
  - [ ] **month of year(date or date and time)**
  - [ ] **week of year(date or date and time)**

**Durations**:

  - [ ] **duration(duration_string)**: Convert to either a _days and time_ or _years and months_ duration a _string_.
  - [ ] **years and months duration(from, to)**: Convert to a _years and months duration_ the difference between `from` and `to`, both of which must be of the matching types, either _date_ or _date and time_.

**Lists**:

Twenty-two list functions. Some of the functions can take either a list or a variable number of arguments (all numbers) that will be treated as if it were a list. These functions will be noted as (list or varargs).

  - [ ] **list contains(list, element)**: Does the list contain the element? Can even find nulls.
  - [ ] **count(list)**: return size of list, or zero if list is empty
  - [ ] **min(list or varargs)**
  - [ ] **max(list or varargs)**
  - [ ] **sum(list or varargs)**
  - [ ] **mean(list or varargs)**
  - [ ] **all(list or varargs)**
  - [ ] **any(list or varargs)**
  - [ ] **sublist(ist, start position, length?)**
  - [ ] **append(list, item...)**: Append one or more items to the list, returning a new list.
  - [ ] **concatenate(list...)**: Concatenate one or more lists to form a new list.
  - [ ] **insert before(list, position, newItem)**
  - [ ] **remove(list, position)**
  - [ ] **reverse(list)**
  - [ ] **index of(list, match)**
  - [ ] **union(list...)**: concatenate with duplicate removal.
  - [ ] **distinct values(list)**: Duplicate removal.
  - [ ] **flatten(list)**: Flatten nested lists.
  - [ ] **product(list or varargs)**: Returns the product of the numbers.
  - [ ] **median(list or varargs)**
  - [ ] **stddev(list or varargs)**
  - [ ] **mode(list or varargs)**

**Ranges**:

Fourteen range functions, most with multiple cases involving points and ranges.
Refer to Table 78 in the DMN version 1.3 spec for the semantics.
The functions marked as "four ways" can take any of these argument lists:

  - point1, point2
  - point, range
  - range, point
  - range1, range2

Here are the functions:

  - [x] **before(four ways)**
  - [x] **after(four ways)**
  - [x] **meets(range1, range2)**
  - [x] **met by(range1, range2)**
  - [x] **overlaps(range1, range2)**
  - [x] **overlaps before(range1, range2)**
  - [x] **overlaps after(range1, range2)**
  - [x] **finishes(point, range) or (range1, range2)**
  - [x] **finished by(range, point) or (range1, range2)**
  - [x] **includes(range, point) or (range1, range2)**
  - [x] **during(point, range) or (range1, range2)**
  - [x] **starts(point, range) or (range1, range2)**
  - [x] **started by(range, point) or (range1, range2)**
  - [x] **coincides(point1, point2) or (range1, range2)**

The spec calls the points "scalars". That certainly means numbers, but we will take it to also refer to dates and date and times.

**Contexts**

  - [ ] **get value(context, key)**
  - [ ] **get entries(context)** -> List

**Sort**

  - [ ] **sort(list, precedes)**: The precedes function defines the ordering.

**Operators not supported by Rust**:

Feel has operators not supported by Rust, so they will be implemented as 
FeelFunctions.

  - [ ] **power(number, exponent)** -> `Number`
  - [ ] **instance of(value, type)** -> `Boolean`




