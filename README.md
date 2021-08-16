# Decision Model and Notation Feel Language Interpreter

Parses and executes expressions in the FEEL Language, as defined by the OMG: [Decision Model and Notation](https://www.omg.org/dmn/)

FEEL is the "Friendly Enough Expression Language".

The goal of this project is to:

 - Parse FEEL expressions that conform to version 1.3 of the DMN Specification.
 - Execute FEEL expressions and return the result.
 - Support all FEEL data types.
 - Support all special properties (such as the date and time properties).
 - Support all FEEL Built-in functions. (Approximately 80 functions).
 - Provide a FEEL library for embedded use in Rust applications.
 - Provide a REST API service with rule caching to serve as a simple Rules Engine.
 - Enable storage of rules in Redis.
 - Support simple rule chaining, where one rule may execute another.
 - Support State Machines, where rules are handed the starting state and create and return the resulting state.
 - Support Decision Tables
 
 The current state of progress:

 - All FEEL data types are implemented.
 - Date and DateTime data types **do not** support Time zones.
 - All standard library functions have been implemented.
 - Contexts and nested contexts are functional.
 - The syntax recognizer (built using the PEST parser generator, with a PEG grammar) can recognize most features. It is based on a grammar for an earlier version of FEEL, hence is missing a few features, like time and date literals. These will be added eventually.
 - Most of the OpCodes for the Virtual machine are implemented.
 - Code generation macros have been written for **nested-for loops** over ranges and lists.
 - Code generation macros **have not** been written for "**every**" and "**some**" list folding statements.
 - Code generation for user defined functions **has not** been written.
 - Compiled expressions may be executed on the VM.
 - **The parser is not yet hooked up to the code generator**. This is the largest unfinished piece.

