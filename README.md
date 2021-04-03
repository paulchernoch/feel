# Decision Model and Notation Feel Language Interpreter

Parses and executes expressions in the FEEL Language.

FEEL is the "Friendly Enough Expression Language".

The goal of this project is to:

 - Parse FEEL expressions that conform to version 1.3 of the DMN Specification.
 - Execute FEEL expressions and return the result.
 - Support all FEEL data types.
 - Support all special properties (such as the date and time properties).
 - Support all FEEL Built-in functions. (Approximately 78 functions).
 - Provide a FEEL library for embedded use in Rust applications.
 - Provide a REST API service with rule caching to serve as a simple Rules Engine.
 - Enable storage of rules in Redis.
 - Support simple rule chaining, where one rule may execute another.
 - Support State Machines, where rules are handed the starting state and create and return the resulting state.
 
 

