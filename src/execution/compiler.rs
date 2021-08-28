use super::compiled_expression::CompiledExpression;
use pest::iterators::{Pairs,Pair};
use crate::pest::Parser;
use crate::parsing::duration_parser::parse_duration;
use crate::parsing::duration::{DurationVariety};
use crate::parsing::feel_parser::{Rule,FeelParser,show_pair_tree};
use crate::parsing::feel_value::FeelValue;

/// Compile a string into a CompiledExpression by walking the AST created by the Pest parser. 
pub struct Compiler<'a> {
  pub save_parse_tree: bool,
  pub parse_tree: Option<Pairs<'a, Rule>>
}

impl<'a> Compiler<'a> {
    pub fn new() -> Self {
        Compiler {
            save_parse_tree: false,
            parse_tree: None
        }
    }

    /// Dump the parse tree to the console, for debugging.
    pub fn dump(&self) {
        match &self.parse_tree {
            Some(tree) => {
                show_pair_tree(tree.clone(), 0);
            },
            None => {
                println!("No Parse Tree");
            }
        }
    }

    /// Compile the source expression into a Pest parse tree, then 
    /// walk the tree to generate OpCodes for the VM.
    pub fn compile(&mut self, source_expression: &'a str) -> Result<CompiledExpression, String> {
        match FeelParser::parse(Rule::start, source_expression) {
            Ok(pairs) => {
                if self.save_parse_tree {
                    self.parse_tree = Some(pairs.clone());
                }
                let mut expr = CompiledExpression::new(&source_expression.to_owned());
                match self.pair_list(pairs).as_slice() {
                    [root_node] => {
                        match self.walk_tree(root_node, &mut expr) {
                            Ok(()) => {
                                expr.resolve_jumps();
                                Ok(expr)
                            },
                            Err(message) => {
                                Result::Err(format!("Failed to compile {}. Error: {}", source_expression, message))
                            }
                        }
                    },
                    [] => Result::Err(format!("Parse tree empty. Failed to compile {}", source_expression)),
                    _ => Result::Err(format!("Parse tree has more than one root. Failed to correctly compile {}", source_expression))
                }
            },
            Err(e) => {
                Result::Err(format!("Could not parse expression. {:?}", e))
            }
        }
    }

    /// Decide if the compiler should go down another level without generating any OpCodes at this level. 
    /// Many intermediate rules are merely used to handle operator precedence climbing and do not cause code to be generated. 
    fn should_descend(left_rule: Rule, right_rule: Rule) -> bool {
        match (left_rule, right_rule) {
            (Rule::expression, _) => true, 
            (Rule::textual_expression, _) => true, 
            (Rule::simple_expression, _) => true, 
            (Rule::parenthesized_expression, _) => true, 
            (Rule::literal, _) => true, 
            (Rule::simple_literal, _) => true, 
            (Rule::simple_positive_unary_test, Rule::interval) => true, 
            (Rule::simple_value, _) => true, 
            (Rule::endpoint, _) => true, 
            (Rule::positive_unary_test, Rule::simple_positive_unary_test) => true, 
            (Rule::left_expa, _) => true, 
            (Rule::left_expb, _) => true, 
            (Rule::left_expc, _) => true, 
            (Rule::left_expd, _) => true, 
            (Rule::left_expe, _) => true, 
            (Rule::left_expf, _) => true, 
            (Rule::left_expg, _) => true, 
            (Rule::left_exph, _) => true, 
            (Rule::txt_expa, _) => true, 
            (Rule::txt_expb, _) => true, 
            (Rule::txt_expc, _) => true, 
            (Rule::txt_expd, _) => true, 
            (Rule::txt_expe, _) => true, 
            (Rule::txt_expf, _) => true, 
            (Rule::txt_expg, _) => true, 
            (Rule::txt_exph, _) => true, 
            (Rule::txt_expi, _) => true, 
            (Rule::arithmetic_expression, _) => true, 
            (Rule::unary_expression, _) => true, 
            (Rule::boxed_expression, _) => true, 
            (Rule::reserved_word, _) => true, 
            _ => false
         }
    }

    /// Walk the parse tree recursively and insert OpCodes in the given expression
    /// to perform the relevant computations.
    fn walk_tree(&mut self, pair: &Pair<'a, Rule>, expr: &mut CompiledExpression) -> Result<(), String> {
        // TODO: The following Rules are not yet handled: 
        //       - simple_unary_tests (needed for Decision Tables?)
        //       - unary_tests (needed for Decision Tables?)
        //       - named_parameters, named_parameter
        //       - instance_of
        //       - function_definition, function_body and related
        //       - for_expression, in_expressions, in_expression and related
        //       - quantified_expression (every and some loops)
        //       - context, key, context_entry, context_entries
        //       - date_time_literal, date_time_keyword
        //  May need to add new builtins for date time keywords: 
        //       - date and time - has builtin
        //       - time - has builtin
        //       - date - has builtin
        //       - duration - has builtin
        //       - years and months duration - has builtin
        //       - days and time duration - Spec Sec 10.3.2.3.7 says use the duration function?
    

        let rule = pair.as_rule();
        let children = self.children(pair);
        // The match arms correspond loosely to the grammar rules. 
        // If a grammar rule does nothing but yield a lefthand token from a single righthand one,
        // recurse without doing anything.
        match self.get_production_rule(pair).as_slice() {
            [Rule::start, _] => self.walk_tree(&Compiler::first_child(pair), expr),
            [left, right] if Compiler::should_descend(*left, *right) => self.walk_tree(&Compiler::first_child(pair), expr),
            [Rule::exponentiation, Rule::unary_expression, ..] => self.infix_to_postfix(children, expr),
            [Rule::multiplicative, Rule::exponentiation, ..] => self.infix_to_postfix(children, expr),
            [Rule::additive, Rule::multiplicative, ..] => self.infix_to_postfix(children, expr),
            [Rule::comparision, _, Rule::comparision_operator, .. ] => self.infix_to_postfix(children, expr),
            [Rule::comparision, _, Rule::between_token, _, Rule::and_token, _] => {
                match children.as_slice() {
                    [item, _, lower, _, upper] => {
                        let r1 = self.walk_tree(item, expr);
                        let r2 = self.walk_tree(lower, expr);
                        let r3 = self.walk_tree(upper, expr);
                        expr.append_str("between");
                        if r1.is_err() || r2.is_err() || r3.is_err() {
                            Err(format!("Error parsing BETWEEN expression {}.", pair.as_str()))
                        }
                        else {
                            Ok(())
                        }
                    },
                    _ => Err(format!("Incorrect number of arguments to BETWEEN expression {}.", pair.as_str()))
                }
            },

            // Case of a single positive unary test
            [Rule::comparision, _, Rule::in_token, Rule::positive_unary_test] => {
                match children.as_slice() {
                    [item, _, unary_test] => {
                        let r1 = self.walk_tree(item, expr);
                        let r2 = self.walk_tree(unary_test, expr);
                        expr.append_str("in");
                        match (r1, r2) {
                            (Ok(_), Ok(_)) => Ok(()),
                            (Err(item_error), Ok(_)) => Err(format!("Error parsing item for IN expression {}: {}", pair.as_str(), item_error)),
                            (Ok(_), Err(unary_error)) => Err(format!("Error parsing unary test for IN expression {}: {}", pair.as_str(), unary_error)),
                            (Err(item_error), Err(unary_error)) => Err(format!("Error parsing item and unary test IN expression {}:\n  Item -> {}\n  Test -> {}", pair.as_str(), item_error, unary_error))
                        }
                    },
                    _ => Err(format!("Incorrect number of arguments to IN expression {}.", pair.as_str()))
                }
            },
            // Case of a list of positive unary tests
            [Rule::comparision, _, Rule::in_token, Rule::positive_unary_tests] => {
                match children.as_slice() {
                    [item, _, unary_tests] => {
                        let r1 = self.walk_tree(item, expr);
                        if let Err(item_error) = r1 {
                            return Err(format!("Error parsing item for IN expression {}: {}", pair.as_str(), item_error));
                        }
                        expr.append_str("list");
                        let r2 = self.walk_list(unary_tests, expr, Some("push".to_owned()));
                        if let Err(tests_error) = r2 {
                            return Err(format!("Error parsing unary tests for IN expression {}: {}", pair.as_str(), tests_error));
                        }
                        expr.append_str("in");
                        Ok(())
                    },
                    _ => {
                        Err(format!("Incorrect number of arguments to IN expression {}.", pair.as_str()))
                    }
                }
            },
           
            [Rule::conjunction, ..] => self.infix_to_postfix(children, expr),
            [Rule::disjunction, ..] => self.infix_to_postfix(children, expr),
            [Rule::arithmetic_negation, _] => match self.walk_tree(&Compiler::first_child(pair), expr) {
                Ok(()) => {
                    expr.append_str("neg");
                    Ok(())
                },
                Err(message) => Err(message)
            },      
            
            // Generate Literals or Lists or follow path expressions

            [Rule::numeric_literal] => {
                expr.append_str(&format!("num({})", pair.as_str()));
                Ok(())
            },         
            [Rule::boolean_literal, ..] => {
                expr.append_str(&format!("{}", pair.as_str().to_lowercase()));
                Ok(())
            },
            // Parses unicode and other escape sequences.
            [Rule::string_literal, ..] => {
                expr.append_load_string(&self.walk_string(pair));
                Ok(())
            },

            [Rule::null_literal, ..] => {
                expr.append_str("null");
                Ok(())
            },

            // A path expression has several parts separated by periods. 
            // Parts are normally names, but variations are supported.
            [Rule::path_expression, ..] => self.walk_path(pair, expr),

            // The grammar may be defective, as in an instance where I thought qualified_name
            // would be generated, instead we get path_expression.
            [Rule::qualified_name, ..] => self.walk_names(pair, expr),  

            // Names will be processed differently if they are part of a qualified_name as above. 
            // If encountered here, the self.contexts is the context that is searched. 
            [Rule::name, ..] => {
                expr.append_load_string(&self.walk_name_parts(pair));
                expr.append_str("name @");
                Ok(())
            },
            [Rule::list, Rule::list_entries] => {
                expr.append_str("list");
                self.walk_list(&Compiler::first_child(pair), expr, Some("push".to_owned()))
            },
            // An empty list
            [Rule::list] => {
                expr.append_str("list");
                Ok(())
            },

            // Ranges (aka intervals)

            [Rule::positive_unary_test, Rule::null_literal] => {
                expr.append_str("null");
                Ok(())
            },

            // Ranges made from unary operators, like "<=10" or ">5"
            [Rule::simple_positive_unary_test, Rule::unary_operator, _] => self.walk_open_range(pair, expr),

            // Ranges with brackets and "..", like [1..9] or [0..10)
            [Rule::interval, ..] => self.walk_interval(pair, expr),

            [Rule::function_invocation, _, Rule::positional_parameters] => self.walk_function(pair, expr),

            [Rule::function_invocation, _, Rule::named_parameters] => self.walk_function(pair, expr),

            // Date, time, duration literals as @-strings
            [Rule::date_time_literal, Rule::at_literal] => self.walk_date_time_literal_string(pair, expr),

            // Date, time, duration literals as funcion calls
            [Rule::date_time_literal, Rule::date_time_keyword, ..] => self.walk_date_time_literal_function(pair, expr),


            // A dash returns true for any value found at the top of the data stack. 
            [Rule::dash_unary_test, ..] => {
                expr.append_str("drop true");
                Ok(())
            },

            // Function with no parameters
            [Rule::function_invocation, _] => self.walk_function(pair, expr),

            // If-then-else expression
            [Rule::if_expression, Rule::if_token, Rule::expression, Rule::then_token, Rule::expression, Rule::else_token, Rule::expression] => {
                self.walk_if_then_else(pair, expr)
            },

            _ => Err(format!("Compilation not implemented for rule {:?}", rule))
        }
    }

    fn walk_list(&mut self, pair: &Pair<'a, Rule>, expr: &mut CompiledExpression, after_each: Option<String>) -> Result<(), String> {
        let list_entries_children = self.children(pair);
        for list_item in list_entries_children.iter() {
            // Why did I get the first child here???
            // match self.walk_tree(&Compiler::first_child(list_item), expr) {
            match self.walk_tree(list_item, expr) {
                Ok(()) => {
                    match after_each {
                        Some(ref s) =>  { expr.append_str(s); },
                        None => ()
                    };
                },
                Err(message) => { return Err(message); }
            };
        }
        Ok(())
    }

    /// Convert a unicode string to a character.
    /// The unicode_string may be either of these formats: 
    ///    - \uXXXX
    ///    - \UXXXXXX
    /// where X is a hexadecimal digit. 
    fn parse_unicode(unicode_string: &str) -> Option<char> {
        let number = &unicode_string[2..];
        u32::from_str_radix(number, 16)
            .ok()
            .and_then(std::char::from_u32)
    }

    /// Accepts a string_literal and processes its component characters to 
    /// create a string. This does not create any OpCodes! Caller decides
    /// what to do with the string.
    fn walk_string(&mut self, pair: &Pair<'a, Rule>) -> String {
        let double_string_characters = self.children(pair);
        let mut s_literal = String::new();
        for dsc in double_string_characters.iter() {
            let char_string = dsc.as_str();
            match char_string {
                "\\\"" => s_literal.push('"'),
                "\\\\" => s_literal.push('\\'),
                "\\b" => s_literal.push('\x08'), // Backspace
                "\\f" => s_literal.push('\x0C'), // Formfeed
                "\\n" => s_literal.push('\n'),
                "\\r" => s_literal.push('\r'),
                "\\t" => s_literal.push('\t'),
                "\\v" => s_literal.push('\x0B'), // Vertical formfeed
                unicode if char_string.len() >= 6 => {
                    // Assume first two characters are "\u" or "\U" followed by four or six hex digits.
                    match Compiler::parse_unicode(unicode) {
                        Some(uni_char) => s_literal.push(uni_char),
                        None => {
                            // TODO: Log error
                            s_literal.push('�') // U+FFFD
                        }
                    }
                },
                _ => s_literal.push_str(char_string)
            };
        }
        s_literal
    }

    /// Accepts a "qualified_name" pair and processes its names as a
    /// series of context lookups. 
    /// The first name will be used to retrieve a value (possibly a context)
    /// from contexts and push it onto the data stack. 
    /// All subsequent names will be used to lookup values (in chained fashion)
    /// from a series of intermediate contexts, until the final one likely produces a value. 
    fn walk_names(&mut self, pair: &Pair<'a, Rule>, expr: &mut CompiledExpression) -> Result<(), String> {
        let names = self.children(pair);
        let mut is_first_name = true;
        for name_pair in names.iter() {
            let name_string = self.walk_name_parts(name_pair);
            expr.append_load_string(&name_string);
            if is_first_name {
                expr.append_str("name @");
            }
            else {
                expr.append_str("name qget");
            }
            is_first_name = false;
        }
        Ok(())
    }

    /// A path expression has several parts separated by periods. 
    /// The parts are normally names, but variations are supported: 
    ///   - If the first part is a name, perform an @ lookup from contexts. 
    ///   - If the first part is not a name, assume it 
    ///     is an expression that yields a context. Perform no @ lookup.
    ///   - If a subsequent part is a name, perform a data stack qget lookup.
    ///   - If a subsequent part is not a name, assume it yields a string which will 
    ///     be used in place of a name for a data stack qget lookup. 
    fn walk_path(&mut self, pair: &Pair<'a, Rule>, expr: &mut CompiledExpression) -> Result<(), String> {
        let segments = self.children(pair);
        let mut is_first_path_segment = true;
        for segment in segments.iter() {
            let name_found = self.find_without_branching(segment, Some(Rule::name));
            if is_first_path_segment {
                match name_found {
                    Some(name_pair) => {
                        let name_string = self.walk_name_parts(&name_pair);
                        expr.append_load_string(&name_string);
                        expr.append_str("name @");
                    },
                    None => {
                        let result = self.walk_tree(segment, expr);
                        if result.is_err() { return result; }
                        // Do not append "name qget" - this should yield a context.
                    }
                };
            }
            else {
                match name_found {
                    Some(name_pair) => {
                        let name_string = self.walk_name_parts(&name_pair);
                        expr.append_load_string(&name_string);
                        expr.append_str("name qget");
                    },
                    None => {
                        let result = self.walk_tree(segment, expr);
                        if result.is_err() { return result; }
                        expr.append_str("name qget");
                    }
                };
            }
            is_first_path_segment = false;
        }
        Ok(())
    }

    /// Accepts a "name" pair and processes its component parts to 
    /// create a String that can later be parsed into a QName. 
    /// This does not create any OpCodes! Caller decides
    /// what to do with the QName.
    fn walk_name_parts(&mut self, pair: &Pair<'a, Rule>) -> String {
        let name_parts = self.children(pair);
        let mut name_list: Vec<String> = Vec::new();
        for name_part_pair in name_parts.iter() {
            name_list.push(name_part_pair.as_str().to_owned());
        }
        name_list.join(" ")
    }

    /// Recognize a simple_positive_unary_test and create an open-ended range for it. 
    ///   - < will become: [..limit)
    ///   - <= will become: [..limit]
    ///   - > will become: (limit..]
    ///   - >= will become: [limit..] 
    /// If the endpoint is a name, create a range that references a name instead of a value. 
    /// If the endpoint is a simple_literal, create a range that references a value. 
    /// If the endpoint is a qualified name, look up the value and then use it for the range.
    /// NOTE: This final case should create a range based on a qualified name, but
    /// that is not implemented. 
    fn walk_open_range(&mut self, pair: &Pair<'a, Rule>, expr: &mut CompiledExpression) -> Result<(), String> {
        let children = self.children(pair);
        let endpoint = &children[1];
        // See what kind of endpoint it is. If a name, make a Range that references a name,
        // otherwise a Range that references a literal. 
        match self.find_without_branching(endpoint, Some(Rule::name)) {
            Some(name_pair) => {
                let name_string = self.walk_name_parts(&name_pair);
                expr.append_load_string(&name_string);
                // Do not add "@" to fetch the value of the name. 
                // That will be done by the range when it is evaluated.
                expr.append_str("name");
            },
            None => {
                // Process the normal way
                let result = self.walk_tree(endpoint, expr);
                if result.is_err() { return result; }
            }
        }
        let range_op = match children[0].as_str() {
            "<=" => "[..,hi]",
            "<" => "[..,hi)",
            ">=" => "[lo,..]",
            ">" => "(lo,..]",
            _ => unreachable!()
        };
        expr.append_str(range_op);
        Ok(())
    }

    /// Assume that the pair is for an interval and add commands to
    /// create a Range from its four children.
    /// The Range's interval can be open or closed at either end. 
    fn walk_interval(&mut self, pair: &Pair<'a, Rule>, expr: &mut CompiledExpression) -> Result<(), String> {
        let children = self.children(pair);
        match children.as_slice() {
            [interval_start, low_endpoint, high_endpoint, interval_end] => {
                // We create instructions for the endpoints first, then append the range op. 
                let r1 = self.walk_tree(low_endpoint, expr);
                let r2 = self.walk_tree(high_endpoint, expr);
                
                let is_open_interval_start = self.find_without_branching(interval_start, Some(Rule::open_interval_start)).is_some();
                let is_open_interval_end = self.find_without_branching(interval_end, Some(Rule::open_interval_end)).is_some();
                let range_op = match (is_open_interval_start, is_open_interval_end) {
                    (true, true) => "(lo,hi)",
                    (true, false) => "(lo,hi]",
                    (false,true) => "[lo,hi)",
                    (false,false) => "[lo,hi]"
                };
                expr.append_str(range_op);

                match (r1, r2) {
                    (Ok(_), Ok(_)) => Ok(()),
                    (Err(low_error), Ok(_)) => Err(format!("Error parsing low endpoint for interval expression {}: {}", pair.as_str(), low_error)),
                    (Ok(_), Err(high_error)) => Err(format!("Error parsing high endpoint for interval expression {}: {}", pair.as_str(), high_error)),
                    (Err(low_error), Err(high_error)) => Err(format!("Error parsing endpoints for inteval expression {}:\n  Low -> {}\n  High -> {}", pair.as_str(), low_error, high_error))
                }
            },
            _ => Err(format!("Incorrect number of arguments to interval expression {}.", pair.as_str()))
        }
    }

    /// Create a date, time, date and time, or duration literal by calling a builtin function. 
    /// The name of the function usually matches the keyword used in the rule.
    fn walk_date_time_literal_function(&mut self, pair: &Pair<'a, Rule>, expr: &mut CompiledExpression) -> Result<(), String> {
        for child in self.children(pair).iter() {
            if child.as_rule() == Rule::date_time_keyword {
                let keyword = child.as_str();
                // Spec curiously does not call for a function named "days and time duration",
                // but duration can create either kind (I think).
                let function_name = match keyword {
                    "days and time duration" => "duration",
                    _ => keyword
                };
                expr.append_str(&format!("'{}' name list", function_name));
            }
            else {
                if let Err(message) = self.walk_tree(child, expr) {
                    return Err(format!("Unable to compile argument to date and time function: {}. {}", pair.as_str(), message));
                }
                expr.append_str("push");
            }
        }
        expr.append_str("call");
        Ok(())
    }

    /// Parses an @-string into a date, time or duration
    fn walk_date_time_literal_string(&mut self, pair: &Pair<'a, Rule>, expr: &mut CompiledExpression) -> Result<(), String> {
        // Note the stripping of the the enclosing @ sign and quotes. 
        let s = Compiler::first_child(pair).as_str();
        let literal_string = s[2..s.len()-1].to_owned();
        if literal_string.starts_with("-P") || literal_string.starts_with("P") {
            // Duration Literal
            if parse_duration(&literal_string, DurationVariety::Full).is_err() {
                return Err(format!("Duration string improperly formatted: {}", literal_string))
            }
            expr.append_str(&format!("'duration' name list '{}' push call", literal_string));
            Ok(())
        }
        else {
            // Date, time, or date and time literal
            // Use the presence of dash and colon to discriminate. 
            let has_date = literal_string.contains("-");
            let has_time = literal_string.contains(":");
            match (has_date, has_time) {
                (true, true) => {
                    match FeelValue::new_date_and_time(&literal_string) {
                        Some(_dt) => {
                            expr.append_str(&format!("'date and time' name list '{}' push call", literal_string));
                            Ok(())
                        },
                        None => Err(format!("Date and time string improperly formatted: {}", literal_string))
                    }
                },
                (true, false) => {
                    match FeelValue::new_date(&literal_string) {
                        Some(_date) => {
                            expr.append_str(&format!("'date' name list '{}' push call", literal_string));
                            Ok(())
                        },
                        None => Err(format!("Date string improperly formatted: {}", literal_string))
                    }
                },
                (false, true) => {
                    match FeelValue::new_time(&literal_string) {
                        Some(_time) => {
                            expr.append_str(&format!("'time' name list '{}' push call", literal_string));
                            Ok(())
                        },
                        None => Err(format!("Time string improperly formatted: {}", literal_string))
                    }
                },
                (false, false) => Err(format!("Date or time string improperly formatted: {}", literal_string))
            }
        }
    }
    /// Append a function call to the expression, using parts parsed from the children of the 
    /// given pair. 
    fn walk_function(&mut self, pair: &Pair<'a, Rule>, expr: &mut CompiledExpression) -> Result<(), String> {
        let children = self.children(pair);
        match children.as_slice() {
            [function_or_name, positional_parameters] if positional_parameters.as_rule() == Rule::positional_parameters => {
                let r1 = self.walk_tree(function_or_name, expr);
                expr.append_str("list");
                let r2 = self.walk_list(positional_parameters, expr, Some("push".to_owned()));
                expr.append_str("call");

                match (r1, r2) {
                    (Ok(_), Ok(_)) => Ok(()),
                    (Err(name_error), Ok(_)) => Err(format!("Error parsing function name for function call {}: {}", pair.as_str(), name_error)),
                    (Ok(_), Err(args_error)) => Err(format!("Error parsing arguments for for function call {}: {}", pair.as_str(), args_error)),
                    (Err(name_error), Err(args_error)) => Err(format!("Error parsing function call {}:\n  Name -> {}\n  Args -> {}", pair.as_str(), name_error, args_error))
                }
            },
            [_function_or_name, named_parameters] if named_parameters.as_rule() == Rule::named_parameters => {
                Err(format!("Calling a function with named parameters is not yet implemented: {}.", pair.as_str()))
            },
            [function_or_name] => {
                // Calling a function with no parameters. 
                let r1 = self.walk_tree(function_or_name, expr);

                // Prepare an empty argument list and call. 
                expr.append_str("list call");
                match r1 {
                    Ok(_) => Ok(()),
                    Err(name_error) => Err(format!("Error calling function with no parameters {}: {}", pair.as_str(), name_error))
                }
            },
            _ => Err(format!("Incorrect number of arguments to function expression {}.", pair.as_str()))
        }
    }

    fn walk_if_then_else(&mut self, pair: &Pair<'a, Rule>, expr: &mut CompiledExpression) -> Result<(), String> {
        let children = self.children(pair);
        match children.as_slice() {
            [_, condition, _, true_result, _, false_result] => {
                let mut condition_expr = CompiledExpression::new("condition");
                let mut true_expr = CompiledExpression::new("if expression");
                let mut false_expr = CompiledExpression::new("else expression");

                let r1 = self.walk_tree(condition, &mut condition_expr);
                let r2 = self.walk_tree(true_result, &mut true_expr);
                let r3 = self.walk_tree(false_result, &mut false_expr);

                let mut if_else_expr = CompiledExpression::if_else(
                    &mut condition_expr, 
                    &mut true_expr,
                    &mut false_expr
                );
                expr.append(&mut if_else_expr);

                match (r1, r2, r3) {
                    (Ok(_), Ok(_), Ok(_)) => Ok(()),
                    (Err(condition_error), _, _) => Err(format!("Unable to parse condition of if-then-else expression {}. {}", pair.as_str(), condition_error)),
                    (_, Err(true_error), _) => Err(format!("Unable to parse true result expression of if-then-else expression {}. {}", pair.as_str(), true_error)),
                    (_, _, Err(false_error)) => Err(format!("Unable to parse false result expression of if-then-else expression {}. {}", pair.as_str(), false_error)),
                }
            },
            _ => Err(format!("Incorrect number of arguments to if-then-else expression {}.", pair.as_str()))
        }
    }

    fn pair_list(&self, pairs: Pairs<'a, Rule>) -> Vec<Pair<'a, Rule>> {
        let mut list = vec![];
        for pair in pairs {
            list.push(pair);
        }
        list
    }

    /// Create a Vec where the first element is the Rule for the given Pair
    /// and the remaining elements are the Rules for its children. 
    /// In a grammar production rule "a -> b c" this would yield [a,b,c]
    fn get_production_rule(&self, pair: &Pair<'a, Rule>) -> Vec<Rule> {
        let mut list = vec![pair.as_rule()];
        for child_pair in pair.clone().into_inner() {
            list.push(child_pair.as_rule());
        }
        list
    }

    fn first_child(pair: &Pair<'a, Rule>) -> Pair<'a, Rule> {
        pair.clone().into_inner().next().unwrap()
    }

    fn children(&self, pair: &Pair<'a, Rule>) -> Vec<Pair<'a, Rule>> {
        self.pair_list(pair.clone().into_inner())
    }

    /// Traverse the AST downwards until we find the bottom node (having no children)
    /// or find a node that has two or more children. This is used
    /// to skip over many levels of parse tree that do nothing except enforce 
    /// precedence climbing and the like. 
    /// 
    /// If match_rule is not None, stop at the first Pair that matches the given Rule. 
    /// If no match is found, return None.
    fn find_without_branching(&mut self, pair: &Pair<'a, Rule>, match_rule: Option<Rule>) -> Option<Pair<'a, Rule>> {
        if let Some(rule) = match_rule {
            if rule == pair.as_rule() {
                return Some(pair.clone());
            }
        }
        let children = self.pair_list(pair.clone().into_inner());
        match children.len() {
            0 => if match_rule.is_some() { None } else { Some(pair.clone()) },
            1 => self.find_without_branching(&children[0], match_rule),
            _ => if match_rule.is_some() { None } else { Some(pair.clone()) }
        }
    }


    /// Expected to have an odd number of children. 
    /// Every other child should be an operator. 
    /// Alter infix order to postfix. 
    /// Examples are: 
    ///    1 + 2 + 3
    ///    2 * 4 * 6 * 8
    ///    1 / 2
    fn infix_to_postfix(&mut self, children: Vec<Pair<'a, Rule>>, expr: &mut CompiledExpression) -> Result<(), String> {
        for i in (0..children.len()).step_by(2) {
            let result: Result<(), String>;
            if i == 0 {
                result = self.walk_tree(&children[0], expr);
                if result.is_err() { return result; }
            }
            else {
                result = self.walk_tree(&children[i], expr);
                if result.is_err() { return result; }
                let op_string = children[i - 1].as_str();
                match op_string {
                    "+" | "-" | "*" | "/" | "**" => { expr.append_str(op_string) },
                    "<" | "<=" | ">" | ">=" | "=" | "!=" => { expr.append_str(op_string) },
                    "and" | "or" => { expr.append_str(op_string) },
                    _ => {
                        return Err(format!("Expected operator, received {}", op_string))
                    }
                }
            }
        }
        Ok(())
    }
}



// ///////////////////////////////////// //
//                                       //
//               Tests                   //
//                                       //
// ///////////////////////////////////// //

#[cfg(test)]
mod tests {
  use std::str::FromStr;
  use chrono::{NaiveDate};
  use super::Compiler;
  use crate::parsing::execution_log::ExecutionLog;
  use super::super::interpreter::Interpreter;
  use crate::parsing::feel_value::{FeelValue};
  use crate::parsing::{nested_context::NestedContext};
  use crate::parsing::{context::Context};
  use crate::parsing::duration::Duration;

  /// Change to return true to see large diagnostics in several tests, false to not show it.
  fn print_diagnostics() -> bool {
    true
  }

  #[test]
  fn test_invalid_expression() {
      let mut compiler = Compiler::new();
      assert!(compiler.compile("this is not a valid expression!").is_err());
  }

  #[test]
  fn test_addition() {
      /*
      Parse tree for "1 + 2 + 3" looks like this: 

 start                           Text:    1 + 2 + 3
   textual_expression              Text:    1 + 2 + 3
     txt_expe                        Text:    1 + 2 + 3
       arithmetic_expression           Text:    1 + 2 + 3
         additive                        Text:    1 + 2 + 3
           multiplicative                  Text:    1
             exponentiation                  Text:    1
               unary_expression                Text:    1
                 left_expe                       Text:    1
                   left_expf                       Text:    1
                     left_expg                       Text:    1
                       left_exph                       Text:    1
                         txt_expi                        Text:    1
                           literal                         Text:    1
                             simple_literal                  Text:    1
                               numeric_literal                 Text:    1
           additive_operator               Text:    +
           multiplicative                  Text:    2
             exponentiation                  Text:    2
               unary_expression                Text:    2
                 left_expe                       Text:    2
                   left_expf                       Text:    2
                     left_expg                       Text:    2
                       left_exph                       Text:    2
                         txt_expi                        Text:    2
                           literal                         Text:    2
                             simple_literal                  Text:    2
                               numeric_literal                 Text:    2
           additive_operator               Text:    +
           multiplicative                  Text:    3
             exponentiation                  Text:    3
               unary_expression                Text:    3
                 left_expe                       Text:    3
                   left_expf                       Text:    3
                     left_expg                       Text:    3
                       left_exph                       Text:    3
                         txt_expi                        Text:    3
                           literal                         Text:    3
                             simple_literal                  Text:    3
                               numeric_literal                 Text:    3
      */
      compiler_test("1 + 2 + 3", FeelValue::Number(6.0));
  }

  #[test]
  fn test_string_literal_with_single_quotes() {
    compiler_test("'Hello world!'", "Hello world!".into());
    compiler_test(
        "'No escaping \\n happens in single quoted strings'", 
        "No escaping \\n happens in single quoted strings".into()
    );
  } 

  #[test]
  fn test_string_literal_with_double_quotes() {
    compiler_test("\"Hello world!\"", "Hello world!".into());
    compiler_test(
        "\"Escaping \\n happens in double quoted strings\"", 
        "Escaping \n happens in double quoted strings".into()
    );
  } 

  #[test]
  fn test_unicode_string_literal() {
    compiler_test(
        "\"\\u2661 \\u2661 me do, \\U01F411 know \\U01F441 \\u2661 \\U01F411\"", 
        FeelValue::String("♡ ♡ me do, 🐑 know 👁 ♡ 🐑".to_owned())
    );
  } 

  #[test]
  fn test_qualified_name() {
    compiler_test("hair color", "brown".into());
    compiler_test("outer level.inner level", "Sanctum Sanctorum".into());
  } 

  #[test]
  fn test_date_time_literal_function() {
    compiler_test_with_builtins("date(2001,9,11)", FeelValue::Date(NaiveDate::from_ymd(2001, 9, 11)));
  }

  #[test]
  fn test_date_time_literal_string() {
    compiler_test_with_builtins(
        "@'-P2Y6M'", 
        FeelValue::YearMonthDuration(Duration::from_str("-P2Y6M").unwrap())
    );
    compiler_test_with_builtins(
        "@'P1DT30M'", 
        FeelValue::DayTimeDuration(Duration::from_str("P1DT30M").unwrap())
    );

    compiler_test_with_builtins(
        "@'2020-02-14'", 
        FeelValue::new_date("2020-02-14").unwrap()
    );
    compiler_test_with_builtins(
        "@'08:30:00'", 
        FeelValue::new_time("08:30:00").unwrap()
    );
  }

  #[test]
  fn test_math() {
    compiler_test("10 + 2 * 30 / 15", FeelValue::Number(14.0));
    compiler_test("1 + 2 - 3 * 6", FeelValue::Number(-15.0));
    compiler_test("-(12 - 20)", FeelValue::Number(8.0)); // Unary negation
    compiler_test("4 ** 3 ** 2", 4096.0.into());
  }  

  #[test]
  fn test_relational() {
    compiler_test("10 < 20.5", FeelValue::Boolean(true));
    compiler_test("10 <= 20.5", FeelValue::Boolean(true));
    compiler_test("10 > 20.5", FeelValue::Boolean(false));
    compiler_test("10 = 20.5", FeelValue::Boolean(false));
    compiler_test("10 != 20.5", FeelValue::Boolean(true));
  }  

  #[test]
  fn test_relational_logical() {
    compiler_test("(1 + 2) < (3 * 4) and (5 + 6) != 12", FeelValue::Boolean(true));
  }  

  #[test]
  fn test_logical() {
    compiler_test("true and true", FeelValue::Boolean(true));
    compiler_test("true and false", FeelValue::Boolean(false));
    compiler_test("true or false", FeelValue::Boolean(true));
    compiler_test("false or false", FeelValue::Boolean(false));
  }   

  #[test]
  fn test_between() {
    compiler_test("5 between 1 + 3 and 10", true.into());
  }  

  /// in operator with a single unary test
  #[test]
  fn test_in_unary_test() {
    compiler_test("5 in <= 10", true.into());
    compiler_test("10 in < 10", false.into());
    compiler_test("1 in > 0", true.into());
    compiler_test("10 in >= 12", false.into());
  }  

  /// in operator with a List of multiple unary tests
  #[test]
  fn test_in_list_of_unary_tests() {
    compiler_test("5 in (<= 10, >0)", true.into());
  }  

  /// in operator with a single unary test
  #[test]
  fn test_in_interval() {
    compiler_test("5 in [0..10]", true.into());
    compiler_test("5 in [0..10)", true.into());
    compiler_test("5 in (0..10]", true.into());
    compiler_test("5 in (0..10)", true.into());
    compiler_test("10 in [0..10]", true.into());
    compiler_test("10 in [0..10)", false.into());
    compiler_test("0 in [0..10)", true.into());
    compiler_test("0 in (0..10)", false.into());
  }  

  #[test]
  fn test_list() {
    compiler_test("[]", FeelValue::new_list(vec![]));
    compiler_test("[1,2,true]", FeelValue::new_list(vec![1.0.into(), 2.0.into(), true.into()]));
  } 

  #[test]
  fn test_function_positional_parameters() {
    compiler_test_with_builtins("sum(1,2,3,4)", 10.0.into());
  }

  #[test]
  fn test_if_then_else() {
    compiler_test("if 2 + 2 = 4 then 1 else 0", 1.0.into());
    compiler_test("if 2 + 2 > 4 then 1 else 0", 0.0.into());
  } 

  fn compiler_test(source_expression: &str, expected: FeelValue) {
    compiler_test_base(source_expression, expected, false);
  }

  fn compiler_test_with_builtins(source_expression: &str, expected: FeelValue) {
    compiler_test_base(source_expression, expected, true);
  }

  fn compiler_test_base(source_expression: &str, expected: FeelValue, add_builtins: bool) {
    ExecutionLog::clear();
    let mut compiler = Compiler::new();
    compiler.save_parse_tree = true;
    let result = compiler.compile(source_expression);
    if print_diagnostics() {
        compiler.dump();
    }
    match result.clone() {
        Ok(expr) => {
            if print_diagnostics() { println!("Expression\n{}", expr); }
            let mut nctx = if add_builtins { 
                NestedContext::new_with_builtins()
            } else {
                NestedContext::new()
            };
            let ctx = Context::new();
            let inner_ctx = Context::new();
            inner_ctx.insert("inner level", "Sanctum Sanctorum".into());
            ctx.insert("outer level", inner_ctx.into());
            ctx.insert("hair color", "brown".into());
            ctx.insert("wind speed", 30.0.into());
            nctx.push(ctx.into());
            let mut interpreter = Interpreter::new(expr, nctx);
            let (actual, message) = interpreter.trace();
            if print_diagnostics() { println!("{}", message); }

            assert_eq!(expected, actual);    
        },
        Err(s) => {
            ExecutionLog::print("Error compiling expression:\n");
            assert!(false, "Error compiling expression: {}", s);
        }
    };
    ExecutionLog::clear();
  }
}
