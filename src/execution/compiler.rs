use super::compiled_expression::CompiledExpression;
use pest::iterators::{Pairs,Pair};
use crate::pest::Parser;
use crate::parsing::feel_parser::{Rule,FeelParser,show_pair_tree};

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
            (Rule::positive_unary_test, _) => true, 
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

    fn walk_tree(&mut self, pair: &Pair<'a, Rule>, expr: &mut CompiledExpression) -> Result<(), String> {
        let rule = pair.as_rule();
        let children = self.children(pair);
        match self.get_production_rule(pair).as_slice() {
            [Rule::start, _] => self.walk_tree(&Compiler::first_child(pair), expr),
            [left, right] if Compiler::should_descend(*left, *right) => self.walk_tree(&Compiler::first_child(pair), expr),
            [Rule::exponentiation, Rule::unary_expression, ..] => self.infix_to_postfix(children, expr),
            [Rule::multiplicative, Rule::exponentiation, ..] => self.infix_to_postfix(children, expr),
            [Rule::additive, Rule::multiplicative, ..] => self.infix_to_postfix(children, expr),
            [Rule::comparision, _, Rule::comparision_operator, .. ] => self.infix_to_postfix(children, expr),
            // TODO: comparisons using in_token
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
                    _ => {
                        Err(format!("Incorrect number of arguments to BETWEEN expression {}.", pair.as_str()))
                    }
                }
            },
            // Case of a single positive unary test
            [Rule::comparision, _, Rule::in_token, Rule::positive_unary_test] => {
                match children.as_slice() {
                    [item, _, unary_test] => {
                        let r1 = self.walk_tree(item, expr);
                        let r2 = self.walk_tree(unary_test, expr);
                        expr.append_str("in");
                        if r1.is_err() || r2.is_err() {
                            Err(format!("Error parsing IN expression {}.", pair.as_str()))
                        }
                        else {
                            Ok(())
                        }
                    },
                    _ => {
                        Err(format!("Incorrect number of arguments to IN expression {}.", pair.as_str()))
                    }
                }
            },
            // Case of a list of positive unary tests
            [Rule::comparision, _, Rule::in_token, Rule::positive_unary_tests] => {
                match children.as_slice() {
                    [item, _, unary_tests] => {
                        let r1 = self.walk_tree(item, expr);
                        // TODO: Replace with logic to create a list of the unary tests. 
                        let r2 = self.walk_tree(unary_tests, expr);
                        expr.append_str("in");
                        if r1.is_err() || r2.is_err() {
                            Err(format!("Error parsing IN expression {}.", pair.as_str()))
                        }
                        else {
                            Ok(())
                        }
                    },
                    _ => {
                        Err(format!("Incorrect number of arguments to IN expression {}.", pair.as_str()))
                    }
                }
            },
           
            [Rule::conjunction, ..] => self.infix_to_postfix(children, expr),
            [Rule::disjunction, ..] => self.infix_to_postfix(children, expr),
            [Rule::arithmetic_negation, _] => {
                match self.walk_tree(&Compiler::first_child(pair), expr) {
                    Ok(()) => {
                        expr.append_str("neg");
                        Ok(())
                    },
                    Err(message) => Err(message)
                }
            },         
            [Rule::numeric_literal] => {
                expr.append_str(&format!("num({})", pair.as_str()));
                Ok(())
            },         
            [Rule::boolean_literal, ..] => {
                expr.append_str(&format!("{}", pair.as_str().to_lowercase()));
                Ok(())
            },
            [Rule::list, Rule::list_entries] => {
                let list_entries_children = self.children(&Compiler::first_child(pair));
                expr.append_str("list");
                for list_item in list_entries_children.iter() {
                    match self.walk_tree(&Compiler::first_child(list_item), expr) {
                        Ok(()) => {
                            expr.append_str("push");
                        },
                        Err(message) => { return Err(message); }
                    };
                }
                Ok(())
            },
            // An empty list
            [Rule::list] => {
                expr.append_str("list");
                Ok(())
            },
            _ => Err(format!("Compilation not implemented for rule {:?}", rule))
        }
    }

    fn pair_list(&self, pairs: Pairs<'a, Rule>) -> Vec<Pair<'a, Rule>> {
        let mut list = vec![];
        for pair in pairs {
            list.push(pair);
        }
        list
    }

    fn rule_list(&self, pairs: Pairs<'a, Rule>) -> Vec<Rule> {
        let mut list = vec![];
        for pair in pairs {
            list.push(pair.as_rule());
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
  use super::Compiler;
  use super::super::compiled_expression::CompiledExpression;
  use super::super::interpreter::Interpreter;
  use crate::parsing::feel_value::{FeelValue};
  use crate::parsing::{nested_context::NestedContext};

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

  #[test]
  fn test_list() {
    compiler_test("[]", FeelValue::new_list(vec![]));
    compiler_test("[1,2,true]", FeelValue::new_list(vec![1.0.into(), 2.0.into(), true.into()]));
  } 

  fn compiler_test(source_expression: &str, expected: FeelValue) {
    let mut compiler = Compiler::new();
    compiler.save_parse_tree = true;
    let result = compiler.compile(source_expression);
    if print_diagnostics() {
        compiler.dump();
    }
    match result.clone() {
        Ok(expr) => {
            if print_diagnostics() { println!("Expression\n{}", expr); }
            let mut interpreter = Interpreter::new(expr, NestedContext::new());
            let (actual, message) = interpreter.trace();
            if print_diagnostics() { println!("{}", message); }

            assert_eq!(expected, actual);    
        },
        Err(s) => {
            assert!(false, "Error compiling expression: {}", s);
        }
    };
  }
}
