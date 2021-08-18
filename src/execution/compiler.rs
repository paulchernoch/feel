use super::compiled_expression::CompiledExpression;
use pest::iterators::Pairs;
use crate::pest::Parser;
use crate::parsing::feel_parser::{Rule,FeelParser,show_pair_tree};

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

    pub fn compile(&mut self, source_expression: &'a str) -> Result<CompiledExpression, String> {
        match FeelParser::parse(Rule::start, source_expression) {
            Ok(pairs) => {
                if self.save_parse_tree {
                    self.parse_tree = Some(pairs.clone());
                }
                Result::Err(format!("Compiler not implemented. Unable to compile {}", source_expression))
            },
            Err(e) => {
                Result::Err(format!("Could not parse expression. {:?}", e))
            }
        }
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
  // use super::super::compiled_expression::CompiledExpression;


  #[test]
  fn test_invalid_expression() {
      let mut compiler = Compiler::new();
      assert!(compiler.compile("this is not a valid expression!").is_err());
  }

  #[test]
  fn test_addition() {
      let mut compiler = Compiler::new();
      compiler.save_parse_tree = true;
      let result = compiler.compile("2 + 2");
      compiler.dump();
      assert!(!result.is_err());
  }
}
