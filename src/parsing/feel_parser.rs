use pest::iterators::Pairs;

#[derive(Parser)]
#[grammar = "parsing/feel.pest"]
pub struct FeelParser;

/// Dumps a Parse Tree - useful for unit tests and debugging.
pub fn show_pair_tree(pairs: Pairs<Rule>, indent: usize) -> () {
  for pair in pairs {
    // A pair is a combination of the rule which matched and a span of input
    println!("{} {:30}  Text:    {}", 
      " ".repeat(indent), 
      format!("{:?}", pair.as_rule()), 
      pair.as_str()
    );
    show_pair_tree(pair.into_inner(), indent + 2);
  }
}

/// Parse a Feel expression into a PEST Grammar derived AST. 
/// To see the output of this test:
///   > cargo test test_parse -- --nocapture
#[cfg(test)]
mod tests {
  use super::FeelParser;
  use super::Rule;
  use super::show_pair_tree;
  use crate::pest::Parser;
  // use pest::iterators::Pairs;
  #[test]
  fn test_parse() {
    let pairs = FeelParser::parse(Rule::start, "mass * speed of light ** 2").unwrap_or_else(|e| panic!("{}", e));
    show_pair_tree(pairs, 0);
  }
}
