use crate::pest::Parser;
use std::result::Result;
use pest::iterators::{Pairs, Pair};
use super::lattice_type::{LatticeType};

#[derive(Parser)]
#[grammar = "parsing/lattice_type.pest"]
pub struct LatticeTypeParser;

/*
fn dump_pair(pair: &Pair<'_, Rule>, message: &str) {
    println!("PAIR for {}:", message);
    println!("    Rule:    {:?}", pair.as_rule());
    println!("    Span:    {:?}", pair.as_span());
    println!("    Text:    {}", pair.as_str());
}
*/

/// Recursively process a parser Pair and any children to construct a LatticeType.  
fn process_pair(pair: Pair<'_, Rule>) -> LatticeType {
    let rule = pair.as_rule();
    match rule {

        // The simple, non-recursive types:

        Rule::null => LatticeType::Null,
        Rule::any => LatticeType::Any,
        Rule::number => LatticeType::Number,
        Rule::string => LatticeType::String,
        Rule::name => LatticeType::Name,
        Rule::boolean => LatticeType::Boolean,
        Rule::date => LatticeType::Date,
        Rule::date_and_time => LatticeType::DateAndTime,
        Rule::time => LatticeType::Time,
        Rule::years_and_months_duration => LatticeType::YearMonthDuration,
        Rule::days_and_time_duration => LatticeType::DayTimeDuration,

        // The complex, recursive types
        Rule::range => {
            let range_limit_type = process_pair(pair.into_inner().next().unwrap());
            LatticeType::range(range_limit_type)
        },
        Rule::list => {
            let list_element_type = process_pair(pair.into_inner().next().unwrap());
            LatticeType::list(list_element_type)
        },
        Rule::function => {
            let mut parameter_types: Vec<LatticeType> = Vec::new();
            let mut inner = pair.into_inner();
            let param_tokens = inner.next().unwrap();
            // The actual return type is inside a "return_type" token.
            let return_type_token = inner.next().unwrap().into_inner().next().unwrap();

            for p in param_tokens.into_inner() {
                parameter_types.push(process_pair(p));
            }
            let return_type = process_pair(return_type_token);
            
            LatticeType::function(parameter_types, return_type)
        },
        Rule::context => {
            let mut key_value_pairs: Vec<(String,LatticeType)> = Vec::new();
            for context_item in pair.into_inner() {
                let mut inner = context_item.into_inner();
                let key = inner.next().unwrap().as_str().trim().to_string();
                let item_type = process_pair(inner.next().unwrap());
                key_value_pairs.push((key, item_type));
            }
            LatticeType::context(key_value_pairs)
        },

        _ => unreachable!(format!("Unexpected token {:?} while building LatticeType", pair.as_str()))
    }
}

/// Parse a LatticeType from a string using a PEG parser defined in a Pest grammar file. 
pub fn parse_lattice_type(lattice_type_string: &str) -> Result<LatticeType, String> {
    let parse_result: Result<Pairs<Rule>, pest::error::Error<Rule>> = LatticeTypeParser::parse(Rule::ladder_type, lattice_type_string);
    match parse_result {
        Result::Err(e) => return Result::Err(format!("Error parsing {} into a Lattice Type is {:?}", lattice_type_string, e)),
        Result::Ok(pairs) => {
  
            // At the top level, expect only two tokens, and the second is EOI, to be skipped.
            let pair = pairs.into_iter().next().unwrap(); 
            return Result::Ok(process_pair(pair));
        }
    }
  
}


#[cfg(test)]
mod tests {
    use crate::parsing::lattice_type::LatticeType;
    use super::parse_lattice_type;

    #[test]
    fn test_parsing_simple_lattice_types() {
        assert_eq!(
            LatticeType::Null,
            parse_lattice_type("Null").unwrap()
        );
        assert_eq!(
            LatticeType::Any,
            parse_lattice_type("Any").unwrap()
        );
        assert_eq!(
            LatticeType::Number,
            parse_lattice_type("number").unwrap()
        );
        assert_eq!(
            LatticeType::String,
            parse_lattice_type("string").unwrap()
        );
        assert_eq!(
            LatticeType::Name,
            parse_lattice_type("name").unwrap()
        );
        assert_eq!(
            LatticeType::Boolean,
            parse_lattice_type("boolean").unwrap()
        );
        assert_eq!(
            LatticeType::Date,
            parse_lattice_type("date").unwrap()
        );
        assert_eq!(
            LatticeType::DateAndTime,
            parse_lattice_type("date and time").unwrap()
        );
        assert_eq!(
            LatticeType::Time,
            parse_lattice_type("time").unwrap()
        );
        assert_eq!(
            LatticeType::YearMonthDuration,
            parse_lattice_type("years and months duration").unwrap()
        );
        assert_eq!(
            LatticeType::DayTimeDuration,
            parse_lattice_type("days and time duration").unwrap()
        );
    }

    #[test]
    fn test_parsing_complex_lattice_types() {
        assert_eq!(
            LatticeType::range(LatticeType::Number),
            parse_lattice_type("range<number>").unwrap()
        );
        assert_eq!(
            LatticeType::list(LatticeType::String),
            parse_lattice_type("list < string >").unwrap()
        );
        assert_eq!(
            LatticeType::function(vec![LatticeType::String, LatticeType::Number], LatticeType::String),
            parse_lattice_type("function(string, number) -> string").unwrap()
        );
        assert_eq!(
            LatticeType::context(vec![("first name".to_string(), LatticeType::String), ("age".to_string(), LatticeType::Number)]),
            parse_lattice_type("context<age: number, first name: string>").unwrap()
        );
    }

}
