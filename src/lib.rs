extern crate chrono;
extern crate pest;
#[macro_use]
extern crate pest_derive;

pub mod parsing;

#[allow(unused_imports)]
use parsing::qname::QName;

#[allow(unused_imports)]
use parsing::duration::Duration;

#[allow(unused_imports)]
use parsing::context::Context;

#[allow(unused_imports)]
use parsing::feel_value::FeelValue;

extern crate strum;
#[macro_use]
extern crate strum_macros;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
