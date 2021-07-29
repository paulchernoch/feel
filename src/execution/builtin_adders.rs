use super::builtins::Builtins;
use crate::parsing::feel_value::FeelValue;
use crate::parsing::context::Context;
use crate::parsing::nested_context::NestedContext;

/// Methods for adding Builtin functions to a context.
pub struct BuiltinAdders {
}

macro_rules! make_adder {
    ($builtin: ident, $lattice_type_string: expr) => {
      pub fn $builtin(ctx: &mut Context) {
        let f = move |value: &FeelValue, _ctx: &mut NestedContext| -> FeelValue { Builtins::$builtin(value.clone(), _ctx) };
        Builtins::add_builtin(ctx, &stringify!($builtin).replace("_", " "), $lattice_type_string, f);
      }
    };
}

impl BuiltinAdders {

    make_adder!(not, "function(boolean)->boolean");
    make_adder!(substring, "function(string,number,number)->string");
    make_adder!(string_length, "function(string)->number");
    make_adder!(upper_case, "function(string)->string");
    make_adder!(lower_case, "function(string)->string");
    make_adder!(substring_before, "function(string,string)->string");
    make_adder!(substring_after, "function(string,string)->string");
    make_adder!(replace, "function(string,string,string,string)->string");
    make_adder!(contains, "function(string,string)->boolean");
    make_adder!(starts_with, "function(string,string)->boolean");
    make_adder!(ends_with, "function(string,string)->boolean");
    make_adder!(matches, "function(string,string,string)->boolean");
    make_adder!(split, "function(string,string)->list<string>");
    make_adder!(list_contains, "function(list<Any>,Any)->boolean");
    make_adder!(count, "function(list<Any>)->number");
    make_adder!(min, "function(list<Any>)->Any");
    make_adder!(max, "function(list<Any>)->Any");

    make_adder!(sum, "function(list<number>)->number");
    make_adder!(mean, "function(list<number>)->number");
    make_adder!(all, "function(list<Any>)->boolean");
    make_adder!(any, "function(list<Any>)->boolean");
    make_adder!(sublist, "");
    make_adder!(append, "");
    make_adder!(concatenate, "");
    make_adder!(insert_before, "");
    make_adder!(remove, "");
    make_adder!(reverse, "");
    make_adder!(index_of, "");
    make_adder!(union, "");
    make_adder!(distinct_values, "");
    make_adder!(flatten, "");
    make_adder!(product, "");
    make_adder!(median, "");
    make_adder!(stddev, "");
    make_adder!(mode, "");
    make_adder!(decimal, "");
    make_adder!(floor, "");
    make_adder!(ceiling, "");
    make_adder!(abs, "");
    make_adder!(modulo, "");
    make_adder!(power, "");
    make_adder!(sqrt, "");
    make_adder!(log, "");
    make_adder!(exp, "");
    make_adder!(even, "");
    make_adder!(odd, "");
    make_adder!(before, "");
    make_adder!(after, "");
    make_adder!(meets, "");
    make_adder!(met_by, "");
    make_adder!(overlaps, "");
    make_adder!(overlaps_before, "");
    make_adder!(overlaps_after, "");
    make_adder!(finishes, "");
    make_adder!(finished_by, "");
    make_adder!(includes, "");
    make_adder!(during, "");
    make_adder!(starts, "");
    make_adder!(started_by, "");
    make_adder!(coincides, "");
    make_adder!(in_operator, "");
    make_adder!(get_value, "");
    make_adder!(get_entries, "");
    make_adder!(day_of_week, "");
    make_adder!(month_of_year, "");
    make_adder!(week_of_year, "");
    make_adder!(duration, "");
    make_adder!(years_and_months_duration, "");
    make_adder!(is, "");
    make_adder!(is_not, "");
    make_adder!(equals, "");
    make_adder!(date, "");
    make_adder!(date_and_time, "");
    make_adder!(time, "");
    make_adder!(number, "");
    make_adder!(type_name, "");
    make_adder!(string, "");
    make_adder!(instance_of, "");
    

}
