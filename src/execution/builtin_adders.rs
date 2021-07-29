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
    make_adder!(sublist, "function(list<Any>,number,number)->list<Any>");
    make_adder!(append, "function(list<Any>,Any)->list<Any>");  // varargs may cause issues
    make_adder!(concatenate, "function(list<Any>)->list<Any>"); // varargs may cause issues
    make_adder!(insert_before, "function(list<Any>,number,Any)->list<Any>");
    make_adder!(remove, "function(list<Any>,number)->list<Any>");
    make_adder!(reverse, "function(list<Any>)->list<Any>");
    make_adder!(index_of, "function(list<Any>,Any)->list<Any>");
    make_adder!(union, "function(list<Any>)->list<Any>");       // varargs may cause issues
    make_adder!(distinct_values, "function(list<Any>)->list<Any>");
    make_adder!(flatten, "function(list<Any>)->list<Any>");
    make_adder!(product, "function(list<number>)->number");
    make_adder!(median, "function(list<number>)->number");
    make_adder!(stddev, "function(list<number>)->number");
    make_adder!(mode, "function(list<Any>)->list<Any>");
    make_adder!(decimal, "function(number,number)->number");
    make_adder!(floor, "function(number)->number");
    make_adder!(ceiling, "function(number)->number");
    make_adder!(abs, "function(Any)->Any");
    make_adder!(modulo, "function(number,number)->number");
    make_adder!(power, "function(number,number)->number");
    make_adder!(sqrt, "function(number)->number");
    make_adder!(log, "function(number)->number");
    make_adder!(exp, "function(number)->number");
    make_adder!(even, "function(number)->boolean");
    make_adder!(odd, "function(number)->boolean");
    make_adder!(before, "function(Any,Any)->boolean");
    make_adder!(after, "function(Any,Any)->boolean");
    make_adder!(meets, "function(Any,Any)->boolean");
    make_adder!(met_by, "function(Any,Any)->boolean");
    make_adder!(overlaps, "function(Any,Any)->boolean");
    make_adder!(overlaps_before, "function(Any,Any)->boolean");
    make_adder!(overlaps_after, "function(Any,Any)->boolean");
    make_adder!(finishes, "function(Any,Any)->boolean");
    make_adder!(finished_by, "function(Any,Any)->boolean");
    make_adder!(includes, "function(Any,Any)->boolean");
    make_adder!(during, "function(Any,Any)->boolean");
    make_adder!(starts, "function(Any,Any)->boolean");
    make_adder!(started_by, "function(Any,Any)->boolean");
    make_adder!(coincides, "function(Any,Any)->boolean");
    make_adder!(in_operator, "function(Any,Any)->boolean");
    make_adder!(get_value, "function(context<>,Any)->Any"); // Second argument is string or name
    make_adder!(get_entries, "function(context<>)->list<context<key:name,value:Any>>");
    make_adder!(day_of_year, "function(Any)->number");
    make_adder!(day_of_week, "function(Any)->string");
    make_adder!(month_of_year, "function(Any)->string");
    make_adder!(week_of_year, "function(Any)->number");
    make_adder!(duration, "function(string)->Any");
    make_adder!(years_and_months_duration, "function(Any,Any)->years and months duration");
    make_adder!(is, "function(Any,Any)->boolean");
    make_adder!(is_not, "function(Any,Any)->boolean");
    make_adder!(equals, "function(Any,Any)->boolean");
    make_adder!(date, "function(Any)->date"); // Three different function signatures permitted!
    make_adder!(date_and_time, "function(Any)->date and time");
    make_adder!(time, "function(Any)->time");
    make_adder!(number, "function(string,string,string)->number");

    /// Add a builtin to the context that calls "Builtins::type_name" 
    /// but is accessed by the key "type".  
    pub fn type_name(ctx: &mut Context) {
        // NOTE: This method could not be generated by the make_adder macro
        // because it would have to create a method named "type", which
        // is a reserved word in Rust. Thius this method is called "type_name"
        // but is stored in the context under the name "type",
        // unlike all the other builtins, whose names may be generated by
        // simply replacing underscores by spaces. 
        let f = move |value: &FeelValue, _ctx: &mut NestedContext| -> FeelValue { Builtins::type_name(value.clone(), _ctx) };
        Builtins::add_builtin(ctx, "type", "function(Any)->string", f);
    }

    make_adder!(string, "function(Any)->string");
    make_adder!(instance_of, "function(Any,Any)->boolean");
}
