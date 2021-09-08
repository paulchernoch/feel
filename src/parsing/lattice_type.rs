use std::fmt::{Debug,Display,Formatter,Result};
use std::str::FromStr;
use std::result::Result as stdResult;
use super::feel_value::FeelType;
use super::context::ContextReader;
use super::feel_value::FeelValue;
use super::lattice_type_parser::parse_lattice_type;

/// Models the Generic Type Lattice shown in Figure 10-16 in the DMN 1.3 Spec.
/// Types may be Null, Simple (number, string, boolean, date, time, date and time, years and months duration, days and time duration) 
/// or Constructed (range, list, context, function), or the Any type.
/// 
/// Important concerns handled by this class are type equivalence (DMN 1.3 §10.3.2.9.1) 
/// and type conformance (DMN 1.3 §10.3.2.9.2).
#[derive(Clone)]
pub enum LatticeType {
  /// A Number
  Number,
  /// A String
  String,
  /// A Qualified Name, which is a list of strings, none of which should contain whitespace.
  Name, 
  /// A Boolean
  Boolean,
  /// A Date
  Date,
  /// A Time
  Time,
  /// A DateTime
  DateAndTime,
  /// Duration with only years and months filled in
  YearMonthDuration, 
  /// Duration with only days, hours, minutes and seconds filled in
  DayTimeDuration,
  /// A Range defined as a pair of low and high values
  Range(Box<LatticeType>),
  /// A sequential collection of other values
  List(Box<LatticeType>),
  /// A collection of name-value pairs
  Context(Vec<(String,LatticeType)>),
  /// A function taking a single argument (a FeelValue) and returning a single result (a FeelValue)
  Function { parameters: Vec<LatticeType>, return_type: Box<LatticeType> },
  /// A missing value
  Null,
  /// A value of any type
  Any
}

#[derive(Clone)]
pub struct ContextTypeBuilder {
    pairs: Vec<(String,LatticeType)>
}

impl ContextTypeBuilder {
    pub fn new() -> ContextTypeBuilder {
        ContextTypeBuilder { pairs: Vec::new() }
    }

    pub fn add_key<S: Into<String>>(mut self, key: S, key_type: LatticeType ) -> Self {
        self.pairs.push((key.into(), key_type));
        self
    }

    pub fn build(mut self) -> LatticeType {
        // Type equivalence and compatibility rely upon keys being sorted.
        self.pairs.sort_by_cached_key(|(key, _t)| key.clone() );
        LatticeType::Context(self.pairs)
    }
}

impl LatticeType {

    /// Infer the most specific type we can for the items in the list. 
    ///   - If list is empty, return Any. 
    ///   - If all items are Null, return Null. 
    ///   - If some items are Null, but all remaining items have the same type T, return T. 
    ///   - If at least two items exist in the list that are not Null and have different types, return Any. 
    fn infer_list_item_type<C: ContextReader>(list: &Vec<FeelValue>, context: &C) -> LatticeType {
        match list.len() {
            0 => LatticeType::Any,
            1 => LatticeType::from_value(&list[0], context),
            _ => {
                let mut first_item_type = LatticeType::from_value(&list[0], context);
                let same_types = list.iter().skip(1).all(|item| {
                    let item_type = LatticeType::from_value(item, context);
                    if first_item_type.equivalent_to(&LatticeType::Null) {
                        first_item_type = item_type.clone();
                        true
                    }
                    else if item_type.equivalent_to(&LatticeType::Null) {
                        true
                    }
                    else {
                        first_item_type.equivalent_to(&item_type)
                    }
                });
                if same_types {
                    first_item_type
                }
                else {
                    LatticeType::Any
                }
            }
        }
    }

    /// Infer the LatticeType from a FeelValue. 
    pub fn from_value<C: ContextReader>(value: &FeelValue, context: &C) -> LatticeType {
        match value {
            FeelValue::Number(_) => LatticeType::Number,
            FeelValue::String(_) => LatticeType::String,
            FeelValue::Name(_) => LatticeType::Name,
            FeelValue::Boolean(_) => LatticeType::Boolean,
            FeelValue::Date(_) => LatticeType::Date,
            FeelValue::Time(_) => LatticeType::Time,
            FeelValue::DateAndTime(_) => LatticeType::DateAndTime,
            FeelValue::YearMonthDuration(_) => LatticeType::YearMonthDuration,
            FeelValue::DayTimeDuration(_) => LatticeType::DayTimeDuration,
            FeelValue::Range(range) => LatticeType::Range(Box::new(range.get_bounds_lattice_type(context))),
            FeelValue::List(rr_list) => LatticeType::List(Box::new(LatticeType::infer_list_item_type(&rr_list.borrow(), context))),
            FeelValue::Context(c) => c.infer_lattice_type(context),

            FeelValue::Function(f) => f.get_lattice_type().clone(),
            FeelValue::Null => LatticeType::Null,
            _ => LatticeType::Any
          }
    }


    /// Create a LatticeType::Function with the given parameter types and return type. 
    pub fn function(parameters: Vec<LatticeType>, return_type: LatticeType) -> LatticeType {
        LatticeType::Function { parameters: parameters, return_type: Box::new(return_type) }
    }

    /// Create a LatticeType::Function with the given number of parameters, all of the given type, and returning the same type. 
    /// This is good for creating a function that takes Any and returns Any, or takes Strings and returns a String.
    pub fn homogeneous_function(parameter_count: usize, lattice_type: LatticeType) -> LatticeType {
        let params: Vec<LatticeType> = (1..=parameter_count).map(|_i| lattice_type.clone()).collect();
        LatticeType::Function { parameters: params, return_type: Box::new(lattice_type) }
    }

    /// Create a LatticeType::Range.
    pub fn range(t: LatticeType) -> LatticeType {
        LatticeType::Range(Box::new(t))
    }

    /// Create a LatticeType::List.
    pub fn list(t: LatticeType) -> LatticeType {
        LatticeType::List(Box::new(t))
    }

    /// Create a LatticeType::Context.
    pub fn context(mut pairs: Vec<(String,LatticeType)>) -> LatticeType {
        pairs.sort_by_cached_key(|(key, _t)| key.clone() );
        LatticeType::Context(pairs)
    }

    /// Determine if one type conforms to another, according to DMN 1.3 §10.3.2.9.2.
    /// 
    /// The conformance relation (<:) is defined as follows:
    ///   1. Conformance includes equivalence. If T ≡ S then T <: S
    ///   2. For every type T, Null <: T <: Any, where Null is the lower type in the lattice and Any the upper type in the lattice.
    ///   3. The list type list<T> conforms to list<S> iff T conforms to S.
    ///   4. The context type context<k1: T1, ..., kn: Tn> conforms to context<l1: S1, ..., lm: Sm> 
    ///      iff n ≥ m and for every li : Si there is a unique kj:Tj such that li = kj and Tj <:Si for i=1,m
    ///   5. The function type (T1,...,Tn)→U conforms to type (S1,...,Sm)→V 
    ///      iff n=m,Si <:Ti fori=1,n and U<:V. 
    ///      The FEEL functions follow the “contravariant function argument type” and “covariant function return type” 
    ///      principles to provide type safety.
    ///   6. The range type range<T> conforms to range<S> 
    ///      iff T conforms to S. 
    /// 
    /// Type conformance is transitive: if type1 conforms to type2, and type2 conforms to type3 , then type1 conforms to type3.
    pub fn conforms_to(&self, t: &LatticeType) -> bool {
        match (self, t) {
            (LatticeType::Null, _) => true,
            (_, LatticeType::Any) => true,

            // This covers all remaining cases except the complex types (range, list, context, function)
            (a, b) if a.equivalent_to(b) => true,

            (LatticeType::Range(t1), LatticeType::Range(t2)) => t1.conforms_to(&*t2),
            (LatticeType::List(t1), LatticeType::List(t2)) => t1.conforms_to(&*t2),
            (LatticeType::Context(key_types1), LatticeType::Context(key_types2)) 
                if key_types1.len() >= key_types2.len() => {
                key_types2
                    .iter()
                    .all(|(key2,type2)| key_types1.iter().any(|(key1,type1)| key1 == key2 && type1.conforms_to(type2) ))
            },
            (LatticeType::Function { parameters: parm1, return_type: rt1 }, 
                LatticeType::Function { parameters: parm2, return_type: rt2 })
                if parm1.len() == parm2.len() && rt1.conforms_to(&*rt2) => {
                parm1
                    .iter()
                    .zip(parm2.iter())
                    .all(|(a,b)| a.conforms_to(b))
            },
            _ => false
        }
    }

    /// Determine if one type is equivalent to another, according to DMN 1.3 §10.3.2.9.1.
    /// The equivalence relationship (≡) between types is defined as follows:
    ///   1. Primitive datatypes are equivalent to themselves, e.g., string ≡ string. 
    ///   2. Two list types list<T> and list<S> are equivalent iff T is equivalent to S. 
    ///      For example, the types of [“a”, “b”] and [“c”] are equivalent.
    ///   3. Two context types context<k1: T1, ..., kn: Tn> and context<l1: S 1, ..., lm: Sm> 
    ///      are equivalent iff n = m and for every ki :Ti there is a unique lj :Sj 
    ///      such that ki = lj and Ti ≡ Sj for i = 1, n. 
    ///      Context types are the types defined via ItemDefinitions or the types associated to 
    ///      FEEL context literals such as { “name”: “John”, “age”: 25}.
    ///   4. Two function types (T1,...,Tn)→U and (S1,...,Sm)→V 
    ///      are equivalent iff n=m, Ti ≡ Sj for i=1,n and U≡V.
    ///   5. Two range types range<T> and range<S> are equivalent 
    ///      iff T is equivalent to S. 
    ///      For example, the types of [1..10] and [30..40] are equivalent.
    /// 
    /// Type equivalence is transitive: 
    ///   if type1 is equivalent to type2, 
    ///   and type2 is equivalent to type3, 
    ///   then type1 is equivalent to type3.
    pub fn equivalent_to(&self, t: &LatticeType) -> bool {
        match (self, t) {
            (LatticeType::Number, LatticeType::Number) => true,
            (LatticeType::String, LatticeType::String) => true,
            (LatticeType::Name, LatticeType::Name) => true,
            (LatticeType::Boolean, LatticeType::Boolean) => true,
            (LatticeType::Date, LatticeType::Date) => true,
            (LatticeType::Time, LatticeType::Time) => true,
            (LatticeType::DateAndTime, LatticeType::DateAndTime) => true,
            (LatticeType::YearMonthDuration, LatticeType::YearMonthDuration) => true,
            (LatticeType::DayTimeDuration, LatticeType::DayTimeDuration) => true,
            (LatticeType::Range(t1), LatticeType::Range(t2)) => t1.equivalent_to(&*t2),
            (LatticeType::List(t1), LatticeType::List(t2)) => t1.equivalent_to(&*t2),
            (LatticeType::Context(key_types1), LatticeType::Context(key_types2)) 
              if key_types1.len() == key_types2.len() => {
                key_types1
                    .iter()
                    .zip(key_types2.iter())
                    .all(|((key1,type1),(key2,type2))| key1 == key2 && type1.equivalent_to(type2))
            },
            (LatticeType::Function { parameters: parm1, return_type: rt1 }, 
             LatticeType::Function { parameters: parm2, return_type: rt2 })
               if parm1.len() == parm2.len() && rt1.equivalent_to(&*rt2) => {
                 parm1
                    .iter()
                    .zip(parm2.iter())
                    .all(|(a,b)| a.equivalent_to(b))
             },
            (LatticeType::Null, LatticeType::Null) => true,
            (LatticeType::Any, LatticeType::Any) => true,
            _ => false
        }
    }

}

impl Eq for LatticeType {}

impl PartialEq for LatticeType {
    fn eq(&self, other: &Self) -> bool {
      self.equivalent_to(other)
    }
}

impl From<LatticeType> for FeelType {
    /// Get the non-generic FeelType that most closely corresponds to this LatticeType.
    /// This is a lossy conversion. 
    fn from(t: LatticeType) -> Self {
        match t {
            LatticeType::Number => FeelType::Number,
            LatticeType::String => FeelType::String,
            LatticeType::Name => FeelType::Name,
            LatticeType::Boolean => FeelType::Boolean,
            LatticeType::Date => FeelType::Date,
            LatticeType::Time => FeelType::Time,
            LatticeType::DateAndTime => FeelType::DateAndTime,
            LatticeType::YearMonthDuration => FeelType::YearMonthDuration,
            LatticeType::DayTimeDuration => FeelType::DayTimeDuration,
            LatticeType::Range(_) => FeelType::Range,
            LatticeType::List(_) => FeelType::List,
            LatticeType::Context(_) => FeelType::Context,
            LatticeType::Function { .. } => FeelType::Function,
            LatticeType::Null => FeelType::Null,
            LatticeType::Any => FeelType::Any
        }
    }
}

impl From<FeelType> for LatticeType {
    /// Get the generic LatticeType that most generally corresponds to this FeelType.
    /// This is an imprecise conversion. 
    fn from(t: FeelType) -> Self {
        match t {
            FeelType::Number => LatticeType::Number,
            FeelType::String => LatticeType::String,
            FeelType::Name => LatticeType::Name,
            FeelType::Boolean => LatticeType::Boolean,
            FeelType::Date => LatticeType::Date,
            FeelType::Time => LatticeType::Time,
            FeelType::DateAndTime => LatticeType::DateAndTime,
            FeelType::YearMonthDuration => LatticeType::YearMonthDuration,
            FeelType::DayTimeDuration => LatticeType::DayTimeDuration,
            FeelType::Range => LatticeType::Range(Box::new(LatticeType::Any)),
            FeelType::List => LatticeType::List(Box::new(LatticeType::Any)),
            FeelType::Context => LatticeType::Context(Vec::new()), // An empty Context with no properties
            FeelType::Function => LatticeType::Function { parameters: vec![LatticeType::Any], return_type: Box::new(LatticeType::Any) },
            FeelType::Null => LatticeType::Null,
            FeelType::Any => LatticeType::Any
        }
    }
}

impl Debug for LatticeType {
    /// Format the LatticeType as a string that conforms carefully to the representation shown in the DMN spec. 
    /// From the spec, it is difficult to tell if spaces are inserted after commas, the context key colons,
    /// or around the arrow in the funtion return value. We add spaces in all those places. 
    /// We do not add spaces around the angle brackets or function parentheses or between the primary type name and the 
    /// opening bracket.
    fn fmt(&self, f: &mut Formatter) -> Result {
        let mut tmp = "".to_string();
        let type_string = match self {
            LatticeType::Number => "number",
            LatticeType::String => "string",
            LatticeType::Name => "name", // TODO: Not sure what string should be returned for this. 
            LatticeType::Boolean => "boolean",
            LatticeType::Date => "date",
            LatticeType::Time => "time",
            LatticeType::DateAndTime => "date and time",
            LatticeType::YearMonthDuration => "years and months duration",
            LatticeType::DayTimeDuration => "days and time duration",
            LatticeType::Range(t) => { tmp = format!("range<{:?}>", &*t); &tmp },
            LatticeType::List(t) => { tmp = format!("list<{:?}>", &*t); &tmp },
            LatticeType::Context(pairs) => {
                tmp.push_str("context<");
                let mut no_comma = true;
                for (k,t) in pairs {
                    if no_comma {
                        no_comma = false;
                    }
                    else {
                        tmp.push_str(", ");
                    }
                    tmp.push_str(&format!("{}: {:?}", k, t));
                }
                tmp.push_str(">");
                &tmp
            },
            LatticeType::Function { parameters, return_type } => {
                tmp.push_str("function(");
                let mut no_comma = true;
                for p in parameters {
                    if no_comma {
                        no_comma = false;
                    }
                    else {
                        tmp.push_str(", ");
                    }
                    tmp.push_str(&format!("{:?}", p));
                }
                tmp.push_str(&format!(") -> {:?}", &*return_type));
                &tmp
            },
            LatticeType::Null => "Null", // The Spec capitalizes Null for some reason.
            LatticeType::Any => "Any"
        };
        write!(f, "{}", type_string)
    }
}

impl Display for LatticeType {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{:?}", self)
    }
}

impl FromStr for LatticeType {
    type Err = String;

    fn from_str(s: &str) -> stdResult<Self, Self::Err> {
        match parse_lattice_type(s) {
            Ok(lattice_type) => stdResult::Ok(lattice_type),
            Err(message) => stdResult::Err(message)
        }
    }
}

/////////////// TESTS /////////////////

#[cfg(test)]
mod tests {
    use super::LatticeType;
    use super::ContextTypeBuilder;
    use super::super::feel_value::FeelValue;
    use super::super::context::Context;
    use std::str::FromStr;

    #[test]
    fn test_equivalence() {
        assert!(LatticeType::Any.equivalent_to(&LatticeType::Any));
        assert!(! LatticeType::Any.equivalent_to(&LatticeType::String));
        assert!(LatticeType::Number.equivalent_to(&LatticeType::Number));
        assert!(LatticeType::Null.equivalent_to(&LatticeType::Null));
        assert!(LatticeType::String.equivalent_to(&LatticeType::String));
        assert!(LatticeType::Boolean.equivalent_to(&LatticeType::Boolean));
        assert!(LatticeType::Date.equivalent_to(&LatticeType::Date));
        assert!(LatticeType::Time.equivalent_to(&LatticeType::Time));
        assert!(! LatticeType::DateAndTime.equivalent_to(&LatticeType::Time));

        // Ranges must also match element type
        assert!(LatticeType::Range(Box::new(LatticeType::Number))
          .equivalent_to(&LatticeType::Range(Box::new(LatticeType::Number))));
        assert!(! LatticeType::Range(Box::new(LatticeType::Number))
          .equivalent_to(&LatticeType::Range(Box::new(LatticeType::String))));

        // Lists must also match element type
        assert!(LatticeType::List(Box::new(LatticeType::Number))
          .equivalent_to(&LatticeType::List(Box::new(LatticeType::Number))));
        assert!(! LatticeType::List(Box::new(LatticeType::Number))
          .equivalent_to(&LatticeType::List(Box::new(LatticeType::String))));   

        // Functions must match on parameter count, type, and return value
        let function_type = LatticeType::function(vec![LatticeType::Number], LatticeType::Number);
        let same_type = LatticeType::function(vec![LatticeType::Number], LatticeType::Number);
        let diff_param_count = LatticeType::function(vec![LatticeType::Number, LatticeType::Number], LatticeType::Number);
        let diff_return_type = LatticeType::function(vec![LatticeType::Number], LatticeType::Any);
        assert!(function_type.equivalent_to(&same_type));
        assert!(! function_type.equivalent_to(&diff_param_count));
        assert!(! function_type.equivalent_to(&diff_return_type));

        // Contexts must have same keys and same types for matching keys. 
        let context_type = ContextTypeBuilder::new()
            .add_key("name", LatticeType::String)
            .add_key("age", LatticeType::Number)
            .build();
        // Change order of keys, but expect builder to sort it out.
        let same_type = ContextTypeBuilder::new()
            .add_key("age", LatticeType::Number)
            .add_key("name", LatticeType::String)
            .build();
        let types_differ = ContextTypeBuilder::new()
            .add_key("name", LatticeType::String)
            .add_key("age", LatticeType::String)
            .build();
        let arity_differs = ContextTypeBuilder::new()
            .add_key("name", LatticeType::String)
            .add_key("nationality", LatticeType::String)
            .add_key("age", LatticeType::Number)
            .build();
            assert!(context_type.equivalent_to(&same_type));
            assert!(! context_type.equivalent_to(&types_differ));
            assert!(! context_type.equivalent_to(&arity_differs));
    }

    #[test]
    fn test_conformance() {
        assert!(LatticeType::Any.conforms_to(&LatticeType::Any));
        assert!(LatticeType::Null.conforms_to(&LatticeType::Any));
        assert!(LatticeType::Null.conforms_to(&LatticeType::String));
        assert!(LatticeType::Null.conforms_to(&LatticeType::Time));

        // Not commutative!
        assert!(! LatticeType::Any.conforms_to(&LatticeType::String));
        assert!(LatticeType::String.conforms_to(&LatticeType::Any));

        assert!(LatticeType::Number.conforms_to(&LatticeType::Number));
        assert!(LatticeType::Null.conforms_to(&LatticeType::Null));
        assert!(LatticeType::String.conforms_to(&LatticeType::String));
        assert!(LatticeType::Boolean.conforms_to(&LatticeType::Boolean));
        assert!(LatticeType::Date.conforms_to(&LatticeType::Date));
        assert!(LatticeType::Time.conforms_to(&LatticeType::Time));
        assert!(! LatticeType::DateAndTime.conforms_to(&LatticeType::Time));

        // Ranges must also match element type
        assert!(LatticeType::range(LatticeType::Number)
          .conforms_to(&LatticeType::range(LatticeType::Number)));
        assert!(! LatticeType::range(LatticeType::Number)
          .conforms_to(&LatticeType::range(LatticeType::String)));
        // Second Range can be over Any. 
        assert!(LatticeType::range(LatticeType::Number)
          .conforms_to(&LatticeType::range(LatticeType::Any)));
        // But first Range cannot be over Any
        assert!(! LatticeType::Range(Box::new(LatticeType::Any))
          .conforms_to(&LatticeType::range(LatticeType::Number)));


        // Lists must also match element type
        assert!(LatticeType::list(LatticeType::Number)
          .conforms_to(&LatticeType::list(LatticeType::Number)));
        assert!(! LatticeType::list(LatticeType::Number)
          .conforms_to(&LatticeType::list(LatticeType::String)));   
        assert!(LatticeType::list(LatticeType::Number)
          .conforms_to(&LatticeType::list(LatticeType::Any)));
        assert!(LatticeType::list(LatticeType::Null)
          .conforms_to(&LatticeType::list(LatticeType::Number)));

        // Functions must match on parameter count, type, and return value
        let function_type = LatticeType::function(vec![LatticeType::Number], LatticeType::Number);
        let same_type = LatticeType::function(vec![LatticeType::Number], LatticeType::Number);
        let diff_param_count = LatticeType::function(vec![LatticeType::Number, LatticeType::Number], LatticeType::Number);
        let diff_return_type = LatticeType::function(vec![LatticeType::Number], LatticeType::Any);
        assert!(function_type.conforms_to(&same_type));
        assert!(! function_type.conforms_to(&diff_param_count));
        assert!(function_type.conforms_to(&diff_return_type));

        // Contexts must have same keys and same types for matching keys. 
        let context_type = ContextTypeBuilder::new()
            .add_key("name", LatticeType::String)
            .add_key("age", LatticeType::Number)
            .build();
        // Change order of keys, but expect builder to sort it out.
        let same_type = ContextTypeBuilder::new()
            .add_key("age", LatticeType::Number)
            .add_key("name", LatticeType::String)
            .build();
        let types_differ = ContextTypeBuilder::new()
            .add_key("name", LatticeType::String)
            .add_key("age", LatticeType::String)
            .build();
        let extra_field = ContextTypeBuilder::new()
            .add_key("name", LatticeType::String)
            .add_key("nationality", LatticeType::String)
            .add_key("age", LatticeType::Number)
            .build();
        assert!(context_type.conforms_to(&same_type));
        assert!(! context_type.conforms_to(&types_differ));
        assert!(! context_type.conforms_to(&extra_field));
        // Reverse the order and you may have conformance. 
        // A context with more keys can conform to a context with fewer keys, 
        // if none are missing and the types conform. 
        assert!(extra_field.conforms_to(&context_type));
    }
  
    #[test]
    fn test_from_value() {
        let ctx = Context::new();
        assert_eq!(LatticeType::from_value(&FeelValue::Boolean(true), &ctx), LatticeType::Boolean);
        assert_eq!(LatticeType::from_value(&FeelValue::Number(2.0), &ctx), LatticeType::Number);
        assert_eq!(
            LatticeType::from_value(&FeelValue::new_list(vec![1.into(),2.into(),3.into()]), &ctx), 
            LatticeType::list(LatticeType::Number)
        );
        assert_eq!(
            LatticeType::from_value(&FeelValue::new_list(vec![1.into(),FeelValue::Null,3.into()]), &ctx), 
            LatticeType::list(LatticeType::Number)
        );
        // Inhomogeneous List should be list<Any>
        assert_eq!(
            LatticeType::from_value(&FeelValue::new_list(vec![1.into(),FeelValue::Null,"hello".into()]), &ctx), 
            LatticeType::list(LatticeType::Any)
        );
    }

    #[test]
    fn test_display() {
        assert_eq!("Null", &LatticeType::Null.to_string());
        assert_eq!("Any", &LatticeType::Any.to_string());
        assert_eq!("date", &LatticeType::Date.to_string());
        assert_eq!("date and time", &LatticeType::DateAndTime.to_string());
        assert_eq!("time", &LatticeType::Time.to_string());
        assert_eq!("boolean", &LatticeType::Boolean.to_string());
        assert_eq!("number", &LatticeType::Number.to_string());
        assert_eq!("years and months duration", &LatticeType::YearMonthDuration.to_string());
        assert_eq!("days and time duration", &LatticeType::DayTimeDuration.to_string());
        assert_eq!("range<number>", &LatticeType::range(LatticeType::Number).to_string());
        assert_eq!("list<string>", &LatticeType::list(LatticeType::String).to_string());
        assert_eq!("list<range<date>>", &LatticeType::list(LatticeType::range(LatticeType::Date)).to_string());
        // context method should sort the keys
        assert_eq!(
            "context<age: number, children: list<string>, name: string>", 
            &LatticeType::context(
                vec![
                    ("name".into(), LatticeType::String), 
                    ("age".into(), LatticeType::Number), 
                    ("children".into(), LatticeType::list(LatticeType::String))
                ]
            ).to_string()
        );
        assert_eq!(
            "context<>", 
            &LatticeType::context(Vec::new()).to_string()
        );
        assert_eq!(
            "function(string, number) -> string", 
            &LatticeType::function(vec![LatticeType::String, LatticeType::Number], LatticeType::String).to_string()
        );
    }

    #[test]
    fn test_from_str() {
        assert_eq!(LatticeType::from_str("date and time").unwrap(), LatticeType::DateAndTime);
        assert!(LatticeType::from_str("error").is_err());
        assert_eq!(LatticeType::from_str("function(string)->number").unwrap(), LatticeType::function(vec![LatticeType::String], LatticeType::Number));
    }

}

