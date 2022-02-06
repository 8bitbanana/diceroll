#![allow(clippy::vec_init_then_push)]

use std::{fmt, fmt::{Debug, Display}, ops::{Bound, RangeBounds}};

use crate::roll::interpreter::rolls::Roll;

use super::{DiceRoll, FudgeRoll, rolls::FudgeValue};

use serde::{Serialize, Deserialize};

//pub type InterpReturn = f64;

//pub type InterpReturn = (f64, Vec<ReturnExpr>);

#[derive(Debug, Clone)]
pub enum InterpError {
    #[allow(unused)]
    Runtime(String),
    Timeout
}

pub type InterpResult = Result<InterpReturn, InterpError>;

pub type InterpRange = (Bound<i64>, Bound<i64>);

#[derive(Debug, Clone)]
pub struct InterpRangeUnion(Vec<InterpRange>);

impl InterpRangeUnion {
    pub fn new() -> Self {
        Self(Vec::new())
    }

    pub fn add(&mut self, range: InterpRange) {
        self.0.push(range);
    }

    pub fn extend(&mut self, new_bound: Self) {
        self.0.extend(new_bound.0);
    }

    pub fn contains(&self, value: &i64) -> bool {
        for range in &self.0 {
            if range.contains(value) {return true;}
        }
        false
    }
}

impl Default for InterpRangeUnion {
    fn default() -> Self {
        Self::new()
    }
}

impl From<(Bound<i64>, Bound<i64>)> for InterpRangeUnion {
    fn from(value: (Bound<i64>, Bound<i64>)) -> Self {
        let mut x = Self::new();
        x.add(value);
        x
    }
}

#[derive(Serialize, Deserialize)]
#[derive(Debug, Clone)]
pub struct InterpReturn {
    pub total: f64,
    pub output: Vec<ReturnExpr>
}

impl InterpReturn {
    pub fn fixed(value: f64) -> Self {
        Self {
            total: value,
            output: vec![ReturnExpr::Literal(ReturnLiteral::Fixed(value))]
        }
    }

    pub fn variable(key: String, value: f64) -> Self {
        Self {
            total: value,
            output: vec![ReturnExpr::Literal(ReturnLiteral::Variable(key, value))]
        }
    }

    #[allow(unused)]
    pub fn roll(roll: DiceRoll) -> Self {
        Self {
            total: roll.get_value(),
            output: vec![ReturnExpr::Literal(roll.into())]
        }
    }

    pub fn function<F>(operation: F, function_name: ReturnExprString, args: &mut Vec<Self>) -> Self
        where F : FnOnce(Vec<f64>) -> f64 {
        let mut output = Vec::<ReturnExpr>::new();
        let mut argument_values = Vec::<f64>::new();
        output.push(ReturnExpr::String(function_name));
        output.push(ReturnExpr::String(ReturnExprString::LParen));

        for arg in args {
            argument_values.push(arg.total);
            output.append(arg.output.as_mut());
            output.push(ReturnExpr::String(ReturnExprString::Comma));
        }
        output.pop();

        output.push(ReturnExpr::String(ReturnExprString::RParen));
        Self {total: operation(argument_values), output}
    }

    pub fn combine<F>(operation: F, lhs: &mut Self, sep: ReturnExprString, rhs: &mut Self) -> Self 
        where F : FnOnce(f64, f64) -> f64 {
        let mut output = Vec::<ReturnExpr>::new();
        output.append(lhs.output.as_mut());
        output.push(ReturnExpr::String(sep));
        output.append(rhs.output.as_mut());
        Self {total: operation(lhs.total, rhs.total), output}
    }

    pub fn delimit<F>(operation: F, lhs: ReturnExprString, inner: &mut Self, rhs: ReturnExprString) -> Self 
        where F : FnOnce(f64) -> f64 {
        let mut output = Vec::<ReturnExpr>::new();
        output.push(ReturnExpr::String(lhs));
        output.append(inner.output.as_mut());
        output.push(ReturnExpr::String(rhs));
        Self {total: operation(inner.total), output}
    }

    pub fn prefix<F>(operation: F, prefix: ReturnExprString, inner: &mut Self) -> Self
        where F : FnOnce(f64) -> f64 {
        let mut output = Vec::<ReturnExpr>::new();
        output.push(ReturnExpr::String(prefix));
        output.append(inner.output.as_mut());
        Self {total: operation(inner.total), output}
    }

    pub fn add_parens(&mut self) -> Self {
        self.output.insert(0, ReturnExpr::String(ReturnExprString::LParen));
        self.output.push(ReturnExpr::String(ReturnExprString::RParen));
        self.to_owned()
    }
}

impl Display for InterpReturn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} -> ( ", self.total)?;
        for ret_expr in self.output.iter() {
            write!(f, "{}", ret_expr)?;
        }
        write!(f, " )")
    }
}

/*
Originally, anywhere this was used was just &'static str. Since
the only thing to go in those places would be const strings, it
worked out great. However, this needs to be Deserialize-able, that
isn't an option anymore.

One option is to just use String,
but then there would be tons of heap allocations when building these
expressions.

Instead, this enum is used, with strum used to provide AsRef, giving a 
&'static str when needed according to the strum(serialize) attribute.
*/
#[derive(Debug, Clone, Serialize, Deserialize)]
#[derive(strum_macros::Display, strum_macros::AsRefStr)]
#[allow(non_camel_case_types)]
pub enum ReturnExprString {
    #[strum(serialize = "(")] LParen,
    #[strum(serialize = ")")] RParen,
    #[strum(serialize = ",")] Comma,
    #[strum(serialize = " + ")] Plus_Delimit,
    #[strum(serialize = " - ")] Minus_Delimit,
    #[strum(serialize = " * ")] Star_Delimit,
    #[strum(serialize = " / ")] ForwardSlash_Delimit,
    #[strum(serialize = " ^ ")] Caret_Delimit,
    #[strum(serialize = "-")] Minus_NoDelimit,
    #[strum(serialize = "ceil")] Text_Ceil,
    #[strum(serialize = "floor")] Text_Floor,
    #[strum(serialize = "round")] Text_Round,
    #[strum(serialize = "abs")] Text_Abs,
    #[strum(serialize = "min")] Text_Min,
    #[strum(serialize = "max")] Text_Max
}

#[derive(Serialize, Deserialize)]
#[serde(tag = "t", content="c")]
#[derive(Debug, Clone)]
pub enum ReturnExpr {
    Literal(ReturnLiteral),
    String(ReturnExprString)
}

#[derive(Serialize, Deserialize)]
#[serde(tag = "t", content="c")]
#[derive(Debug, Clone)]
pub enum ReturnLiteral {
    Dice {
        value: f64,
        size: u64,
        ignored: bool,
        crit_success: bool,
        crit_fail: bool
    },
    Fixed(f64),
    Variable(String, f64),
    Fudge {
        value: FudgeValue,
        ignored: bool
    }
}

impl From<DiceRoll> for ReturnLiteral {
    fn from(roll: DiceRoll) -> Self {
        Self::Dice {
            value: roll.get_value(),
            size: roll.get_raw_size(),
            ignored: roll.get_ignored(),
            crit_success: roll.is_crit_success(),
            crit_fail: roll.is_crit_fail()
        }
    }
}

impl From<FudgeRoll> for ReturnLiteral {
    fn from(fudge: FudgeRoll) -> Self {
        Self::Fudge {
            value: fudge.get_raw_value(),
            ignored: fudge.get_ignored()
        }
    }
}

impl Display for ReturnExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ReturnExpr::Literal(value) => write!(f, "{}", value),
            ReturnExpr::String(value) => f.write_str(value.as_ref()),
        }
    }
}

impl Display for ReturnLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ReturnLiteral::Dice {value, ignored, crit_success, crit_fail, ..} => {
                let inner = value.to_string();
                if *ignored {
                    write!(f, "[-{}-]", inner)
                } else if *crit_success {
                    write!(f, "[|{}| Crit]", inner)
                } else if *crit_fail {
                    write!(f, "[|{}| Fail]", inner)
                }  else {
                    write!(f, "[|{}|]", inner)
                }
            },
            ReturnLiteral::Fixed(value) => {
                write!(f, "{}", value)
            },
            ReturnLiteral::Fudge {value, ignored} => {
                let inner = value.to_string();
                if *ignored {
                    write!(f, "[-{}-]", inner)
                } else {
                    write!(f, "[|{}|]", inner)
                }
            },
            ReturnLiteral::Variable(key, value) => {
                write!(f, "{} ({})", value, key)
            }
        }
    }
}