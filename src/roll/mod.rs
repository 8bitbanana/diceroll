mod tokens;
mod lexer;
pub mod parser;
pub mod interpreter;

use std::{fmt};

pub use interpreter::InterpReturn as Output;

use interpreter::InterpError;
pub use interpreter::{ReturnExpr, ReturnLiteral, DiceRoll, FudgeRoll, Roll};

use crate::roll::interpreter::ResolvedVariables;

use self::interpreter::UnresolvedVariables;

use serde::{Serialize, Deserialize};

#[derive(Serialize, Deserialize)]
#[serde(tag = "t", content="c")]
#[derive(Debug, Clone)]
pub enum ParseError {
    Lex(usize),
    Parse(usize)
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParseError::Lex(location) => {
                write!(f, "Lex error at {}", location)
            },
            ParseError::Parse(location) => {
                write!(f, "Parse error at {}", location)
            }
        }
    }
}

#[derive(Serialize, Deserialize)]
#[serde(tag = "t", content="c")]
#[derive(Debug, Clone)]
pub enum RuntimeError {
    Interpret(String),
    Timeout
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RuntimeError::Interpret(details) => {
                write!(f, "Interpret error - \"{}\"", details)
            },
            RuntimeError::Timeout => {
                write!(f, "Timeout")
            }
        }
    }
}

#[derive(Serialize, Deserialize)]
#[serde(tag = "t", content="c")]
#[derive(Debug, Clone)]
pub enum FullError {
    Parse(ParseError),
    Runtime(RuntimeError)
}

impl std::fmt::Display for FullError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Parse(err) => err.fmt(f),
            Self::Runtime(err) => err.fmt(f)
        }
    }
}

impl std::convert::From<InterpError> for RuntimeError {
    fn from(err: InterpError) -> Self {
        match err {
            InterpError::Runtime(msg) => RuntimeError::Interpret(msg),
            InterpError::Timeout => RuntimeError::Timeout
        }
    }
}

impl std::convert::From<ParseError> for FullError {
    fn from(err: ParseError) -> Self {
        Self::Parse(err)
    }
}

impl std::convert::From<RuntimeError> for FullError {
    fn from(err: RuntimeError) -> Self {
        Self::Runtime(err)
    }
}

pub fn run(input: &str, variables: &ResolvedVariables) -> Result<Output, FullError> {
    let parsed = match parse(input) {
        Ok((parsed, _deps)) => parsed,
        Err(err) => {return Err(err.into());}
    };
    Ok(run_parsed(&parsed, variables)?)
}

pub fn parse(input: &str) -> Result<(parser::Expr, UnresolvedVariables), ParseError> {
    let (lex_result, input_map, unresolved_variables) = match lexer::lex(input) {
        Ok(x) => x,
        Err((i, _)) => {
            return Err(ParseError::Lex(i));
        }
    };
    let parse_result = match parser::parse(&lex_result) {
        Ok(x) => x,
        Err((remaining, _)) => {
            let location = lex_result.len() - remaining.len();
            // Map the location from lexed tokens to the actual input string
            let mut current_pos = 0;
            #[allow(clippy::clippy::needless_range_loop)]
            for index in 0..location {
                current_pos += input_map[index].len();
            }
            return Err(ParseError::Parse(current_pos));
        }
    };

    Ok((parse_result, unresolved_variables))
}

pub fn run_parsed(expr: &parser::Expr, variables: &ResolvedVariables) -> Result<Output, RuntimeError> {
    let context = interpreter::InterpretContext {
        variables
    };
    let intp_result = match interpreter::interpret(expr, &context) {
        Ok(x) => x,
        Err(e) => {
            return Err(e.into());
        }
    };

    Ok(intp_result)
}

pub fn run_stubbed(input: &str) -> Result<Output, FullError>
{
    let (parse_result, unresolved_variables) = parse(input)?;
    //println!("{:?}", parse_result);

    let mut resolved_variables = ResolvedVariables::new();
    for key in unresolved_variables.iter() {
        resolved_variables.add_resolved(key.clone(), interpreter::InterpReturn::fixed(0.0));
    }

    let intp_result = run_parsed(&parse_result, &resolved_variables)?;

    Ok(intp_result)
}