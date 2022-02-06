use nom::{IResult, branch::alt, bytes::complete::{tag, tag_no_case, take}, character::{complete::{digit1, multispace0}}, combinator::{consumed, map}, error::{ErrorKind}, error_position, multi::many1, sequence::delimited};

use super::interpreter::UnresolvedVariables;

#[derive(PartialEq, Debug, Clone)]
pub enum Token {
    Illegal,
    Plus,
    Minus,
    Multiply,
    Divide,
    FixedLiteral(u64),
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
    Equals,
    LParen,
    RParen,
    ExclamationMark,
    Function(FunctionToken),
    Letter(char),
    Comma,
    Caret,
    Variable(String)
}

#[derive(PartialEq, Debug, Clone)]
pub enum FunctionToken {
    Floor, Ceil, Round, Abs, Min, Max
}

// #[derive(PartialEq, Debug, Clone)]
// pub struct Tokens<'a> {
//     pub tok: &'a [Token],
//     pub start: usize,
//     pub end: usize
// }

fn match_single_variable_char(input: &str) -> IResult<&str, char> {
    let (input, next_char) = take(1usize)(input)?;
    let next_char = next_char.chars().next().unwrap();
    match next_char {
        '[' | ']' => Err(nom::Err::Error(error_position!(input, ErrorKind::Tag))),
        _ => Ok((input, next_char))
    }
}

fn lex_variable(input: &str) -> IResult<&str, Token> {
    let (input, letters) = alt((
        delimited(
            tag("[["),
            many1(match_single_variable_char),
            tag("]]")
        ),
        delimited(
            tag("["),
            many1(match_single_variable_char),
            tag("]")
        )
    ))(input)?;
    Ok((input, Token::Variable(letters.into_iter().collect())))
}

fn lex_fixedliteral(input: &str) -> IResult<&str, Token> {
    let (input, digits) = digit1(input)?;
    match digits.parse() {
        Ok(x) => Ok((input, Token::FixedLiteral(x))),
        Err(_) => Err(nom::Err::Error(error_position!(input, ErrorKind::ParseTo)))
    }
}

fn lex_function_name(input: &str) -> IResult<&str, Token> {
    let (input, function) = alt((
        map(tag_no_case("floor"), |_| FunctionToken::Floor),
        map(tag_no_case("ceil"), |_| FunctionToken::Ceil),
        map(tag_no_case("round"), |_| FunctionToken::Round),
        map(tag_no_case("abs"), |_| FunctionToken::Abs),
        map(tag_no_case("min"), |_| FunctionToken::Min),
        map(tag_no_case("max"), |_| FunctionToken::Max)
    ))(input)?;
    Ok((input, Token::Function(function)))
}

fn lex_punctuation(input: &str) -> IResult<&str, Token> {
    alt((
        map(tag("=<"), |_| Token::LessThanOrEqual),
        map(tag("<="), |_| Token::LessThanOrEqual),
        map(tag(">="), |_| Token::GreaterThanOrEqual),
        map(tag("=>"), |_| Token::GreaterThanOrEqual),
        map(tag("<"), |_| Token::LessThan),
        map(tag(">"), |_| Token::GreaterThan),
        map(tag("!"), |_| Token::ExclamationMark),
        map(tag(","), |_| Token::Comma),
        map(tag("^"), |_| Token::Caret),
        map(tag("="), |_| Token::Equals),
        map(tag("+"), |_| Token::Plus),
        map(tag("-"), |_| Token::Minus),
        map(tag("*"), |_| Token::Multiply),
        map(tag("/"), |_| Token::Divide),
        map(tag("("), |_| Token::LParen),
        map(tag(")"), |_| Token::RParen)
    ))(input)
}

fn lex_char(input: &str) -> IResult<&str, Token> {

    let (input, c) = take(1usize)(input)?;

    let err = || nom::Err::Error(error_position!(input, ErrorKind::Alpha));

    let c = c.chars().next().ok_or_else(err)?;
    if c.is_alphabetic() {
        let c_lower = c.to_lowercase().next().ok_or_else(err)?;
        Ok((input, Token::Letter(c_lower)))
    } else {
        Err(err())
    }
}

#[allow(unused)]
fn lex_illegal(input: &str) -> IResult<&str, Token> {
    map(take(1usize), |_| Token::Illegal)(input)
}

fn lex_once(input: &str) -> IResult<&str, (String, Option<Token>)> {
    let mut consumed_input = String::new();

    let (input, (cons, _)) = consumed(multispace0)(input)?;
    consumed_input += cons;
    if input.is_empty() {
        return Ok((input, (consumed_input, None)))
    }

    let parser = alt((
        lex_variable,
        lex_function_name,
        lex_punctuation,
        lex_fixedliteral,
        lex_char,
        //lex_illegal
    ));
    let (input, (cons, token)) = consumed(parser)(input)?;
    consumed_input += cons;
    Ok((input, (consumed_input, Some(token))))
}

#[allow(clippy::type_complexity)] // shuddup clippy
pub fn lex(input: &str) -> Result<(Vec<Token>, Vec<String>, UnresolvedVariables), (usize, ErrorKind)> {
    let mut tokens = Vec::<Token>::new();
    let mut consumed_inputs = Vec::<String>::new();
    let mut unresolved_variables = UnresolvedVariables::new();
    let mut current_input = input;
    loop {
        match lex_once(current_input) {
            Ok((input, (consumed_input, token))) => {
                current_input = input;
                consumed_inputs.push(consumed_input);
                match token {
                    Some(token) => {
                        if let Token::Variable(key) = &token {
                            unresolved_variables.add(key.clone());
                        };
                        tokens.push(token);
                    },
                    None => {break;}
                }
            },
            Err(nom::Err::Error(e)) => {
                let pos = consumed_inputs.iter().fold(0, |t, x| t + x.len());
                return Err((pos, e.code));
            },
            _ => panic!()
        }
        if current_input.is_empty() {
            break;
        }
    }
    Ok((tokens, consumed_inputs, unresolved_variables))
}