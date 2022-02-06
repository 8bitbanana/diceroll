

use nom::IResult;
use nom::bytes::complete::tag;
use nom::bytes::complete::take;
use nom::combinator::map;
use nom::combinator::opt;
use nom::error::ErrorKind;
use nom::error_position;
use nom::multi::many0;
use nom::multi::separated_list1;
use nom::sequence::delimited;
use nom::branch::alt;
use nom::sequence::pair;
use nom::sequence::preceded;
use nom::sequence::tuple;

use super::lexer::{Token, FunctionToken};

use super::tokens::Tokens;

#[derive(Debug, Clone)]
pub enum Literal {
    FixedLiteral(u64),
    VariableLiteral(String)
}

#[derive(Debug, Clone)]
pub enum Expr {
    InfixExpr(Infix, Box<Expr>, Box<Expr>),
    RollInfixExpr(Box<Expr>, RollExprRhs, Vec<RollMod>),
    RollPrefixExpr(RollExprRhs, Vec<RollMod>),
    LitExpr(Literal),
    PrefixExpr(Prefix, Box<Expr>),
    // ParenExpr does nothing special, but it adds parens in the formatted output.
    ParenExpr(Box<Expr>),
    FunctionExpr(FunctionType, Vec<Expr>)
}

#[derive(Debug, Clone)]
pub enum FunctionType {
    FunctionCeil,
    FunctionFloor,
    FunctionRound,
    FunctionAbs,
    FunctionMin,
    FunctionMax
}

#[derive(Debug, Clone)]
pub enum RollExprRhs {
    RhsExpr(Box<Expr>),
    RhsFudge
}

#[derive(Debug, Clone)]
pub enum Prefix {
    PrefixPlus,
    PrefixMinus,
}

#[derive(Debug, Clone)]
pub enum Infix {
    InfixPlus,
    InfixMinus,
    InfixMultiply,
    InfixDivide,
    InfixPowerOf
}

#[derive(PartialEq, PartialOrd, Debug, Clone)]
pub enum Precedence {
    PLowest,
    PSum,
    PProduct,
    PPowerOf,
    PRoll
}

#[derive(Debug, Clone)]
pub enum RangeExpr {
    Equals(Box<Expr>),
    LessThan(Box<Expr>),
    LessThanOrEqual(Box<Expr>),
    GreaterThan(Box<Expr>),
    GreaterThanOrEqual(Box<Expr>)
}

pub type RangeExprs = Vec<RangeExpr>;

#[derive(Debug, Clone)]
pub enum RollMod {
    Explode(Option<RangeExprs>), // 1d8!
    //CompoundExplode(Option<RangeExpr>), // 1d8!!
    //PenetratingExplode(Option<RangeExpr>), // 1d8!p
    DropLowest(Box<Expr>), // 1d8d1
    DropHighest(Box<Expr>), // 2d20dh1
    KeepLowest(Box<Expr>), // 2d20kl1
    KeepHighest(Box<Expr>), // 1d8k1
    CritSuccess(Option<RangeExprs>), // 1d8cs>6
    CritFail(Option<RangeExprs>), // 1d8cf<3
    Reroll(RangeExprs), // 1d8r<2
    Replace(RangeExprs,Box<Expr>), // 1d8<3=3
    SortAcending, // 4d20sa
    SortDecending // 4d20sd
}

#[macro_export]
macro_rules! tokens {
    ( $($Items:expr,)* ) => {
        Tokens::new(&[
            $($Items),*,
        ])
    };
    ( $($Items:expr),* ) => {
        tokens!($($Items,)*)
    }
}

fn parse_atom_expr(input: Tokens) -> IResult<Tokens, Expr> {
    alt((
        parse_literal_expr,
        parse_dice_prefix_expr,
        parse_prefix_expr,
        parse_function_expr,
        parse_paren_expr
    ))(input)
}

fn match_function_name(input: Tokens) -> IResult<Tokens, FunctionType> {
    let (input, token) = take(1usize)(input)?;
    let func_parsed = match &token[0] {
        Token::Function(func) => {
            match func {
                FunctionToken::Abs => FunctionType::FunctionAbs,
                FunctionToken::Ceil => FunctionType::FunctionCeil,
                FunctionToken::Floor => FunctionType::FunctionFloor,
                FunctionToken::Round => FunctionType::FunctionRound,
                FunctionToken::Min => FunctionType::FunctionMin,
                FunctionToken::Max => FunctionType::FunctionMax
            }
        },
        _ => return Err(nom::Err::Error(error_position!(input, ErrorKind::Tag)))
    };
    Ok((input, func_parsed))
}

fn is_valid_argument_count(function: &FunctionType, count: usize) -> bool {
    match function {
        FunctionType::FunctionCeil => count == 1,
        FunctionType::FunctionFloor => count == 1,
        FunctionType::FunctionRound => count == 1,
        FunctionType::FunctionAbs => count == 1,
        FunctionType::FunctionMin => count >= 1,
        FunctionType::FunctionMax => count >= 1,
    }
}

fn parse_function_expr(input: Tokens) -> IResult<Tokens, Expr> {
    let (input, function) = match_function_name(input)?;
    let (input, _) = tag(tokens!(Token::LParen))(input)?;
    let (input, arguments) = separated_list1(
        tag(tokens!(Token::Comma)), 
        parse_expr
    )(input)?;
    let (input, _) = opt(tag(tokens!(Token::Comma)))(input)?;
    let (input, _) = tag(tokens!(Token::RParen))(input)?;

    if !is_valid_argument_count(&function, arguments.len()) {
        return Err(nom::Err::Failure(error_position!(input, ErrorKind::ManyMN)));
    }

    Ok((input, Expr::FunctionExpr(function, arguments)))
}

fn parse_paren_expr(input: Tokens) -> IResult<Tokens, Expr> {
    let (input, expr) = delimited(tag(tokens!(Token::LParen)), parse_expr, tag(tokens!(Token::RParen)))(input)?;
    Ok((input, Expr::ParenExpr(expr.into())))
}

fn parse_dice_prefix_expr(input: Tokens) -> IResult<Tokens, Expr> {
    let (input, _) = tag(tokens!(Token::Letter('d')))(input)?;

    let fudge_tag_result: IResult<Tokens, Tokens> = tag(tokens!(Token::Letter('f')))(input.clone());
    let (input, right) = if let Ok((input, _)) = fudge_tag_result {
        (input, RollExprRhs::RhsFudge)
    } else if let Ok((input, right)) = parse_atom_expr(input.clone()) {
        (input, RollExprRhs::RhsExpr(right.into()))
    } else {
        return Err(nom::Err::Error(error_position!(input, ErrorKind::Alt)));
    };
    
    let (input, dicemods) = many0(parse_dicemod)(input)?;
    Ok((input, Expr::RollPrefixExpr(right, dicemods)))
}

fn parse_prefix_expr(input: Tokens) -> IResult<Tokens, Expr> {
    map(
        pair(match_prefix_token, parse_atom_expr),
        |(prefix, expr)| Expr::PrefixExpr(prefix, expr.into())
    )(input)
}

fn match_prefix_token(input: Tokens) -> IResult<Tokens, Prefix> {
    alt((
        map(tag(tokens!(Token::Plus)), |_| Prefix::PrefixPlus),
        map(tag(tokens!(Token::Minus)), |_| Prefix::PrefixMinus)
    ))(input)
}

fn parse_variable_literal_expr(input: Tokens) -> IResult<Tokens, Expr> {
    let (input, next_token) = take(1usize)(input)?;
    match &next_token[0] {
        Token::Variable(key) => Ok((input, Expr::LitExpr(Literal::VariableLiteral(key.clone())))),
        _ => Err(nom::Err::Error(error_position!(input, ErrorKind::Tag)))
    }
}

fn parse_fixed_literal_expr(input: Tokens) -> IResult<Tokens, Expr> {
    let (input, nexttoken) = take(1usize)(input)?;
    match nexttoken[0] {
        Token::FixedLiteral(amount) => {
            Ok((input, Expr::LitExpr(Literal::FixedLiteral(amount))))
        }
        _ => Err(nom::Err::Error(error_position!(input, ErrorKind::Tag)))
    }
}

fn parse_literal_expr(input: Tokens) -> IResult<Tokens, Expr> {
    alt((
        parse_fixed_literal_expr,
        parse_variable_literal_expr
    ))(input)
}

fn parse_pratt_expr(input: Tokens, precedence: Precedence) -> IResult<Tokens, Expr> {
    let (input, left) = parse_atom_expr(input)?;
    go_parse_pratt_expr(input, precedence, left)
}

fn go_parse_pratt_expr(input: Tokens, precedence: Precedence, left: Expr) -> IResult<Tokens, Expr> {
    if input.len() > 0 {
        let preview = input[0].clone();
        match infix_op(&preview) {
            (ref peek_precedence, _) if precedence < *peek_precedence => {
                // alt() doesn't work with the two input arguments (input, left).
                // We have to replicate alt() manually instead.
                if let Ok((input, left)) = parse_dice_infix_expr(input.clone(), left.clone()) {
                    return go_parse_pratt_expr(input, precedence, left)
                };
                if let Ok((input, left)) = parse_infix_expr(input.clone(), left) {
                    return go_parse_pratt_expr(input, precedence, left)
                };
                Err(nom::Err::Failure(error_position!(input, ErrorKind::Alt)))
            }
            _ => Ok((input, left))
        }
    } else {
        Ok((input, left))
    }
}

fn parse_range_expr(input: Tokens) -> IResult<Tokens, RangeExprs> {
    separated_list1(tag(tokens!(Token::Comma)), parse_single_range_expr)(input)
}

fn parse_range_atom(input: Tokens) -> IResult<Tokens, Expr> {
    alt((
        parse_paren_expr,
        parse_literal_expr
    ))(input)
}

fn parse_single_range_expr(input: Tokens) -> IResult<Tokens, RangeExpr> {
    let (input, (token, expr)) = pair(
        opt(
            alt((
                tag(tokens!(Token::Equals)),
                tag(tokens!(Token::GreaterThan)),
                tag(tokens!(Token::GreaterThanOrEqual)),
                tag(tokens!(Token::LessThan)),
                tag(tokens!(Token::LessThanOrEqual))
            ))
        ),
        parse_range_atom
    )(input)?;
    Ok((input, match token {
        Some(token) => {
            match token[0] {
                Token::Equals => RangeExpr::Equals(expr.into()),
                Token::GreaterThan => RangeExpr::GreaterThan(expr.into()),
                Token::GreaterThanOrEqual => RangeExpr::GreaterThanOrEqual(expr.into()),
                Token::LessThan => RangeExpr::LessThan(expr.into()),
                Token::LessThanOrEqual => RangeExpr::LessThanOrEqual(expr.into()),
                _ => unreachable!()
            }
        },
        None => {
            RangeExpr::Equals(expr.into())
        }
    }))
}

fn parse_dicemod(input: Tokens) -> IResult<Tokens, RollMod> {
    alt((
        // map(tag(tokens!(Token::ExclamationMark, Token::ExclamationMark)), |_| RollMod::CompoundExplode),
        // map(tag(tokens!(Token::ExclamationMark, Token::Char('p'))), |_| RollMod::PenetratingExplode),
        // map(tag(tokens!(Token::ExclamationMark)), |_| RollMod::Explode),
        map(tag(tokens!(Token::Letter('s'), Token::Letter('a'))), |_| RollMod::SortAcending),
        map(tag(tokens!(Token::Letter('s'), Token::Letter('d'))), |_| RollMod::SortDecending),
        map(
            preceded(
                tag(tokens!(Token::ExclamationMark)),
                opt(parse_range_expr)
            ),
            |range| {
                RollMod::Explode(range)
            }
        ),
        map(
            preceded(
                tag(tokens!(Token::Letter('w'))),
                tuple((parse_range_expr, tag(tokens!(Token::Equals)), parse_range_atom)),
            ),
            |(range, _, expr)| {
                RollMod::Replace(range, expr.into())
            }
        ),
        map(
            preceded(
                tag(tokens!(Token::Letter('d'), Token::Letter('h'))),
                parse_range_atom
            ), 
            |expr| RollMod::DropHighest(expr.into())
        ),
        map(
            preceded(
                tag(tokens!(Token::Letter('k'), Token::Letter('l'))),
                parse_range_atom
            ), 
            |expr| RollMod::KeepLowest(expr.into())
        ),
        map(
            preceded(
                alt((
                    tag(tokens!(Token::Letter('d'), Token::Letter('l'))),
                    tag(tokens!(Token::Letter('d')))
                )),
                parse_range_atom
            ), 
            |expr| RollMod::DropLowest(expr.into())
        ),
        map(
            preceded(
                alt((
                    tag(tokens!(Token::Letter('k'), Token::Letter('h'))),
                    tag(tokens!(Token::Letter('k')))
                )),
                parse_range_atom
            ), 
            |expr| RollMod::KeepHighest(expr.into())
        ),
        map(
            tag(tokens!(Token::Letter('c'), Token::Letter('s'), Token::Letter('n'))),
            |_| RollMod::CritSuccess(None)
        ),
        map(
            tag(tokens!(Token::Letter('c'), Token::Letter('f'), Token::Letter('n'))),
            |_| RollMod::CritFail(None)
        ),
        map(
            preceded(tag(tokens!(Token::Letter('c'), Token::Letter('s'))), parse_range_expr), 
            |range| RollMod::CritSuccess(Some(range))
        ),
        map(
            preceded(tag(tokens!(Token::Letter('c'), Token::Letter('f'))), parse_range_expr), 
            |range| RollMod::CritFail(Some(range))
        ),
        map(
            preceded(tag(tokens!(Token::Letter('r'))), parse_range_expr), 
            RollMod::Reroll
        )
    ))(input)
}

fn parse_dice_infix_expr(input: Tokens, left: Expr) -> IResult<Tokens, Expr> {
    let (input, _) = tag(tokens!(Token::Letter('d')))(input)?;

    let fudge_tag_result: IResult<Tokens, Tokens> = tag(tokens!(Token::Letter('f')))(input.clone());
    let (input, right) = if let Ok((input, _)) = fudge_tag_result {
        (input, RollExprRhs::RhsFudge)
    } else if let Ok((input, right)) = parse_pratt_expr(input.clone(), Precedence::PRoll) {
        (input, RollExprRhs::RhsExpr(right.into()))
    } else {
        return Err(nom::Err::Error(error_position!(input, ErrorKind::Alt)));
    };

    let (input, mods) = many0(parse_dicemod)(input)?;
    Ok((input, Expr::RollInfixExpr(left.into(), right, mods)))
}

fn parse_infix_expr(input: Tokens, left: Expr) -> IResult<Tokens, Expr> {
    let (input, t1) = take(1usize)(input)?;
    let next = t1[0].clone();
    let (precedence, maybe_op) = infix_op(&next);
    match maybe_op {
        None => Err(nom::Err::Error(error_position!(input, ErrorKind::Tag))),
        Some(op) => {
            let (input, right) = parse_pratt_expr(input, precedence)?;
            Ok((input, Expr::InfixExpr(op, left.into(), right.into())))
        }
    }
}

fn infix_op(preview: &Token) -> (Precedence, Option<Infix>) {
    match preview {
        Token::Plus => (Precedence::PSum, Some(Infix::InfixPlus)),
        Token::Minus => (Precedence::PSum, Some(Infix::InfixMinus)),
        Token::Multiply => (Precedence::PProduct, Some(Infix::InfixMultiply)),
        Token::Divide => (Precedence::PProduct, Some(Infix::InfixDivide)),
        Token::Caret => (Precedence::PPowerOf, Some(Infix::InfixPowerOf)),
        Token::Letter('d') => (Precedence::PRoll, None),
        _ => (Precedence::PLowest, None)
    }
}

fn parse_expr(input: Tokens) -> IResult<Tokens, Expr> {
    parse_pratt_expr(input, Precedence::PLowest)
}


pub fn parse<'a>(input: &'a [Token]) -> Result<Expr, (Tokens<'a>, ErrorKind)> {
    let input: Tokens<'a> = Tokens::new(input);
    let result = parse_expr(input);
    match result {
        Ok((input, expr)) => {
            if input.is_empty() {
                Ok(expr)
            } else {
                Err((input, ErrorKind::Complete))
            }
        },
        Err(nom::Err::Error(e)) => Err((e.input.to_owned(), e.code)),
        Err(nom::Err::Failure(e)) => Err((e.input.to_owned(), e.code)),
        _ => panic!()
    }   
}
