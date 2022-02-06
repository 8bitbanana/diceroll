use std::{ops::{Bound}};

use super::{parser::{Expr, FunctionType, Infix, Literal, Prefix, RangeExpr, RollExprRhs, RollMod}};

mod rolls;
use rolls::{populate_dice, populate_fudge_dice};
pub use rolls::{DiceRoll, FudgeRoll, Roll};

mod returns;
pub use returns::{InterpResult, InterpReturn, InterpError, ReturnExpr, ReturnLiteral};
use returns::*;

mod variables;
pub use variables::{ResolvedVariables, UnresolvedVariables};

#[derive(Debug)]
pub struct InterpretContext<'a> {
    pub variables: &'a ResolvedVariables
}

fn to_unsigned(input: f64) -> u64 {
    input.round() as u64
}

fn to_signed(input: f64) -> i64 {
    input.round() as i64
}

fn calculate_single_range(range_expr: &RangeExpr, context: &InterpretContext) -> Result<InterpRange, InterpError> {
    Ok(match range_expr {
        RangeExpr::Equals(expr) => {
            let value = run_expr(expr, context)?.total;
            (Bound::Included(to_signed(value)), Bound::Included(to_signed(value)))
        },
        RangeExpr::LessThan(expr) => {
            (Bound::Unbounded, Bound::Excluded(to_signed(run_expr(expr, context)?.total)))
        },
        RangeExpr::LessThanOrEqual(expr) => {
            (Bound::Unbounded, Bound::Included(to_signed(run_expr(expr, context)?.total)))
        },
        RangeExpr::GreaterThan(expr) => {
            (Bound::Excluded(to_signed(run_expr(expr, context)?.total)), Bound::Unbounded)
        },
        RangeExpr::GreaterThanOrEqual(expr) => {
            (Bound::Included(to_signed(run_expr(expr, context)?.total)), Bound::Unbounded)
        },
    })
}

fn calculate_range(in_ranges: &[RangeExpr], context: &InterpretContext) -> Result<InterpRangeUnion, InterpError> {
    let mut ranges = InterpRangeUnion::new();
    for in_range in in_ranges {
        ranges.add(calculate_single_range(in_range, context)?);
    }
    Ok(ranges)
}

#[allow(clippy::unnecessary_wraps)]
fn do_fudge_roll(count: f64, mods: &[RollMod], context: &InterpretContext) -> InterpResult {
    let count = to_unsigned(count);

    let mut dice: Vec<FudgeRoll> = Vec::new();

    populate_fudge_dice(&mut dice, count);
    apply_dicemods::<FudgeRoll>(&mut dice, mods, context)?;

    let mut total: f64 = 0.0;
    for d in &dice {
        if !d.get_ignored() {
            total += d.get_value();
        }
    }
    Ok(
        InterpReturn {
            total,
            output: dice.into_iter().map(|d| ReturnExpr::Literal(d.into())).collect()
        }
    )
}

fn apply_dicemods<T: Roll + Clone>(dice: &mut Vec<T>, mods: &[RollMod], context: &InterpretContext) -> Result<(), InterpError> {
    for dicemod in mods {
        use rolls::*;
        match dicemod {
            RollMod::DropLowest(expr) => {
                apply_drop(dice, to_unsigned(run_expr(expr, context)?.total), HighOrLow::Low)?;
            }
            RollMod::KeepHighest(expr) => {
                apply_keep(dice, to_unsigned(run_expr(expr, context)?.total), HighOrLow::High)?;
            }
            RollMod::DropHighest(expr) => {
                apply_drop(dice, to_unsigned(run_expr(expr, context)?.total), HighOrLow::High)?;
            }
            RollMod::KeepLowest(expr) => {
                apply_keep(dice, to_unsigned(run_expr(expr, context)?.total), HighOrLow::Low)?;
            }
            RollMod::Reroll(range) => {
                apply_reroll(dice, calculate_range(range, context)?)?;
            },
            RollMod::Replace(range, expr) => {
                apply_replace(dice, calculate_range(range, context)?, run_expr(expr, context)?.total)?;
            },
            RollMod::Explode(range) => {
                match range {
                    Some(range) => apply_explode(dice, Some(calculate_range(range, context)?))?,
                    None => apply_explode(dice, None)?
                }
            },
            RollMod::CritSuccess(Some(range)) => {
                apply_set_crit_success_bound(dice, Some(calculate_range(range, context)?))?
            },
            RollMod::CritSuccess(None) => {
                apply_set_crit_success_bound(dice, None)?
            },
            RollMod::CritFail(Some(range)) => {
                apply_set_crit_fail_bound(dice, Some(calculate_range(range, context)?))?
            },         
            RollMod::CritFail(None) => {
                apply_set_crit_fail_bound(dice, None)?
            },
            RollMod::SortAcending => {
                apply_sort_acending(dice)
            },
            RollMod::SortDecending => {
                apply_sort_decending(dice)
            }
        }
    }
    Ok(())
}

fn do_roll(count: f64, size: f64, mods: &[RollMod], context: &InterpretContext) -> InterpResult {
    let count = to_unsigned(count);
    let size = to_unsigned(size);

    let mut dice: Vec<DiceRoll> = Vec::new();

    populate_dice(&mut dice, count, size);
    apply_dicemods(&mut dice, mods, context)?;

    let mut total: f64 = 0.0;
    for d in &dice {
        if !d.get_ignored() {
            total += d.get_value();
        }
    }

    Ok(
        InterpReturn {
            total,
            output: dice.into_iter().map(|d| ReturnExpr::Literal(d.into())).collect()
        }
    )
}

fn run_literal(literal: &Literal, context: &InterpretContext) -> InterpResult {
    match literal {
        Literal::FixedLiteral(value) => Ok(InterpReturn::fixed(*value as f64)),
        Literal::VariableLiteral(key) => {
            match context.variables.get(key) {
                Some(value) => Ok(InterpReturn::variable(key.clone(), value.total)),
                None => Err(InterpError::Runtime(format!("Unresolved variable \"{}\"", key)))
            }
        }
    }
}

fn run_infix(infix: &Infix, left: &Expr, right: &Expr, context: &InterpretContext) -> InterpResult {
    match infix {
        Infix::InfixPlus => {
            Ok(InterpReturn::combine(
                |a,b| a + b, 
                &mut run_expr(left, context)?,
                ReturnExprString::Plus_Delimit, 
                &mut run_expr(right, context)?)
            )
        },
        Infix::InfixMinus => {
            Ok(InterpReturn::combine(
                |a,b| a - b, 
                &mut run_expr(left, context)?,
                ReturnExprString::Minus_Delimit, 
                &mut run_expr(right, context)?)
            )
        },
        Infix::InfixMultiply => {
            Ok(InterpReturn::combine(
                |a,b| a * b, 
                &mut run_expr(left, context)?,
                ReturnExprString::Star_Delimit, 
                &mut run_expr(right, context)?)
            )
        },
        Infix::InfixDivide => {
            Ok(InterpReturn::combine(
                |a,b| a / b, 
                &mut run_expr(left, context)?,
                ReturnExprString::ForwardSlash_Delimit, 
                &mut run_expr(right, context)?)
            )
        },
        Infix::InfixPowerOf => {
            Ok(InterpReturn::combine(
                |a, b| a.powf(b),
                &mut run_expr(left, context)?, 
                ReturnExprString::Caret_Delimit, 
                &mut run_expr(right, context)?)
            )
        }
    }
}

fn run_roll_infix(left: &Expr, right: &RollExprRhs, mods: &[RollMod], context: &InterpretContext) -> InterpResult {
    match right {
        RollExprRhs::RhsExpr(right) => {
            do_roll(run_expr(left, context)?.total, run_expr(right, context)?.total, mods, context)
        },
        RollExprRhs::RhsFudge => {
            do_fudge_roll(run_expr(left, context)?.total, mods, context)
        }
    }
}


fn run_prefix(prefix: &Prefix, inner: &Expr, context: &InterpretContext) -> InterpResult {
    match prefix {
        Prefix::PrefixPlus => run_expr(inner, context),
        Prefix::PrefixMinus => Ok(
            InterpReturn::prefix(
                |x| -x, 
                ReturnExprString::Minus_NoDelimit, 
                &mut run_expr(inner, context)?
            ))
    }
}

fn run_function_args(arguments: &[Expr], context: &InterpretContext) -> Result<Vec<InterpReturn>, InterpError> {
    let mut returns = Vec::new();
    for arg in arguments.iter() {
        returns.push(run_expr(arg, context)?);
    }
    Ok(returns)
}

fn cmp_f64(a: &f64, b: &f64) -> std::cmp::Ordering {
    a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal)
}

fn run_function(func: &FunctionType, arguments: &[Expr], context: &InterpretContext) -> InterpResult {
    match func {
        FunctionType::FunctionCeil => Ok(
            InterpReturn::function(
                |args| args[0].ceil(),
                ReturnExprString::Text_Ceil,
                run_function_args(arguments, context)?.as_mut()
            )
        ),
        FunctionType::FunctionFloor => Ok(
            InterpReturn::function(
                |args| args[0].floor(),
                ReturnExprString::Text_Floor,
                run_function_args(arguments, context)?.as_mut()
            )
        ),
        FunctionType::FunctionRound => Ok(
            InterpReturn::function(
                |args| args[0].round(),
                ReturnExprString::Text_Round,
                run_function_args(arguments, context)?.as_mut()
            )
        ),
        FunctionType::FunctionAbs => Ok(
            InterpReturn::function(
                |args| args[0].abs(),
                ReturnExprString::Text_Abs,
                run_function_args(arguments, context)?.as_mut()
            )
        ),
        FunctionType::FunctionMin => Ok(
            InterpReturn::function(
                |args| args.into_iter().min_by(cmp_f64).unwrap(),
                ReturnExprString::Text_Min,
                run_function_args(arguments, context)?.as_mut()
            )
        ),
        FunctionType::FunctionMax => Ok(
            InterpReturn::function(
                |args| args.into_iter().max_by(cmp_f64).unwrap(),
                ReturnExprString::Text_Max,
                run_function_args(arguments, context)?.as_mut()
            )
        )
    }
}

fn run_expr(root: &Expr, context: &InterpretContext) -> InterpResult {
    match root {
        Expr::InfixExpr(infix, left, right) => run_infix(infix, left, right, context),
        Expr::LitExpr(literal) => run_literal(literal, context),
        Expr::PrefixExpr(prefix, inner) => run_prefix(prefix, inner, context),
        Expr::ParenExpr(inner) => Ok(run_expr(inner, context)?.add_parens()),
        Expr::RollInfixExpr(left, right, mods) => Ok(run_roll_infix(left, right, mods, context)?),
        Expr::RollPrefixExpr(inner, mods) => Ok(run_roll_infix(&Expr::LitExpr(Literal::FixedLiteral(1)), inner, mods, context)?),
        Expr::FunctionExpr(func, args) => Ok(run_function(func, args, context)?)
    }
}

pub fn interpret(input: &Expr, context: &InterpretContext) -> InterpResult {
    run_expr(input, context)
}
