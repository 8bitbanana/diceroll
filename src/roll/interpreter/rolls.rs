use std::{ops::{Bound}};

use rand::{Rng, distributions::Standard, prelude::Distribution};

use super::{returns::{InterpRangeUnion, InterpError}, to_signed};

#[derive(Clone)]
enum CritBoundOption {
    Set(InterpRangeUnion),
    Unset,
    None
}


pub trait Roll: std::fmt::Display {
    fn get_ignored(&self) -> bool;
    fn set_ignored(&mut self, value: bool);

    fn get_value(&self) -> f64;
    fn set_value(&mut self, value: f64);
    fn get_bounds(&self) -> (f64, f64);
    fn fmt_inner(&self) -> String;

    fn is_crit_success(&self) -> bool;
    fn is_crit_fail(&self) -> bool;

    fn set_crit_success_bound(&mut self, bound: Option<InterpRangeUnion>);
    fn set_crit_fail_bound(&mut self, bound: Option<InterpRangeUnion>);

    fn extend_crit_success_bound(&mut self, bound: InterpRangeUnion);
    fn extend_crit_fail_bound(&mut self, bound: InterpRangeUnion);

    fn rerolled(&self) -> Self;

    fn fmt_debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.get_ignored() {
            write!(f, "[-{}-]", self.fmt_inner())
        } else if self.is_crit_success() {
            write!(f, "[|{}| Crit]", self.fmt_inner())
        } else if self.is_crit_fail() {
            write!(f, "[|{}| Fail]", self.fmt_inner())
        }  else {
            write!(f, "[|{}|]", self.fmt_inner())
        }
    }
}

#[derive(Clone)]
pub struct DiceRoll {
    value: f64,
    size: u64,
    ignored: bool,
    crit_success_bound: CritBoundOption,
    crit_fail_bound: CritBoundOption
}

impl DiceRoll {
    pub fn get_raw_size(&self) -> u64 { self.size }
}

impl std::fmt::Debug for DiceRoll {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.ignored {
            write!(f, "[.{}.]", self.value)
        } else {
            write!(f, "[|{}|]", self.value)
        }
    }
}

impl std::fmt::Display for DiceRoll {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.fmt_inner())
    }
}

impl Roll for DiceRoll {
    #[inline]
    fn get_value(&self) -> f64 {
        self.value
    }

    #[inline]
    fn set_value(&mut self, value: f64) {
        let (min, max) = self.get_bounds();
        if value >= max {
            self.value = max;
        } else if value <= min {
            self.value = min;
        } else {
            self.value = value;
        }
    }

    #[inline]
    fn get_bounds(&self) -> (f64, f64) {
        (1.0, self.size as f64)
    }

    fn fmt_inner(&self) -> String {
        self.value.to_string()
    }

    #[inline]
    fn get_ignored(&self) -> bool {
        self.ignored
    }

    #[inline]
    fn set_ignored(&mut self, value: bool) {
        self.ignored = value;
    }

    fn rerolled(&self) -> Self {
        let (cs, cf) = (&self.crit_success_bound, &self.crit_fail_bound);
        let mut new = Self::roll(self.size);
        new.crit_success_bound = cs.clone();
        new.crit_fail_bound = cf.clone();
        new
    }

    fn is_crit_success(&self) -> bool {
        match &self.crit_success_bound {
            CritBoundOption::Set(bound) => bound.contains(&to_signed(self.value)),
            CritBoundOption::Unset => {
                InterpRangeUnion::from((Bound::Included(self.size as i64), Bound::Unbounded)).contains(&to_signed(self.value))
            },
            CritBoundOption::None => false
        }
    }

    fn is_crit_fail(&self) -> bool {
        match &self.crit_fail_bound {
            CritBoundOption::Set(bound) => bound.contains(&to_signed(self.value)),
            CritBoundOption::Unset => {
                InterpRangeUnion::from((Bound::Unbounded, Bound::Included(1))).contains(&to_signed(self.value))
            },
            CritBoundOption::None => false
        }
    }

    fn set_crit_success_bound(&mut self, bound: Option<InterpRangeUnion>) {
        match bound {
            Some(bound) => self.crit_success_bound = CritBoundOption::Set(bound),
            None => self.crit_success_bound = CritBoundOption::None
        }
    }

    fn set_crit_fail_bound(&mut self, bound: Option<InterpRangeUnion>) {
        match bound {
            Some(bound) => self.crit_fail_bound = CritBoundOption::Set(bound),
            None => self.crit_fail_bound = CritBoundOption::None
        }
    }

    fn extend_crit_success_bound(&mut self, bound: InterpRangeUnion) {
        match &mut self.crit_success_bound {
            CritBoundOption::Set(current_bound) => {
                current_bound.extend(bound)
            },
            CritBoundOption::Unset => self.crit_success_bound = CritBoundOption::Set(bound),
            CritBoundOption::None => self.crit_success_bound = CritBoundOption::Set(bound)
        }
    }

    fn extend_crit_fail_bound(&mut self, bound: InterpRangeUnion) {
        match &mut self.crit_fail_bound {
            CritBoundOption::Set(current_bound) => {
                current_bound.extend(bound)
            },
            CritBoundOption::Unset => self.crit_fail_bound = CritBoundOption::Set(bound),
            CritBoundOption::None => self.crit_fail_bound = CritBoundOption::Set(bound)
        }
    }

    fn fmt_debug(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.get_ignored() {
            write!(f, "[-{}-]", self.fmt_inner())
        } else if self.is_crit_success() {
            write!(f, "[|{}| Crit]", self.fmt_inner())
        } else if self.is_crit_fail() {
            write!(f, "[|{}| Fail]", self.fmt_inner())
        }  else {
            write!(f, "[|{}|]", self.fmt_inner())
        }
    }
}

impl DiceRoll {
    fn roll(size: u64) -> Self {
        if size == 0 {
            return Self {
                value: 0.0,
                ignored: false,
                size: 0,
                crit_success_bound: CritBoundOption::Unset,
                crit_fail_bound: CritBoundOption::Unset
            };
        };
        let value = rand::thread_rng().gen_range(1..=size);
        Self {
            value: value as f64,
            ignored: false,
            size,
            crit_success_bound: CritBoundOption::Unset,
            crit_fail_bound: CritBoundOption::Unset
        }
    }
}


#[derive(serde::Serialize, serde::Deserialize)]
#[derive(Clone, Debug)]
pub enum FudgeValue {Negative, Zero, Positive}

impl Distribution<FudgeValue> for Standard {
    fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> FudgeValue {
        match rng.gen_range(0..=2) {
            0 => FudgeValue::Negative,
            1 => FudgeValue::Zero,
            _ => FudgeValue::Positive
        }
    }
}

impl std::fmt::Display for FudgeValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            FudgeValue::Negative => "-",
            FudgeValue::Zero => "0",
            FudgeValue::Positive => "+"
        })
    }
}

#[derive(Clone, Debug)]
pub struct FudgeRoll {
    value: FudgeValue,
    ignored: bool
}

impl FudgeRoll {
    pub fn get_raw_value(&self) -> FudgeValue {
        self.value.clone()
    }
}

impl std::fmt::Display for FudgeRoll {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.fmt_inner())
    }
}

impl Roll for FudgeRoll {
    #[inline]
    fn get_value(&self) -> f64 {
        match self.value {
            FudgeValue::Negative => -1.0,
            FudgeValue::Zero => 0.0,
            FudgeValue::Positive => 1.0,
        }
    }

    #[inline]
    fn set_value(&mut self, value: f64) {
        let value = value.round();
        if value <= -1.0 {
            self.value = FudgeValue::Negative;
        } else if value >= 1.0 {
            self.value = FudgeValue::Positive;
        } else {
            self.value = FudgeValue::Zero;
        }
    }

    #[inline]
    fn get_bounds(&self) -> (f64, f64) {
        (-1.0, 1.0)
    }

    fn fmt_inner(&self) -> String {
        match self.value {
            FudgeValue::Negative => "-",
            FudgeValue::Zero => "0",
            FudgeValue::Positive => "+"
        }.to_owned()
    }

    #[inline]
    fn get_ignored(&self) -> bool {
        self.ignored
    }

    #[inline]
    fn set_ignored(&mut self, value: bool) {
        self.ignored = value;
    }

    fn rerolled(&self) -> Self {
        Self::roll()
    }

    fn is_crit_success(&self) -> bool {
        false
    }

    fn is_crit_fail(&self) -> bool {
        false
    }

    // Fudge dice ignores crit success and fails.
    fn set_crit_success_bound(&mut self, _: Option<InterpRangeUnion>) {}
    fn set_crit_fail_bound(&mut self, _: Option<InterpRangeUnion>) {}
    fn extend_crit_success_bound(&mut self, _: InterpRangeUnion) {}
    fn extend_crit_fail_bound(&mut self, _: InterpRangeUnion) {}
}

impl FudgeRoll {
    pub fn roll() -> Self {
        Self {value: rand::random(), ignored: false}
    }
}

pub fn populate_dice(dice: &mut Vec<DiceRoll>, count: u64, size: u64) {
    if count == 0 {return; }   

    for _ in 0..count {
        dice.push(DiceRoll::roll(size));
    }
}

pub fn populate_fudge_dice(dice: &mut Vec<FudgeRoll>, count: u64) {
    if count == 0 {return;}

    for _ in 0..count {
        dice.push(FudgeRoll::roll());
    }
}

fn get_total_unignored<T: Roll>(dice: &[T]) -> u64 {
    let mut total: u64 = 0;
    for d in dice {
        if !d.get_ignored() {
            total+=1;
        }
    }
    total
}

pub enum HighOrLow {High, Low}

fn high_comp<T: Roll>(dice: &[T]) -> Option<usize> {
    let (result, _) = dice.iter().enumerate().fold(
        (None, f64::NEG_INFINITY), 
        |(i, a), (j, b)| {
            if !b.get_ignored() && b.get_value() > a {
                (Some(j), b.get_value())
            } else {
                (i, a)
            }
        }
    );
    result
}

fn low_comp<T: Roll>(dice: &[T]) -> Option<usize> {
    let (result, _) = dice.iter().enumerate().fold(
        (None, f64::INFINITY), 
        |(i, a), (j, b)| {
            if !b.get_ignored() && b.get_value() < a {
                (Some(j), b.get_value())
            } else {
                (i, a)
            }
        }
    );
    result
}


pub fn apply_drop<T: Roll>(dice: &mut Vec<T>, count: u64, hl: HighOrLow) -> Result<(), InterpError> {
    if count == 0 { return Ok(()); }

    for _ in 0..count {
        let result = match hl {
            HighOrLow::High => high_comp(dice),
            HighOrLow::Low => low_comp(dice)
        };
        if let Some(index) = result {
            dice[index].set_ignored(true);
        }
    }

    Ok(())
}

pub fn apply_keep<T: Roll>(dice: &mut Vec<T>, count: u64, hl: HighOrLow) -> Result<(), InterpError> {
    if count == 0 { return Ok(()); }

    let to_remove: u64 = u64::saturating_sub(get_total_unignored(dice), count);

    for _ in 0..to_remove {
        let result = match hl {
            HighOrLow::High => low_comp(dice),
            HighOrLow::Low => high_comp(dice)
        };
        if let Some(index) = result {
            dice[index].set_ignored(true);
        }
    }

    Ok(())
}

pub fn apply_reroll<T: Roll>(dice: &mut Vec<T>, range: InterpRangeUnion) -> Result<(), InterpError> {
    let mut indexes_to_reroll = vec![];
    for (i, d) in dice.iter_mut().enumerate() {
        if !d.get_ignored() && range.contains(&to_signed(d.get_value())) {
            indexes_to_reroll.push(i);
        }
    }
    for (offset, &index) in indexes_to_reroll.iter().enumerate() {
        let full_offset = index+offset;
        dice[full_offset].set_ignored(true);
        dice.insert(full_offset+1, dice[full_offset].rerolled());
    }

    Ok(())
}

pub fn apply_replace<T: Roll + Clone>(dice: &mut Vec<T>, range: InterpRangeUnion, value: f64) -> Result<(), InterpError> {
    let mut indexes_to_replace = vec![];
    for (i, d) in dice.iter_mut().enumerate() {
        if !d.get_ignored() && range.contains(&to_signed(d.get_value())) {
            indexes_to_replace.push(i);
        }
    }
    for (offset, &index) in indexes_to_replace.iter().enumerate() {
        let full_offset = index+offset;
        dice.insert(full_offset+1, dice[full_offset].clone());
        dice[full_offset].set_ignored(true);
        dice[full_offset+1].set_value(value);
    }

    Ok(())
}

pub fn apply_explode<T: Roll + Clone>(dice: &mut Vec<T>, range: Option<InterpRangeUnion>) -> Result<(), InterpError> {
    let mut offset = 0;
    for index in 0..dice.len() {
        let d = &dice[index];
        let range = range.clone().unwrap_or_else(
            || InterpRangeUnion::from((Bound::Included(d.get_bounds().1 as i64), Bound::Included(d.get_bounds().1 as i64)))
        );
        if !d.get_ignored() && range.contains(&to_signed(d.get_value())) {
            // Explode!!!
            loop {
                if offset > 10000 {
                    return Err(InterpError::Timeout);
                }

                let full_offset = index + offset;
                let new_dice = dice[full_offset].rerolled();
                let new_dice_value = new_dice.get_value();
                dice.insert(full_offset+1, new_dice);
                offset += 1;
                if !range.contains(&to_signed(new_dice_value)) {
                    break;
                }
            }
        }
    }

    Ok(())
}

pub fn apply_set_crit_success_bound<T: Roll>(dice: &mut Vec<T>, bound: Option<InterpRangeUnion>) -> Result<(), InterpError> {
    for d in dice {
        match &bound {
            Some(bound) => d.extend_crit_success_bound(bound.clone()),
            None => d.set_crit_success_bound(None)
        }
    }

    Ok(())
}

pub fn apply_set_crit_fail_bound<T: Roll>(dice: &mut Vec<T>, bound: Option<InterpRangeUnion>) -> Result<(), InterpError> {
    for d in dice {
        match &bound {
            Some(bound) => d.extend_crit_fail_bound(bound.clone()),
            None => d.set_crit_fail_bound(None)
        }
    }

    Ok(())
}

pub fn apply_sort_acending<T: Roll>(dice: &mut Vec<T>) {
    dice.sort_by(|a, b| a.get_value().partial_cmp(&b.get_value()).unwrap())
}

pub fn apply_sort_decending<T: Roll>(dice: &mut Vec<T>) {
    dice.sort_by(|a, b| b.get_value().partial_cmp(&a.get_value()).unwrap())
}