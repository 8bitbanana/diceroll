# diceroll

A DnD dice syntax parser I wrote last year.

## Stuff you can do:
 - Roll a dice: `1d8`
 - Do maths: `d8+2`, `d8-2`, `d8*2`, `d8/2`, `d8^2`
 - Use brackets: `(d8+2)*2d4`
 - Fudge dice: `2d8+4dF`
 - A few functions: `floor(...)`, `ceil(...)`, `round(...)`, `abs(...)`
 
## Dice modifiers you can use:
 - Drop lowest: `4d6d1`, `2d20dl1`
 - Drop highest: `2d20dh1`
 - Keep highest: `2d20k1`, `3d10kh1`
 - Keep lowest: `2d20kl1`
 - Overwrite crit success: `d20cs>=19`, `d20cs14,20`, `4d8csn`
 - Overwrite crit fails: `d20cf<=3`, `d20cs1,10`, `4d8cfn`
 - Reroll dice: `d8r1`, `d20r<=10`
 - Set results in a range to a value: `10d8w1=2`, `d20w<10=10`
 - Explode: `d8!`, `d8!>=4`, `d8!1`
 - Sort acending/decending: `10d20sa`, `10d20sd`
