# AOC 2023 day 3 in Uiua

Fix! ← ⍢∘(¬≍, ⊃∘^1) # Compute fixpoint
Shl ← ⍜⍉(⬚0↻ :) :   # Shift left
BlurHoriz ← ↥↥⊃(Shl¯1)(Shl1).
Blur ← ↥↥⊃(⬚0↻¯1)(⬚0↻1). BlurHoriz

Input ← ⊜∘≠, @\n &fras "input"
DigitMask ← ↧ ≥@0:≤@9. Input
Digits ← ×DigitMask -@0 Input
Symbols ← ¬↥ DigitMask =@.Input

NumStartMask ← ↧DigitMask ¬ Shl1 DigitMask
Nums ← ×NumStartMask Fix!(×DigitMask +Digits ×10Shl¯1) Digits

# Part 1: Mark all cells that neighbor a symbol
Mask ← Fix!(↥ BlurHoriz ↧DigitMask.) Blur Symbols
/+♭ ×Mask Nums

# Part 2: Collect numbers in the 3x3 regions around *s
Displace ← ⍜⍉(↥0 ↧-1△Input +) ↯9_2 ⊠⊂.¯1_0_1
SmearedNums ← Fix!(×DigitMask BlurHoriz) Nums
/+ ≡((0|/×) =2⧻. ⊝ ▽>0. ⊡ Displace :SmearedNums) ⊚=@*Input
