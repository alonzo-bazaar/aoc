# part 1 solution
# is 𝕩 an invalid id?
Invalid ← (⌊÷⟜2∘≠)⊸(↑≡↓)∘•Repr
# Invalid 123123 # => 1
# Invalid 123124 # => 0
# Invalid 12312  # => 0
# Invalid 1      # => 0
# Invalid 11     # => 1

# range of all numbers between 𝕨 and 𝕩, extremes included
RangeBetween ← (⊣+↕∘(1⊸+)∘-˜)
# 11 RangeBetween 22 -> 11‿12‿13‿...‿22

# all invalid IDs between 𝕨 and 𝕩, extremes included
Invalids ← (Invalid¨⊸/)∘RangeBetween

# from bqncrate
SplitOn ← {𝕨((¬-˜⊢×·+`»⊸>)∘≠⊔⊢)𝕩}

FileRanges ← (•ParseFloat¨¨)∘('-'⊸SplitOn¨)∘(','⊸SplitOn)∘•FChars

+´ +´¨ Invalids´¨ FileRanges "toyinput" # works
+´ +´¨ Invalids´¨ FileRanges "input"    # also works

# methlab for part 2 solution
AllEqual ← ∧´ {(⊏𝕩)⊸≡˘ 1↓𝕩}
IsNRepeats ← {(𝕨|≠•Repr𝕩)=0}◶⟨0, {AllEqual 𝕨‿∘ ⥊ •Repr 𝕩}⟩
IsRepeats ← {∨´ IsNRepeats⟜𝕩¨ 2+ ↕ 1-˜≠•Repr𝕩}

Invalid2 ← IsRepeats
Invalids2 ← (Invalid2¨⊸/)∘RangeBetween

+´ +´¨ Invalids2´¨ FileRanges "toyinput" # works
+´ +´¨ Invalids2´¨ FileRanges "input"    # also works (bit slow)
