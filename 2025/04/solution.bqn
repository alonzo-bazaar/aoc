Pad ← {
    𝕊𝕩 :(-≠+1˙)⊸↑ (≠+1˙)⊸↑ 𝕩 ; 
    𝕊⁼𝕩:(-≠-1˙)⊸↑ (≠-1˙)⊸↑ 𝕩
}

# back to the valid regex ig
Sol1 ← {
  +˝ 4≥ +´˘ '@'= ('@'⊸=∘(4⊸⊑))˘⊸/ ∘‿9 ⥊ 3‿3 ↕ Pad Pad˘ > •FLines 𝕩
}

Sol1 "toyinput"
Sol1 "input"

# let's see if greedy works
# at every step, remove all possible rolls
# stop when all rolls are off
# given a floor plan return bitmap of which rolls on that floor are accessible
Accessible ← { 𝕊floor:
	s ← ≢ floor
	s ⥊ ({4≥+´'@'=𝕩}∧{'@'=4⊑𝕩})˘ ∘‿9⥊ 3‿3↕ Pad˘ Pad floor
}

# I'm gonna be honest, I only have a sliiigt clue as to wat
RemoveAcc ← {⊣◶⟨⊢,'.'˙⟩´⎉1 ⍉ Accessible⊸≍ 𝕩}

# this is also gonna look very... iterative
Sol2 ← {
accum𝕊floor:
	 access ← Accessible floor 
	 accn ← +´+˝ access
	 # it appears I do not know how to use pick
	 # this is far more brutal, but works
	 𝕤 {𝕨𝕊0:accum; 𝕨𝕊𝕩: (accum+𝕩)𝕎(RemoveAcc floor)} accn
}

0 Sol2 > •Flines "toyinput"
0 Sol2 > •Flines "input"
