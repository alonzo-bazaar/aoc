# idea, get the array of all positions reached by the dial
# without any modulo
# solution 1: when does it become 0 mod 100? (then +´ the mask)
# solution 2: when does the floor change ÷100? (then +´ ≠(something) the resulting array)
# (solution 2 was one hell of a red herring, found a better way)

ReadInt ← (+´(10⊸⋆∘⌽∘↕≠)⊸×)∘(-⟜'0')
ZeroStops ← {+´0=100| 𝕩}

# wentover
# given two integers
# how many integer multiple of 100 are between the two numbers?
# not counting the extremes
# I understand this function kinda sucks ass to look at, sorry
WentOver ← {0=𝕨=𝕩}◶⟨0, (⌊{1 + (⌊100÷˜𝕩-1) - (⌈100÷˜𝕨+1)}⌈)⟩

# given a filename with the problem data
# return assumed positions and the operations used to go between em
FilePositions ← {
    moves ← •file.Lines 𝕩
    
    nums ← ReadInt∘(1⊸↓)¨moves
    addp ← ('R'⊸=)∘⊑¨moves # p in the lisp sense
    ops ← (1-˜2×addp)×nums
    positions ← 50∾50+`ops
    # intial position is 50
    # so gotta start from there and apply shit from there

	positions
}

# given positions we went through, returns solution 1 to problem
Sol1_Positions ← ZeroStops

# given positions we went through
# returns solution 2 to problem
Sol2_Positions ← {
    𝕊 ps:
	wentover ← +´ (¯1↓ps) WentOver¨ (1↓ps)
	wentover + ZeroStops ps
}

Solution ← {
    positions ← FilePositions 𝕩
    
    sol1 ← Sol1_Positions positions
	sol2 ← Sol2_Positions positions
    
    sol1‿sol2
}
