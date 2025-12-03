BankMax ← {
    ds	←	𝕩 - '0'    # digits
	suf ←	¯2⊸↓ ↓ ds  # suffixes of d longer than 2
	                   # (can't really do shit with the shorter ones)
	⌈´ ⌈´¨ (⊑∘×⟜10 + 1⊸↓)¨ suf
}

# test
# BankMax "987654321111111"

Sol1 ← {
	 banks ← •FLines 𝕩
	 +´ BankMax¨ banks
}

Sol1 "toyinput"
Sol1 "input"

# (graveyard.bqn went here)

# ok reject everything, this doesn't work, new idea
# one digit at a time
# for 12th, the most significant digit
# take biggest digit with at least 12 spaces to its left

# split that digit off, take the suffix from that digit onwards
# n-- shit you got left, new iteration
# accumulate
# ez

# is it unidiomatic as all hell... yes, most likey
# could have used a ⍟12 or some shit

# digit of max solution
# mildly lispy approach
Digs ← {
0𝕊ds: ⟨⟩ ;
left𝕊ds:
    sds ←	ds↓˜1-left       # digits you could start with
    md	←	⌈´ sds           # biggest one among these
	im	←	ds ⊑∘⊒ md        # index of max
	md ∾ (left-1)𝕊(ds↓˜1+im)
}

Bank12 ← {
    ds ← '0' -˜ 𝕩
    digs ← 12 Digs ds

	# for non commutative operations
	# please remember that bqn's fold is a right fold
	# given how it's equivalent to putting the op between every element
	# and that bqn functions apply from right to left
	# the fold "starts" from the right/end of the array, and goes to the start

	# so I reversed the digits first
	# then made a train that turns digits into that number backwards
	# because I'm too used to left folds :|
    (10⊸×∘⊢ + ⊣)´ ⌽ digs
}

Sol2 ← {+´ Bank12¨ •Flines 𝕩}

Sol2 "toyinput"
Sol2 "input"
