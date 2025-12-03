# below one may find the arduous development of an idea that
# seemed half dedcent at first until I realized  far, far later
# than needed, that its entire development had been continuously
# approved by nothing but the very special case I was testing it on
# 
# moral of the story is that dyamic programming is never
# a good idea outside of leetcode

# ==== BEGIN SERIES OF UNFORTUNATE EVENTS ====

# Sol2 I can probably dp-ish it

# the reader should note that me devolving into pseudo literate
# programming here is not with the aim of documenting my though
# process
#
# it's more akin to "rambling as a tool of thought"

# methlab
# all possible couples
(⊑⋈1⊸↓)¨ ¯2⊸↓∘↓ '0' -˜ "9876"

# all possible maximums
⌈´ ⌈´¨ (×⟜10∘⊣ + ⊢)´¨ (⊑⋈1⊸↓)¨ ¯2⊸↓∘↓ '0' -˜ "9876"

# this for 2
Max2 ← {⌈´ ⌈´¨ (×⟜10∘⊣ + ⊢)´¨ (⊑⋈1⊸↓)¨ ¯2⊸↓∘↓ '0' -˜ 𝕩}
Max2 "9876"
Max2 "1234"
Max2 "1111"

# let's try for 3
# we could start with all the candidates for 2 digits
# here they'd be 98‿87‿76
Max2Cand ← {⌈´¨ (×⟜10∘⊣ + ⊢)´¨ (⊑⋈1⊸↓)¨ ¯2⊸↓∘↓ '0' -˜ 𝕩}
Max2Cand "9876"
# and all the shit we can add to them here it would be ⟨7‿6, 6⟩
# (we need to drop 76 to make lengths match)
((¯1⊸↓)∘Max2Cand ⋈¨ {'0'-˜ 2↓ ¯1↓ ↓𝕩}) "9876"

# so going from Max2 to Max3 would be like

Max3 ← {
	 m2c ← (¯1⊸↓) Max2Cand 𝕩  # max2 candidates we can add shit to
	 ndc ← '0'-˜ 2↓ ¯1↓ ↓𝕩    # next digit candidates
	 ⌈´ ⌈´¨ (×⟜10∘⊣ + ⊢)´¨ m2c⋈¨ndc
}

# the last few steps were all rather familiar
# so let's try generalizing it

# for every update (going from Max2 to Max3, from Max3 to Max4, ...)
# get the candidates from last iteration
# drop the last one because you can't add shit to the last one
# get all digit suffices you can add to each candidate
# some max max zip +× bullshit between the two
# repeat

# so
# "base case"
# just our part 1 solution but spews more shit
MaxStart ← { 𝕊 str :
    ds	←	𝕩 - '0'      # digits
	fs	←	¯1↓ds        # first-s
	ls	←	1↓ ¯1⊸↓ ↓ ds # last-s (suffixes)

	(⌈⌾⊑)´¨  fs (×⟜10∘⊣+⊢) ls # candidates
	fs‿ls
}

# "recursive case"
# what we illustrated above
MaxUpdate ← { pc𝕊ss: # previous candidates 𝕊 suffixes
    pc ¯1⊸↓↩ # last candidate can't be augmented

	nc ←  ⌈´¨ pc (×⟜10∘⊣+⊢) ss
	nc‿ss
}

# so
⌈´⊑ MaxStart "9876" # max for 2 elements
⌈´⊑ MaxUpdate´ MaxStart "9876" # max for 3 elements
⌈´⊑ MaxUpdate´ MaxUpdate´ MaxStart "9876" # max for 4 elements

3 {⌈´⊑ MaxUpdate´⍟(𝕨-2) MaxStart 𝕩} "9876" # max for n elements

BankMax12 ← 12⊸{⌈´⊑ MaxUpdate´⍟(𝕨-2) MaxStart 𝕩}


Sol2 ← {
	 banks ← •FLines 𝕩
	 +´ BankMax12¨ banks
}

BankMax12¨ •FLines "toyinput"


# ===== END SERIES OF UNFORTUNATE EVENTS =====
