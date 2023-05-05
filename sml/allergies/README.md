# Allergies

Given a person's allergy score, determine whether or not they're allergic to a given item, and their full list of allergies.

An allergy test produces a single numeric score which contains the
information about all the allergies the person has (that they were
tested for).

The list of items (and their value) that were tested are:

* eggs (1)
* peanuts (2)
* shellfish (4)
* strawberries (8)
* tomatoes (16)
* chocolate (32)
* pollen (64)
* cats (128)

So if Tom is allergic to peanuts and chocolate, he gets a score of 34.

Now, given just that score of 34, your program should be able to say:

- Whether Tom is allergic to any one of those allergens listed above.
- All the allergens Tom is allergic to.

Note: a given score may include allergens **not** listed above (i.e.
allergens that score 256, 512, 1024, etc.).  Your program should
ignore those components of the score.  For example, if the allergy
score is 257, your program should only report the eggs (1) allergy.


## Setup

You will need an SML interpreter to solve these problems. There are several popular implementations
[MLton](http://mlton.org/), [SML/NJ](http://www.smlnj.org/), and [PolyML](http://www.polyml.org/). For reference, the problems are being developed using PolyML.

## Testing Note: your problem implementation must be in a file named example.sml

1. cd into your problem directory
2. use the test_<problem-name>.sml file with the SML interpreter of your choice (3 listed above)
3. verify that the allTestsPass variable has a true value
   * if so, congrats
   * if not, something is wrong with your implementation of the problem (or the test cases :|)

#### Example:

     cd ~/exercism/xsml/accumulate
     poly
     Poly/ML 5.2 Release
     > use "test_accumulate.sml";
     val accumulate = fn : 'a list -> ('a -> 'b) -> 'b list
     val it = () : unit
     val test_cases =
         [([], fn, []), ([1, 2, 3], fn, [1, 4, 9]), ([1, 2, 3], fn, [1, 8, 27]),
          ([1, 2, 3], fn, [2, 3, 4]), ([1, 2, 3], fn, [0, 1, 2])]
      : (int list * (int -> int) * int list) list
     val allTestsPass = true : bool
     val run_tests = fn : ('a list * ('a -> ''b) * ''b list) list -> bool list
     val it = () : unit
     (* inspect allTestsPass variable for true or false*)

## Source

Jumpstart Lab Warm-up [http://jumpstartlab.com](http://jumpstartlab.com)

## Submitting Incomplete Solutions
It's possible to submit an incomplete solution so you can see how others have completed the exercise.
