# Accumulate

Implement the `accumulate` operation, which, given a collection and an operation to perform on each element of the collection, returns a new collection containing the result of applying that operation to each element of the input collection.

For example, given the collection of numbers:

- 1, 2, 3, 4, 5

And the operation:

- square a number

Your code should be able to produce the collection of squares:

- 1, 4, 9, 16, 25

Check out the test suite to see the expected function signature.

## Restrictions

Keep your hands off that collect/map/fmap/whatchamacallit functionality
provided by your standard library!
Solve this one yourself using other basic tools instead.

Elixir specific: it's perfectly fine to use `Enum.reduce` or
`Enumerable.reduce`.

Lisp specific: it's perfectly fine to use `MAPCAR` or the equivalent,
as this is idiomatic Lisp, not a library function.

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

Conversation with James Edward Gray II [view source](https://twitter.com/jeg2)
