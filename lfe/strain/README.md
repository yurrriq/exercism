# Strain

Implement the `keep` and `discard` operation on collections. Given a collection and a predicate on the collection's elements, `keep` returns a new collection containing those elements where the predicate is true, while `discard` returns a new collection containing those elements where the predicate is false.

Write two functions that each take a function and a list.  One of them will
return the list of items for which the passed in function is true, and the
other will return the items for which it is false.

For example, given the collection of numbers:

- 1, 2, 3, 4, 5

And the predicate:

- is the number even?

Then your keep operation should produce:

- 2, 4

While your discard operation should produce:

- 1, 3, 5

Note that the union of keep and discard is all the elements.

The functions may be called `keep` and `discard`, or they may need different
names in order to not clash with existing functions or concepts in your
language.

## Restrictions

Keep your hands off that filter/reject/whatchamacallit functionality
provided by your standard library!  Solve this one yourself using other
basic tools instead.

* * * *

For installation and learning resources, refer to the [exercism help page][].

[exercism help page]: http://exercism.io/languages/lfe

To run the provided tests, you will need `make`.
Open a terminal window and run the following from the exercise directory:

```sh
make test
```

You should now be able to see the results of the test suite for the exercise.

## Source

Conversation with James Edward Gray II [https://twitter.com/jeg2](https://twitter.com/jeg2)

## Submitting Incomplete Problems
It's possible to submit an incomplete solution so you can see how others have completed the exercise.
