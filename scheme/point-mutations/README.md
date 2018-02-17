# Point Mutations

Write a program that can calculate the Hamming difference between two DNA strands.

A mutation is simply a mistake that occurs during the creation or
copying of a nucleic acid, in particular DNA. Because nucleic acids are
vital to cellular functions, mutations tend to cause a ripple effect
throughout the cell. Although mutations are technically mistakes, a very
rare mutation may equip the cell with a beneficial attribute. In fact,
the macro effects of evolution are attributable by the accumulated
result of beneficial microscopic mutations over many generations.

The simplest and most common type of nucleic acid mutation is a point
mutation, which replaces one base with another at a single nucleotide.

By counting the number of differences between two homologous DNA strands
taken from different genomes with a common ancestor, we get a measure of
the minimum number of point mutations that could have occurred on the
evolutionary path between the two strands.

This is called the 'Hamming distance'

    GAGCCTACTAACGGGAT
    CATCGTAATGACGGCCT
    ^ ^ ^  ^ ^    ^^

The Hamming distance between these two DNA strands is 7.

# Implementation notes

The Hamming distance is only defined for sequences of equal length. Hence you
may assume that only sequences of equal length will be passed to your hamming
distance function.

No additional dependencies should be required.

Sometimes module functions will be loaded that are useful to the problem case.
You'll see them in the `define-package` macro at the top of the source file,
thus: `#:autoload (mod submod) (function0 function1)`. The curious can read
about included modules [here][0].

If you need help getting set up, see [Getting Started With Scheme][1]
in the [Exercism.io Help][2] pages.

[0]: https://www.gnu.org/software/guile/docs/docs-2.0/guile-ref/Included-Guile-Modules.html
[1]: http://help.exercism.io/getting-started-with-scheme.html
[2]: http://help.exercism.io

## Source

The Calculating Point Mutations problem at Rosalind [view source](http://rosalind.info/problems/hamm/)
