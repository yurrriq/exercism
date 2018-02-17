# Raindrops

Write a program that converts a number to a string, the contents of which depends on the number's prime factors.

- If the number contains 3 as a prime factor, output 'Pling'.
- If the number contains 5 as a prime factor, output 'Plang'.
- If the number contains 7 as a prime factor, output 'Plong'.
- If the number does not contain 3, 5, or 7 as a prime factor,
  just pass the number's digits straight through.

## Examples

- 28's prime-factorization is 2, 2, 7.
  - In raindrop-speak, this would be a simple "Plong".
- 1755 prime-factorization is 3, 3, 3, 5, 13.
  - In raindrop-speak, this would be a "PlingPlang".
- The prime factors of 34 are 2 and 17.
  - Raindrop-speak doesn't know what to make of that,
    so it just goes with the straightforward "34".

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

A variation on a famous interview question intended to weed out potential candidates. [view source](http://jumpstartlab.com)
