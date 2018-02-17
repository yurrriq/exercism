# Anagram

Write a program that, given a word and a list of possible anagrams, selects the correct sublist.

Given `"listen"` and a list of candidates like `"enlists" "google"
"inlets" "banana"` the program should return a list containing
`"inlets"`.

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

Inspired by the Extreme Startup game [view source](https://github.com/rchatley/extreme_startup)
