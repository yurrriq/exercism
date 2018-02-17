# Robot Name

Write a program that manages robot factory settings.

When robots come off the factory floor, they have no name.

The first time you boot them up, a random name is generated, such as
RX837 or BC811.

Every once in a while we need to reset a robot to its factory settings,
which means that their name gets wiped. The next time you ask, it gets a
new name.

Random names means a risk of collisions. In some exercism language
tracks there are tests to ensure that the same name is never used twice.

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

A debugging session with Paul Blackwell at gSchool. [view source](http://gschool.it)
