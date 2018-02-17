# Phone Number

Write a program that cleans up user-entered phone numbers so that they can be sent SMS messages.

The rules are as follows:

- If the phone number is less than 10 digits assume that it is bad
  number
- If the phone number is 10 digits assume that it is good
- If the phone number is 11 digits and the first number is 1, trim the 1
  and use the last 10 digits
- If the phone number is 11 digits and the first number is not 1, then
  it is a bad number
- If the phone number is more than 11 digits assume that it is a bad
  number

We've provided tests, now make them pass.

Hint: Only make one test pass at a time. Disable the others, then flip
each on in turn after you get the current failing one to pass.

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

Event Manager by JumpstartLab [view source](http://tutorials.jumpstartlab.com/projects/eventmanager.html)
