# Hello World

Write a program that greets the user by name, or by saying "Hello world!" if no name is given.

["Hello, World!"](http://en.wikipedia.org/wiki/%22Hello,_world!%22_program) is the traditional first program for beginning programming in a new language.

**Note:** You can skip this exercise by running:

    exercism skip $LANGUAGE hello-world

## Specification

The `Hello World!` program will greet me, the caller.

If I tell the program my name is Alice, it will greet me by saying "Hello, Alice!".

If I neglect to give it my name, it will greet me by saying "Hello, world!"

## Test-Driven Development

As programmers mature, they eventually want to test their code.

Here at Exercism we simulate [Test-Driven Development](http://en.wikipedia.org/wiki/Test-driven_development) (TDD), where you write your tests before writing any functionality. The simulation comes in the form of a pre-written test suite, which will signal that you have solved the problem.

It will also provide you with a safety net to explore other solutions without breaking the functionality.

### A typical TDD workflow on Exercism:

1. Run the test file and pick one test that's failing.
2. Write some code to fix the test you picked.
3. Re-run the tests to confirm the test is now passing.
4. Repeat from step 1.
5. [Submit your solution](http://help.exercism.io/submitting-exercises.html).

## Instructions

Submissions are encouraged to be general, within reason. Having said that, it's also important not to over-engineer a solution.

It's important to remember that the goal is to make code as expressive and readable as we can. However, solutions to the hello-world exercise will be not be reviewed by a person, but by rikki- the robot, who will offer an encouraging word.

### Getting started
First install lua using [homebrew][1]

    $ brew install lua

Then install [luarocks][2] to install packages for lua

    $ brew install luarocks

Then install [busted][3] testing framework for lua

    $ luarocks install busted

Then run your test

    $ busted bob_test.lua

Other resources

  1. [Lua Style Guide][4]
  2. [Learn Lua in 15 minutes][5]

[1]: http://brew.sh/
[2]: http://luarocks.org/
[3]: http://olivinelabs.com/busted/
[4]: https://github.com/Olivine-Labs/lua-style-guide
[5]: http://tylerneylon.com/a/learn-lua/

## Source

This is a program to introduce users to using Exercism [view source](http://en.wikipedia.org/wiki/%22Hello,_world!%22_program)
