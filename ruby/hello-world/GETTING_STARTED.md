# Getting Started

These exercises lean on Test-Driven Development (TDD), but they're not an
exact match.

The following steps assume that you are in the same directory as the test
suite.

You must have the `minitest` gem installed:

    $ gem install minitest

## Step 1

Run the test suite. It's written using the Minitest framework, and can be
run with ruby:

    $ ruby hello_world_test.rb

This will fail, complaining that there is no file called `hello_world`.

To fix the error create an empty file called `hello_world.rb` in the same
directory as the `hello_world_test.rb` file.

## Step 2

Run the test again. It will give you a new error.

    1) Error:
    HelloWorldTest#test_no_name:
    NameError: uninitialized constant HelloWorldTest::HelloWorld
      hello-world/hello_world_test.rb:5:in `test_no_name'

Within the first test, we are referencing a constant named `HelloWorld` when
we say `HelloWorld.hello`. When Ruby sees a capitalized name like
`HelloWorld`, it looks it up in a big huge list of all the constants it knows about,
to see what it points to. It could point to anything, and often in Ruby we have
constants that point to definitions of classes or modules.

When it looks `HelloWorld` up in it's list, it doesn't find anything, so we need
to make one.

There are several ways that this error message can be made to go
away, one of which is to define a `HelloWorld` class.

Open up the hello_world.rb file and add the following code:

    class HelloWorld
    end

## Step 3

Run the test again.

    1) Error:
    HelloWorldTest#test_no_name:
    NoMethodError: undefined method `hello' for HelloWorld:Class
        hello_world_test.rb:5:in `test_no_name'

This time we have a `HelloWorld`, but we're trying tell it to `hello`, and
`HelloWorld` doesn't understand that message.

Open up hello_world.rb and add a method definition inside the class:

    class HelloWorld
      def self.hello
      end
    end

## Step 4

Run the test again.

    1) Failure:
    HelloWorldTest#test_no_name [hello_world_test.rb:11]:
    When given no name, we should greet the world!.
    Expected: "Hello, world!"
      Actual: nil

Up until now we've been getting errors, this time we get a failure.

An error means that Ruby cannot even run properly because of things like missing
files or syntax errors, or referring to things that don't exist.

A failure is different. A failure is when Ruby is running just fine
and the test is expecting one outcome, but getting another.

The test is expecting the `hello` method to return the string `"Hello, world!"`. The easiest way
to make it pass, is to simply stick the string `"Hello, world!"` inside the method definition.

## Step 6

Run the test again.

If it fails you're going to need to read the error message carefully to figure
out what went wrong, and then try again.

If it passes, then you're ready to move to the next step.

Open the hello_world_test.rb file, and find the word "skip". All but the first test
start with "skip", which tells Minitest to ignore the test. This is so that
you don't have to deal with all the failures at once.

To activate the next test, delete the "skip", and run the test suite again.

## Wash, Rinse, Repeat

Delete one "skip" at a time, and make each test pass before you move to the
next one.

## Submit

When everything is passing, you can submit your code with the following
command:

    $ exercism submit hello_world.rb
