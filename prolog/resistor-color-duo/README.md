# Resistor Color Duo

Welcome to Resistor Color Duo on Exercism's Prolog Track.
If you need help running the tests or submitting your code, check out `HELP.md`.

## Instructions

If you want to build something using a Raspberry Pi, you'll probably use _resistors_.
For this exercise, you need to know two things about them:

- Each resistor has a resistance value.
- Resistors are small - so small in fact that if you printed the resistance value on them, it would be hard to read.

To get around this problem, manufacturers print color-coded bands onto the resistors to denote their resistance values.
Each band has a position and a numeric value.

The first 2 bands of a resistor have a simple encoding scheme: each color maps to a single number.
For example, if they printed a brown band (value 1) followed by a green band (value 5), it would translate to the number 15.

In this exercise you are going to create a helpful program so that you don't have to remember the values of the bands.
The program will take color names as input and output a two digit number, even if the input is more than two colors!

The band colors are encoded as follows:

- black: 0
- brown: 1
- red: 2
- orange: 3
- yellow: 4
- green: 5
- blue: 6
- violet: 7
- grey: 8
- white: 9

From the example above:
brown-green should return 15, and
brown-green-violet should return 15 too, ignoring the third color.

## Source

### Created by

- @erikschierboom

### Based on

Maud de Vries, Erik Schierboom - https://github.com/exercism/problem-specifications/issues/1464
