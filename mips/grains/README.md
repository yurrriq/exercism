# Grains

Welcome to Grains on Exercism's MIPS Assembly Track.
If you need help running the tests or submitting your code, check out `HELP.md`.
If you get stuck on the exercise, check out `HINTS.md`, but try and solve it without using those first :)

## Instructions

Calculate the number of grains of wheat on a chessboard given that the number on each square doubles.

There once was a wise servant who saved the life of a prince.
The king promised to pay whatever the servant could dream up.
Knowing that the king loved chess, the servant told the king he would like to have grains of wheat.
One grain on the first square of a chess board, with the number of grains doubling on each successive square.

There are 64 squares on a chessboard (where square 1 has one grain, square 2 has two grains, and so on).

Write code that shows:

- how many grains were on a given square, and
- the total number of grains on the chessboard

## Registers

| Register | Usage     | Type    | Description                      |
| -------- | --------- | ------- | -------------------------------- |
| `$a0`    | input     | integer | square number in the range 1..64 |
| `$v0`    | output    | integer | low 32 bits of output            |
| `$10`    | output    | integer | high 32 bits of output           |
| `$t0-9`  | temporary | any     | for temporary storage            |

## Source

### Created by

- @keiravillekode

### Based on

The CodeRanch Cattle Drive, Assignment 6 - https://coderanch.com/wiki/718824/Grains