# Leap

Welcome to Leap on Exercism's MIPS Assembly Track.
If you need help running the tests or submitting your code, check out `HELP.md`.
If you get stuck on the exercise, check out `HINTS.md`, but try and solve it without using those first :)

## Introduction

A leap year (in the Gregorian calendar) occurs:

- In every year that is evenly divisible by 4.
- Unless the year is evenly divisible by 100, in which case it's only a leap year if the year is also evenly divisible by 400.

Some examples:

- 1997 was not a leap year as it's not divisible by 4.
- 1900 was not a leap year as it's not divisible by 400.
- 2000 was a leap year!

~~~~exercism/note
For a delightful, four-minute explanation of the whole phenomenon of leap years, check out [this YouTube video](https://www.youtube.com/watch?v=xX96xng7sAE).
~~~~

## Instructions

Your task is to determine whether a given year is a leap year.

## Registers

## Registers

| Register | Usage     | Type    | Description                                      |
| -------- | --------- | ------- | ------------------------------------------------ |
| `$a0`    | input     | integer | year to check                                    |
| `$v0`    | output    | boolean | input is leap year (`0` = `false`, `1` = `true`) |
| `$t0-9`  | temporary | any     | used for temporary storage                       |

## Source

### Created by

- @ozan

### Based on

CodeRanch Cattle Drive, Assignment 3 - https://coderanch.com/t/718816/Leap