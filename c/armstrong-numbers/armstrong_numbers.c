#include "armstrong_numbers.h"
#include <math.h>
#include <stdlib.h>

bool is_armstrong_number(int candidate) {
  if (candidate < 0) {
    return false;
  }

  if (candidate < 10) {
    return true;
  }

  unsigned int number = candidate;
  unsigned int num_digits = log10(candidate) + 1;

  int sum_pow_digits = 0;
  while (number > 0) {
    unsigned int digit = number % 10;
    unsigned int pow_digit = 1;
    for (unsigned int i = 0; i < num_digits; i++) {
      pow_digit *= digit;
    }

    sum_pow_digits += pow_digit;
    number /= 10;
  }

  return candidate == sum_pow_digits;
}
