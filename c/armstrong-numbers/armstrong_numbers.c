#include "armstrong_numbers.h"
#include <math.h>
#include <stdlib.h>

unsigned int ipow(unsigned int base, unsigned int exp);

bool is_armstrong_number(int candidate) {
  if (candidate < 10) {
    return true;
  }

  unsigned int n = candidate;
  unsigned int num_digits = log10(candidate) + 1;

  int sum = 0;
  while (n > 0) {
    sum += ipow(n % 10, num_digits);
    n /= 10;
  }

  return candidate == sum;
}

unsigned int ipow(unsigned int base, unsigned int exp) {
  unsigned int result = 1;
  for (;;) {
    if (exp & 1)
      result *= base;
    exp >>= 1;
    if (!exp)
      break;
    base *= base;
  }

  return result;
}
