#include "armstrong_numbers.h"
#include <math.h>
#include <stdlib.h>

unsigned int ipow(unsigned int base, unsigned int exp);

bool is_armstrong_number(int candidate) {
  if (candidate == 0) {
    return true;
  }

  if (candidate < 0) {
    return false;
  }

  unsigned int base = 10;
  unsigned int n = (unsigned int)candidate;
  size_t num_digits = (size_t)ceil(log(n + 1) / log(base));

  int sum = 0;
  while (n > 0) {
    sum += ipow(n % base, num_digits);
    n /= base;
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
