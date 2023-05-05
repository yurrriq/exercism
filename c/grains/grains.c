#include "grains.h"
#include <math.h>

uint64_t square(uint8_t index) {
  if (index >= 1 && index <= 64) {
    return pow(2, index - 1);
  }

  return 0;
}

uint64_t total(void) { return square(65) - 1; }
