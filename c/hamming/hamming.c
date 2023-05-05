#include "hamming.h"
#include <string.h>

int compute(const char *lhs, const char *rhs) {
  unsigned long length_lhs = strlen(lhs);

  if (length_lhs != strlen(rhs)) {
    return -1;
  }

  int distance = 0;
  unsigned long i;
  for (i = 0; i < length_lhs; i++) {
    if (lhs[i] != rhs[i]) {
      distance++;
    }
  }

  return distance;
}
