#include "cowichan_serial.hpp"

real CowichanSerial::vecdiff (Vector actual, Vector computed)
{
  index_t i;
  real diff;
  real maxDiff;

  maxDiff = (real)fabs((double)(actual[0] - computed[0]));
  for (i = 1; i < n; i++) {
    diff = (real)fabs((double)(actual[i] - computed[i]));
    if (maxDiff < diff) {
      maxDiff = diff;
    }
  }

  return maxDiff;
}

