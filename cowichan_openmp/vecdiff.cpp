#include "cowichan_openmp.hpp"

real CowichanOpenMP::vecdiff (Vector actual, Vector computed)
{
  INT64 i;
  real diff;
  real maxDiff;

  Vector maxDiffs = NULL;
  INT64 num_threads = omp_get_max_threads();
  printf ("%d\n", num_threads);

  try {
    maxDiffs = NEW_VECTOR_SZ(real, num_threads);
  }
  catch (...) {out_of_memory();}

#pragma omp parallel private(diff, maxDiff)
  {
    INT64 thread_num = omp_get_thread_num();
    maxDiff = (real)fabs((double)(actual[0] - computed[0]));
#pragma omp for schedule(static)
    for (i = 1; i < n; i++) {
      diff = (real)fabs((double)(actual[i] - computed[i]));
      if (maxDiff < diff) {
        maxDiff = diff;
      }
    }
    maxDiffs[thread_num] = maxDiff;
  }

  maxDiff = maxDiffs[0];
  for (i = 1; i < num_threads; i++) {
    if (maxDiff < maxDiffs[i]) {
      maxDiff = maxDiffs[i];
    }
  }

  delete [] maxDiffs;

  return maxDiff;
}

