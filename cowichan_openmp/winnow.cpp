#include "cowichan_openmp.hpp"
#include "sort.hpp"

void mask_count(BoolMatrix mask, INT64 nr, INT64 nc, INT64* buckets);

void not_enough_points();

void CowichanOpenMP::winnow(IntMatrix matrix, BoolMatrix mask,
    PointVector points) {

  INT64 r, c;
  INT64 len; // number of points
  INT64 stride; // selection stride
  INT64 i;

  INT64 num_threads = omp_get_max_threads();

  INT64* buckets = NULL;

  try {
    buckets = NEW_VECTOR_SZ(INT64, num_threads);
  }
  catch (...) {out_of_memory();}

  // count set cell in each bucket
  mask_count (mask, nr, nc, buckets);

  // calculate offsets
  len = 0;
  for (i = 0; i < num_threads; i++) {
    INT64 tmp = buckets[i];
    buckets[i] = len;
    len += tmp;
  }

  if (len < n) {
    not_enough_points();
  }

  WeightedPointVector weightedPoints = NULL;
  try {
    weightedPoints = NEW_VECTOR_SZ(WeightedPoint, len);
  }
  catch (...) {out_of_memory();}

  // fill temporary vector
#pragma omp parallel private(i)
  {
    INT64 thread_num = omp_get_thread_num();
    i = buckets[thread_num];
#pragma omp for schedule(static)
    for (r = 0; r < nr; r++) {
#pragma omp parallel for schedule(static)
      for (c = 0; c < nc; c++) {
        if (MATRIX_RECT(mask, r, c)) {
          weightedPoints[i++] = WeightedPoint((real)c, (real)r,
              MATRIX_RECT(matrix, r, c));
        }
      }
    }
  }

  delete [] buckets;

#ifdef SORT_TIME
  INT64 start, end;
  start = get_ticks ();
#endif

  // sort
#if defined(LIN32) || defined(LIN64)
#pragma omp parallel
  {
#pragma omp single
    {
      quick_sort(weightedPoints, len);
    }
  }
#else
  histogram_sort(weightedPoints, len);
#endif

#ifdef SORT_TIME
  end = get_ticks ();
#endif

  // copy over points
  stride = len / n;

#pragma omp parallel for schedule(static)
  for (i = n - 1; i >= 0; i--) {
#ifdef WINNOW_OUTPUT
    std::cout << weightedPoints[len - 1 - (n - 1 - i) * stride].weight << "\n";
#endif
    points[i] = weightedPoints[len - 1 - (n - 1 - i) * stride].point;
  }
  
#ifdef SORT_TIME
  std::cout << "winnow sort: ";
  print_elapsed_time(start, end);
  std::cout << std::endl;
#endif

  delete [] weightedPoints;
}

/**
 * Count the number of set cells in each thread bucket.
 * @param mask boolean mask.
 * @param nr number of rows.
 * @param nc number of columns.
 */
void mask_count(BoolMatrix mask, INT64 nr, INT64 nc, INT64* buckets) {

  INT64 r, c, sum = 0;

#pragma omp parallel firstprivate(sum)
  {
    INT64 thread_num = omp_get_thread_num();
#pragma omp for schedule(static)
    for (r = 0; r < nr; r++) {
#pragma omp parallel for schedule(static)
      for (c = 0; c < nc; c++) {
        if (MATRIX_RECT_NC(mask, r, c, nc)) {
          sum++;
        }
      }
    }
    buckets[thread_num] = sum;
  }

}


