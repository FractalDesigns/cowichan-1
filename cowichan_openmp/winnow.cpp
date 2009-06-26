#include "cowichan_openmp.hpp"

void mask_count(BoolMatrix mask, INT64 nr, INT64 nc, INT64* buckets);

void sort(WeightedPointVector vector, INT64 len);

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

  // sort
  sort(weightedPoints, len);

  // copy over points
  stride = len / n;

#pragma omp parallel for schedule(static)
  for (i = n - 1; i >= 0; i--) {
    points[i] = weightedPoints[len - 1 - (n - 1 - i) * stride].point;
  }

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

/**
 * This performs parallel histogram sort, which is similar to bucket sort, but
 * does not require additional space for buckets (in-place).
 */
void sort(WeightedPointVector vector, INT64 len)
{
  const INT64 BUCKETS_PER_THREAD = 50;

  INT64 num_threads = omp_get_max_threads();
  INT64 num_buckets = BUCKETS_PER_THREAD * num_threads;

  // sort serially if array is too small
  if (len < num_buckets) {
    std::sort(vector, &vector[len]);
    return;
  }

  INT64 i, j;

  // find min/max values
  INT_TYPE* minWeights = NULL;
  INT_TYPE* maxWeights = NULL;
  INT_TYPE minWeight, maxWeight;

  try {
    minWeights = NEW_VECTOR_SZ(INT_TYPE, num_threads);
    maxWeights = NEW_VECTOR_SZ(INT_TYPE, num_threads);
  }
  catch (...) {out_of_memory();}

#pragma omp parallel private(i)
  {
    INT64 thread_num = omp_get_thread_num();
    minWeights[thread_num] = vector[0].weight;
    maxWeights[thread_num] = vector[0].weight;
#pragma omp for schedule(static)
    for (i = 1; i < len; i++) {
      if (minWeights[thread_num] > vector[i].weight) {
        minWeights[thread_num] = vector[i].weight;
      }
      else if (maxWeights[thread_num] < vector[i].weight) {
        maxWeights[thread_num] = vector[i].weight;
      }
    }
  }

  minWeight = minWeights[0];
  maxWeight = maxWeights[0];
  for (i = 1; i < num_threads; i++) {
    if (minWeight > minWeights[i]) {
      minWeight = minWeights[i];
    }
    else if (maxWeight < maxWeights[i]) {
      maxWeight = maxWeights[i];
    }
  }

  delete [] minWeights;
  delete [] maxWeights;

  // count number of elements in each bucket
  INT64** threadCounts = NULL;
  INT64* counts = NULL;
  INT64* offsets = NULL;

  try {
    counts = NEW_VECTOR_SZ(INT64, num_buckets);
    offsets = NEW_VECTOR_SZ(INT64, num_buckets + 1);
    threadCounts = NEW_VECTOR_SZ(INT64*, num_threads);
    for (i = 0; i < num_threads; i++) {
      threadCounts[i] = NEW_VECTOR_SZ(INT64, num_buckets);
    }
  }
  catch (...) {out_of_memory();}

#pragma omp parallel private(i)
  {
    INT64 thread_num = omp_get_thread_num();
    for (i = 0; i < num_buckets; i++) {
      threadCounts[thread_num][i] = 0;
    }
#pragma omp for schedule(static)
    for (i = 0; i < len; i++) {
      INT64 bucket = num_buckets * ((INT64)(vector[i].weight - minWeight))
          / ((INT64)(maxWeight - minWeight + 2));
      threadCounts[thread_num][bucket]++;
    }
  }

  for (i = 0; i < num_buckets; i++) {
    counts[i] = threadCounts[0][i];
    for (j = 1; j < num_threads; j++) {
      counts[i] += threadCounts[j][i];
    }
  }

  for (i = 0; i < num_threads; i++) {
    delete [] threadCounts[i];
  }
  delete [] threadCounts;

  // calculate offsets
  INT64 offset = 0;
  INT64 tmp;
  for (i = 0; i < num_buckets; i++) {
    tmp = counts[i];
    offsets[i] = counts[i] = offset;
    offset += tmp;
  }
  offsets[num_buckets] = len;

  // put elements into appropriate buckets by swapping
  // NOTE: not parallel, in-place
  WeightedPoint tmpPoint;
  INT64 src, dest;
  INT64 bucket;

  src = 0;
  while (src < len) {
    bucket = num_buckets * ((INT64)(vector[src].weight - minWeight))
        / ((INT64)(maxWeight - minWeight + 2));

    if ((src >= offsets[bucket]) && (src < offsets[bucket + 1])) {
      src++;
      continue;
    }

    dest = counts[bucket]++;

    tmpPoint = vector[dest];
    vector[dest] = vector[src];
    vector[src] = tmpPoint;
  }

  delete [] counts;

  // sort individual buckets
#pragma omp parallel for schedule(dynamic)
  for (i = 0; i < num_buckets; i++) {
    if (offsets[i] != offsets[i + 1]) {
      std::sort(&vector[offsets[i]], &vector[offsets[i + 1]]);
    }
  }

  delete [] offsets;
}

