#ifndef __sort_hpp__
#define __sort_hpp__

#include "cowichan_openmp.hpp"

/**
 * This performs parallel histogram sort, which is similar to bucket sort, but
 * does not require additional space for buckets (in-place).
 */
void histogram_sort(WeightedPointVector vector, INT64 len);

// OpenMP tasks are not supported by msvc yet
#if defined(LIN32) || defined(LIN64)

// QUICK_SORT_CUTOFF must be > 1
#define QUICK_SORT_CUTOFF 100

/**
 * This performs parallel quick sort.
 */
void quick_sort(WeightedPointVector vector, INT64 len);

/**
 * Partitions vector into points less than and greater than the pivot.
 */
INT64 quick_sort_partition(WeightedPointVector vector, INT64 len,
    INT64 pivotIndex);
    
#endif

#endif


