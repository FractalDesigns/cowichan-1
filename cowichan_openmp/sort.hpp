#ifndef __sort_hpp__
#define __sort_hpp__

#include "cowichan_openmp.hpp"

/**
 * This performs parallel histogram sort, which is similar to bucket sort, but
 * does not require additional space for buckets (in-place).
 */
void histogram_sort(WeightedPointVector vector, index_t len);

// OpenMP tasks are not supported by msvc yet
#if defined(LIN32) || defined(LIN64)

// QUICK_SORT_CUTOFF must be > 1
#define QUICK_SORT_CUTOFF 100

// QUICK_SORT_TASK_CUTOFF
#define QUICK_SORT_TASK_CUTOFF 1000

/**
 * This performs parallel quick sort.
 */
void quick_sort(WeightedPointVector vector, index_t len);

/**
 * Partitions vector into points less than and greater than the pivot.
 */
index_t quick_sort_partition(WeightedPointVector vector, index_t len,
    index_t pivotIndex);
    
#endif

#endif


