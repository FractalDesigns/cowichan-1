#include "cowichan_openmp.hpp"

void CowichanOpenMP::thresh(IntMatrix matrix, BoolMatrix mask) {

  IntVector hist = NULL; // histogram
  INT64 i, j;
  INT64 r, c;
  INT_TYPE vMax; // max value in matrix
  INT64 retain; // selection

  INT64 num_threads;
#pragma omp parallel
  num_threads = omp_get_num_threads();

  INT_TYPE* vMaxLocal = NULL;
  try {
    vMaxLocal = NEW_VECTOR_SZ(INT_TYPE, num_threads);
  }
  catch (...) {out_of_memory();}

  // find max value in matrix
#pragma omp parallel
  {
  INT64 cur_thread = omp_get_thread_num();
  vMaxLocal[cur_thread] = 0;
#pragma omp for schedule(static)
  for (r = 0; r < nr; r++) {
#pragma omp parallel for schedule(static)
    for (c = 0; c < nc; c++) {
      if (vMaxLocal[cur_thread] < MATRIX_RECT(matrix, r, c)) {
        vMaxLocal[cur_thread] = MATRIX_RECT(matrix, r, c);
      }
    }
  }
  }

  vMax = 0;
  for (i = 0; i < num_threads; i++) {
    if (vMax < vMaxLocal[i]) {
      vMax = vMaxLocal[i];
    }
  }

  delete [] vMaxLocal;

  // initialize histogram
  try {
    hist = NEW_VECTOR_SZ(INT_TYPE, vMax + 1);
  }
  catch (...) {out_of_memory();}

  INT_TYPE** histLocal = NULL;
  try {
    histLocal = NEW_VECTOR_SZ(INT_TYPE*, num_threads);
    for (i = 0; i < num_threads; i++) {
      histLocal[i] = NEW_VECTOR_SZ(INT_TYPE, vMax + 1);
    }
  }
  catch (...) {out_of_memory();}

#pragma omp parallel for schedule(static) private(j)
  for (i = 0; i <= vMax; i++) {
    hist[i] = 0;
    for (j = 0; j < num_threads; j++) {
      histLocal[j][i] = 0;
    }
  }

  // count
#pragma omp parallel
  {
  INT64 cur_thread = omp_get_thread_num();
#pragma omp for schedule(static)
  for (r = 0; r < nr; r++) {
#pragma omp parallel for schedule(static)
    for (c = 0; c < nc; c++) {
      histLocal[cur_thread][MATRIX_RECT(matrix, r, c)]++;
    }
  }
  }

  for (i = 0; i < num_threads; i++) {
    for (j = 0; j <= vMax; j++) {
      hist[j] += histLocal[i][j];
    }
    delete [] histLocal[i];
  }

  delete [] histLocal;

  // include
  retain = (INT64)(threshPercent * nc * nr);
  for (i = vMax; ((i >= 0) && (retain > 0)); i--) {
    retain -= hist[i];
  }
  retain = i;

  delete [] hist;

  // threshold
#pragma omp parallel for schedule(static)
  for (r = 0; r < nr; r++) {
#pragma omp parallel for schedule(static)
    for (c = 0; c < nc; c++) {
      MATRIX_RECT(mask, r, c) = MATRIX_RECT(matrix, r, c) > retain;
    }
  }

}

