/**
 * Chained cowichan implementations
 *
 * \file chain.cpp
 * \author Andrew Borzenko
 * \date 03-13-09
 */

#include "../include/main.h"
#ifdef IS_PARALLEL
  #include "../mandel/parallel.h"
  #include "../randmat/parallel.h"
  #include "../half/parallel.h"
  #include "../invperc/parallel.h"
  #include "../thresh/parallel.h"
  #include "../life/parallel.h"
  #include "../winnow/parallel.h"
  #include "../norm/parallel.h"
  #include "../hull/parallel.h"
  #include "../outer/parallel.h"
  #include "../gauss/parallel.h"
  #include "../sor/parallel.h"
  #include "../product/parallel.h"
  #include "../vecdiff/parallel.h"
#else
  #include "../mandel/serial.h"
  #include "../randmat/serial.h"
  #include "../half/serial.h"
  #include "../invperc/serial.h"
  #include "../thresh/serial.h"
  #include "../life/serial.h"
  #include "../winnow/serial.h"
  #include "../norm/serial.h"
  #include "../hull/serial.h"
  #include "../outer/serial.h"
  #include "../gauss/serial.h"
  #include "../sor/serial.h"
  #include "../product/serial.h"
  #include "../vecdiff/serial.h"
#endif

int main(int argc, char* argv[])
{
#ifdef IS_PARALLEL
  mpi::environment env(argc, argv);
  mpi::communicator world;

#ifdef TEST_OUTPUT
  printf ("I am process %d\n", world.rank ());
#endif
#endif

  // there are four paths that can be run
  // 1. mandel -> ... -> invperc -> ...
  // 2. mandel -> ... -> thresh -> ...
  // 3. randmat -> ... -> invperc -> ...
  // 4. randmat -> ... -> thresh -> ...
  bool run_mandel;
  bool run_invperc;

  // mandel inputs
  int2D*  mandel_matrix;
  int     mandel_nr;
  int     mandel_nc;
  real    mandel_base_x;
  real    mandel_base_y;
  real    mandel_ext_x;
  real    mandel_ext_y;

  // randmat inputs
  int2D*  randmat_matrix;
  int     randmat_nr;
  int     randmat_nc;
  int     randmat_limit;
  int     randmat_seed;

  // half inputs
  int2D*  half_matrix;
  int     half_nr;
  int     half_nc;
  
  // invperc inputs
  int2D*  invperc_matrix;
  bool2D* invperc_mask;
  int     invperc_nr;
  int     invperc_nc;  
  real    invperc_fraction;

  // thresh inputs
  int2D*  thresh_matrix;
  bool2D* thresh_mask;
  int     thresh_nr;
  int     thresh_nc;  
  real    thresh_fraction;

  // life inputs
  bool2D* life_mask;
  int     life_nr;
  int     life_nc;
  int     life_iters;

  // winnow inputs
  int2D*  winnow_matrix;
  bool2D* winnow_mask;
  int     winnow_nr;
  int     winnow_nc;
  pt1D*   winnow_pts;
  int     winnow_n;

  // norm inputs
  pt1D*   norm_vec;
  int     norm_n;
  
  // hull inputs
  pt1D*   hull_pts;
  int     hull_n;

  // outer inputs
  pt1D*   outer_pt_vec;
  real2D* outer_matrix;
  real1D* outer_realVec;
  int     outer_n;

  // gauss inputs
  real2D* gauss_matrix;
  real1D* gauss_vector;
  real1D* gauss_answer;
  int     gauss_n;

  // sor inputs
  real2D* sor_matrix;
  real1D* sor_vector;
  real1D* sor_answer;
  int     sor_n;

  // product inputs
  real2D* product_matrix1;
  real2D* product_matrix2;
  real1D* product_vector1;
  real1D* product_vector2;
  real1D* product_result1;
  real1D* product_result2;
  int     product_nr;
  int     product_nc;

  // vecdiff inputs
  real1D* vecdiff_left;
  real1D* vecdiff_right;
  int     vecdiff_n;
  real    vecdiff_norm1_diff;

  // initialize inputs
  run_mandel = false;
  run_invperc = true;
  if (run_mandel) {
    mandel_matrix = new int2D[MAXEXT];
    mandel_nr = MAXEXT;
    mandel_nc = MAXEXT;
    mandel_base_x = 0.0;
    mandel_base_y = 0.0;
    mandel_ext_x = 1.5;
    mandel_ext_y = 1.5;
  }
  else {
    randmat_matrix = new int2D[MAXEXT];
    randmat_nr = MAXEXT;
    randmat_nc = MAXEXT;
    randmat_limit = 10;
    randmat_seed = 333;
  }

  // TODO: allow overriding initial inputs from command line

#ifdef TEST_TIME
  INT64 start, end;
  start = get_ticks ();
#endif

#ifdef IS_PARALLEL
  if (run_mandel) {
    mandel_mpi (world, mandel_matrix, mandel_nr, mandel_nc,
                mandel_base_x, mandel_base_y, mandel_ext_x, mandel_ext_y);
  }
  else {
    randmat_mpi (world, randmat_matrix, randmat_nr, randmat_nc,
                 randmat_limit, randmat_seed);
  }
#else
  if (run_mandel) {
    mandel (mandel_matrix, mandel_nr, mandel_nc,
            mandel_base_x, mandel_base_y, mandel_ext_x, mandel_ext_y);
  }
  else {
    randmat (randmat_matrix, randmat_nr, randmat_nc,
             randmat_limit, randmat_seed);
  }
#endif

#ifdef TEST_TIME
  end = get_ticks ();
  print_elapsed_time (start, end);
#endif

  if (run_mandel) {
    delete [] mandel_matrix;
  }
  else {
    delete [] randmat_matrix;
  }

  return 0;
}
