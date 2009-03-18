#include "cowichan.hpp"

	/**
	 * Runs the cowichan problem set, chained together.
	 * @param numThreads	the number of threads to spawn using TBB.
	 * @param use_randmat	true: generate a random matrix.
	 * 						false: use a window of the mandelbrot set.
	 * @param use_thresh	true: use image thresholding for int->bool.
	 *						false: use invasion percolation for int->bool.
	 */
	void Cowichan::run(int numThreads, bool use_randmat, bool use_thresh) {
		IntMatrix matrix;
		Matrix realmx;
		Vector vector, x_sor, x_gauss;
		BoolMatrix bm;
		PointList* points;
		real e_gauss, e_sor;
		
		// set up for the number of threads we will use
		COWICHAN(numThreads);		
		
/* 1 */	if (use_randmat) {
			randmat(&matrix);
		} else {
			mandel(&matrix);
		}
		
/* 2 */	half(matrix, &matrix);

/* 3 */	if (use_thresh) {
			thresh(matrix, &bm);
		} else {
			invperc(matrix, &bm);
		}
		
/* 4 */	life(bm, &bm);
/* 5 */	winnow(matrix, bm, &points);
/* 6 */	hull(points, &points);
/* 7 */	norm(points, &points);
/* 8 */	outer(points, &realmx, &vector);

/* 9 */	gauss(realmx, vector, &x_gauss);
		sor(realmx, vector, &x_sor);
		
/* 10*/	product(realmx, vector, x_gauss, &e_gauss);
		product(realmx, vector, x_sor, &e_sor);
		
	}
	
/*****************************************************************************/

/**
 * The entry point of the Cowichan/TBB problem set.
 */
int main(int argc, char** argv) {
	Cowichan::run(2, true, false);
	return 0;
}

