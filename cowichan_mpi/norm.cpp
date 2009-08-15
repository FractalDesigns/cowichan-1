/**
 * \file cowichan_mpi/norm.cpp
 * \brief MPI point normalization implementation.
 * \see CowichanMPI::norm
 */

#include "cowichan_mpi.hpp"

namespace cowichan_mpi {

	/*
	 * @ redPt1DPos : find min/max point positions
	 * > none
	 * + fill arguments
	 */
	void
	redPt1DPos(
	  PointVector		vec,			/* vector of points */
	  int		n,			/* number of points */
	  Point	      * ptMin,			/* minimum location */
	  Point	      * ptMax			/* maximum location */
	);
	
	/**
	 * MPI reduction primitive for calculating point minimum.
	 */
	struct minimum_pt {
	  Point operator()(Point a, Point b)
	  {
		if (b.x < a.x) {
		  a.x = b.x;
		}
		if (b.y < a.y) {
		  a.y = b.y;
		}
		return a;
	  }
	};

	/**
	 * MPI reduction primitive for calculating point maximum.
	 */
	struct maximum_pt {
	  Point operator()(Point a, Point b)
	  {
		if (b.x > a.x) {
		  a.x = b.x;
		}
		if (b.y > a.y) {
		  a.y = b.y;
		}
		return a;
	  }
	};	

}

void CowichanMPI::norm(PointVector pointsIn, PointVector pointsOut)
{
  Point ptMin_local, ptMax_local; //pseudo-points
  Point ptMin, ptMax; // pseudo-points
  real sclX, sclY; // scaling factors
  index_t i; // loop index
  index_t lo, hi; // work controls
  bool work; // useful work to do?

  // initialize
  ptMin_local = pointsIn[0];
  ptMax_local = pointsIn[0];

  work = get_block (world, 0, n, &lo, &hi);
  if (work) {
    redPt1DPos(&pointsIn[lo], hi - lo, &ptMin_local, &ptMax_local);
  }

  all_reduce (world, ptMin_local, ptMin, minimum_pt ());
  all_reduce (world, ptMax_local, ptMax, maximum_pt ());

  if (work) {
    // scaling factors
    sclX = (ptMax.x == ptMin.x) ? 0.0 : 1/(ptMax.x - ptMin.x);
    sclY = (ptMax.y == ptMin.y) ? 0.0 : 1/(ptMax.y - ptMin.y);
    // scale
    for (i = lo; i < hi; i++) {
      pointsOut[i].x = sclX * (pointsIn[i].x - ptMin.x);
      pointsOut[i].y = sclY * (pointsIn[i].y - ptMin.y);
    }
  }

  // broadcast normalized values
  for (i = 0; i < world.size (); i++) {
    if (get_block(world, 0, n, &lo, &hi, i)) {
      broadcast (world, &pointsOut[lo], hi - lo, i);
    }
  }

  /* return */
}

namespace cowichan_mpi {

	void
	redPt1DPos(
	  PointVector		vec,			/* vector of points */
	  int		n,			/* number of points */
	  Point	      * ptMin,			/* minimum location */
	  Point	      * ptMax			/* maximum location */
	){
	  int		i;

	  assert(ptMin != NULL);
	  assert(ptMax != NULL);

	  ptMin->x = vec[0].x; ptMin->y = vec[0].y;
	  ptMax->x = vec[0].x; ptMax->y = vec[0].y;
	  for (i=1; i<n; i++){
		if (vec[i].x < ptMin->x) ptMin->x = vec[i].x;
		if (vec[i].x > ptMax->x) ptMax->x = vec[i].x;
		if (vec[i].y < ptMin->y) ptMin->y = vec[i].y;
		if (vec[i].y > ptMax->y) ptMax->y = vec[i].y;
	  }

	  /* return */
	}

}

