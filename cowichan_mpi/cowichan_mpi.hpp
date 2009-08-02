/**
 * \file cowichan_mpi.hpp
 * \brief Boost Message Passing Interface (MPI) implementation of Cowichan
 * problems.
 */

#ifndef __cowichan_mpi_hpp__
#define __cowichan_mpi_hpp__

#include "../cowichan/cowichan.hpp"

/**
 * \brief Additional classes and functions specific to mpi implementation.
 */
namespace cowichan_mpi
{
}

// using a namespace to avoid (documentation) name clashes
using namespace cowichan_mpi;

#include <boost/mpi.hpp>
namespace mpi = boost::mpi;

/**
 * \brief Boost Message Passing Interface (MPI) implementation.
 *
 * Boost MPI is a C++ interface that works with many MPI implementations. For
 * example: MPICH1, MPICH2, OpenMPI.
 *
 * Tags: distributed memory, message passing, mpi wrapper.
 */
class CowichanMPI : public Cowichan {
protected: // chaining functions

  void mandel(IntMatrix matrix);
  void randmat(IntMatrix matrix);
  void half(IntMatrix matrixIn, IntMatrix matrixOut);
  void invperc(IntMatrix matrix, BoolMatrix mask);
  void thresh(IntMatrix matrix, BoolMatrix mask);
  void life(BoolMatrix matrixIn, BoolMatrix matrixOut);
  void winnow(IntMatrix matrix, BoolMatrix mask, PointVector points);
  void norm(PointVector pointsIn, PointVector pointsOut);
  void hull(PointVector pointsIn, PointVector pointsOut);
  void outer(PointVector points, Matrix matrix, Vector vector);
  void gauss(Matrix matrix, Vector target, Vector solution);
  void sor(Matrix matrix, Vector target, Vector solution);
  void product(Matrix matrix, Vector candidate, Vector solution);
  real vecdiff(Vector actual, Vector computed);

};

#endif

