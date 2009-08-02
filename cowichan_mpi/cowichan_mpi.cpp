/**
 * \file cowichan_mpi.cpp
 * \brief This file contains main method that drives the MPI implementation.
 */

#include "cowichan_mpi.hpp"

/**
 * Main method - creates a CowichanMPI instance and executes Cowichan::main.
 * \param argc number of command line arguments.
 * \param argv command line arguments.
 */
int main(int argc, char* argv[])
{
  mpi::environment env(argc, argv);
  mpi::communicator world;

#ifdef OUTPUT_DATA
  std::cout << "I am process" << world.rank() << std::endl;
#endif

  Cowichan* mpi = new CowichanMPI ();

  mpi->main(argc, argv, false, true);

  return 0;
}

void CowichanMPI::mandel(IntMatrix matrix) {}
void CowichanMPI::randmat(IntMatrix matrix) {}
void CowichanMPI::half(IntMatrix matrixIn, IntMatrix matrixOut) {}
void CowichanMPI::invperc(IntMatrix matrix, BoolMatrix mask) {}
void CowichanMPI::thresh(IntMatrix matrix, BoolMatrix mask) {}
void CowichanMPI::life(BoolMatrix matrixIn, BoolMatrix matrixOut) {}
void CowichanMPI::norm(PointVector pointsIn, PointVector pointsOut) {}
void CowichanMPI::hull(PointVector pointsIn, PointVector pointsOut) {}
void CowichanMPI::outer(PointVector points, Matrix matrix, Vector vector) {}
void CowichanMPI::gauss(Matrix matrix, Vector target, Vector solution) {}
void CowichanMPI::sor(Matrix matrix, Vector target, Vector solution) {}
void CowichanMPI::product(Matrix matrix, Vector candidate, Vector solution) {}
real CowichanMPI::vecdiff(Vector actual, Vector computed) {return 0.0;}

