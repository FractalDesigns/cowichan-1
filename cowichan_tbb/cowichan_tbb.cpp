#include "cowichan_tbb.hpp"

int main(int argc, char* argv[])
{
  Cowichan* tbb = new CowichanTBB ();

  task_scheduler_init init(2);

  tbb->main(argc, argv, false, true);

  return 0;
}

void CowichanTBB::life(BoolMatrix matrixIn, BoolMatrix matrixOut) {}
void CowichanTBB::winnow(IntMatrix matrix, BoolMatrix mask, PointVector points) {}
void CowichanTBB::norm(PointVector pointsIn, PointVector pointsOut) {}
void CowichanTBB::hull(PointVector pointsIn, PointVector pointsOut) {}
void CowichanTBB::outer(PointVector points, Matrix matrix, Vector vector) {}
void CowichanTBB::gauss(Matrix matrix, Vector target, Vector solution) {}
void CowichanTBB::sor(Matrix matrix, Vector target, Vector solution) {}
void CowichanTBB::product(Matrix matrix, Vector candidate, Vector solution) {}
real CowichanTBB::vecdiff(Vector actual, Vector computed) {return (real)0;}

