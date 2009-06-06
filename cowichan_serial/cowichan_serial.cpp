#include "cowichan_serial.hpp"

void CowichanSerial::randmat(IntMatrix* matrix)
{
  printf(RANDMAT);
}

void CowichanSerial::half(IntMatrix matrixIn, IntMatrix* matrixOut)
{
  printf(HALF);
}

void CowichanSerial::invperc(IntMatrix matrix, BoolMatrix* mask)
{
  printf(INVPERC);
}

void CowichanSerial::thresh(IntMatrix matrix, BoolMatrix* mask)
{
  printf(THRESH);
}

void CowichanSerial::life(BoolMatrix matrixIn, BoolMatrix* matrixOut)
{
  printf(LIFE);
}

void CowichanSerial::winnow(IntMatrix matrix, BoolMatrix mask, PointList** points)
{
  printf(WINNOW);
}

void CowichanSerial::norm(PointList* pointsIn, PointList** pointsOut)
{
  printf(NORM);
}

void CowichanSerial::hull(PointList* pointsIn, PointList** pointsOut)
{
  printf(HULL);
}

void CowichanSerial::outer(PointList* points, Matrix* matrix, Vector* vector)
{
  printf(OUTER);
}

void CowichanSerial::gauss(Matrix matrix, Vector target, Vector* solution)
{
  printf(GAUSS);
}

void CowichanSerial::sor(Matrix matrix, Vector target, Vector* solution)
{
  printf(SOR);
}

void CowichanSerial::product(Matrix matrix, Vector actual, Vector* solution)
{
  printf(PRODUCT);
}

void CowichanSerial::vecdiff(Vector actual, Vector computed, real* e)
{
  printf(VECDIFF);
}

void CowichanSerial::chain(bool use_randmat, bool use_thresh)
{
  printf(CHAIN);
}


int main(int argc, char* argv[])
{
  Cowichan* serial = new CowichanSerial ();

  serial->main(argc, argv, false, false);

  return 0;
}

