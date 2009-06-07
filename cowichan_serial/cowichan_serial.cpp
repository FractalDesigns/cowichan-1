#include "cowichan_serial.hpp"

void CowichanSerial::winnow(IntMatrix /* matrix */, BoolMatrix /* mask */, PointList** /* points */)
{
  std::cout << WINNOW;
}

void CowichanSerial::norm(PointList* /* pointsIn */, PointList** /* pointsOut */)
{
  std::cout << NORM;
}

void CowichanSerial::hull(PointList* /* pointsIn */, PointList** /* pointsOut */)
{
  std::cout << HULL;
}

void CowichanSerial::outer(PointList* /* points */, Matrix* /* matrix */, Vector* /* vector */)
{
  std::cout << OUTER;
}

void CowichanSerial::gauss(Matrix /* matrix */, Vector /* target */, Vector* /* solution */)
{
  std::cout << GAUSS;
}

void CowichanSerial::sor(Matrix /* matrix */, Vector /* target */, Vector* /* solution */)
{
  std::cout << SOR;
}

void CowichanSerial::product(Matrix /* matrix */, Vector /* actual */, Vector* /* solution */)
{
  std::cout << PRODUCT;
}

void CowichanSerial::vecdiff(Vector /* actual */, Vector /* computed */, real* /* e */)
{
  std::cout << VECDIFF;
}

void CowichanSerial::chain(bool /* use_randmat */, bool /* use_thresh */)
{
  std::cout << CHAIN;
}


int main(int argc, char* argv[])
{
  Cowichan* serial = new CowichanSerial ();

  serial->main(argc, argv, false, false);

  return 0;
}

