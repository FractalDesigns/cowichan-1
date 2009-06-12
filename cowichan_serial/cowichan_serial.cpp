#include "cowichan_serial.hpp"

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

