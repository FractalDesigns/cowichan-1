#include "cowichan_serial.hpp"

real CowichanSerial::vecdiff(Vector /* actual */, Vector /* computed */)
{
  std::cout << VECDIFF;
  return (real)0.4;
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

