#include "cowichan_serial.hpp"

int main(int argc, char* argv[])
{
  Cowichan* serial = new CowichanSerial ();

  serial->main(argc, argv, false, true);

  return 0;
}

