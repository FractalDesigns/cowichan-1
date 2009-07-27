#include "cowichan_serial.hpp"

/**
 * \file cowichan_serial.cpp
 * \brief This file contains main method that drives the serial implementation.
 */

/**
 * Main method - creates a CowichanSerial instance and executes Cowichan::main.
 * \param argc number of command line arguments.
 * \param argv command line arguments.
 */
int main(int argc, char* argv[])
{
  Cowichan* serial = new CowichanSerial ();

  serial->main(argc, argv, false, true);

  return 0;
}

