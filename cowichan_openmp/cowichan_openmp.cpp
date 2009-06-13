#include "cowichan_openmp.hpp"

int main(int argc, char* argv[])
{
  Cowichan* openmp = new CowichanOpenMP ();

  openmp->main(argc, argv, false, true);

  return 0;
}

