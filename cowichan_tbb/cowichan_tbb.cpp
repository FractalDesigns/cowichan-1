#include "cowichan_tbb.hpp"

int main(int argc, char* argv[])
{
  Cowichan* tbb = new CowichanTBB ();

  task_scheduler_init init(2);

  tbb->main(argc, argv, false, true);

  return 0;
}

