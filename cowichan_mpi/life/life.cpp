/**
 * Conway's game of life
 *
 * \file life.cpp
 * \author Andrew Borzenko
 * \date 01-26-09
 */

#include "../include/stdafx.h"
#include "serial.h"
#include "parallel.h"

int main(int argc, _TCHAR* argv[])
{
  bool2D world; /* world to evolve */
  int    nr;    /* row size */
  int    nc;    /* column size */
  int    iters; /* number of iterations */

  int i, j;

  printf ("serial execution...\n");

  //srand ((unsigned int) time (NULL));
  srand (222);

  for (i = 0; i < MAXEXT; i++)
  {
    for (j = 0; j < MAXEXT; j++)
    {
      world[i][j] = rand () % 2;
    }
  }

  nr = MAXEXT;
  nc = MAXEXT;
  iters = 10;

  life (world, nr, nc, iters);

  print_world (world, nr, nc);

  printf ("parallel execution...\n");

  //srand ((unsigned int) time (NULL));
  srand (222);

  for (i = 0; i < MAXEXT; i++)
  {
    for (j = 0; j < MAXEXT; j++)
    {
      world[i][j] = rand () % 2;
    }
  }

  nr = MAXEXT;
  nc = MAXEXT;
  iters = 10;

  life_mpi (world, nr, nc, iters);

  print_world (world, nr, nc);

  return 0;
}
