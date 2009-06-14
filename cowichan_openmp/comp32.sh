#!/bin/sh
g++ -Wall -m32 -fopenmp -O2 -c ../cowichan/cowichan.cpp
g++ -Wall -m32 -fopenmp -O2 -o cowichan_openmp *.cpp cowichan.o
