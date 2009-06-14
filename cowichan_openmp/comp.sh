#!/bin/sh
g++ -Wall -fopenmp -O2 -c ../cowichan/cowichan.cpp
g++ -Wall -fopenmp -O2 -o cowichan_openmp *.cpp cowichan.o
