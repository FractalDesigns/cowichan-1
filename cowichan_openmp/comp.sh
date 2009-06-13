#!/bin/sh
g++ -Wall -O2 -c ../cowichan/cowichan.cpp
g++ -Wall -O2 -o cowichan_openmp *.cpp cowichan.o
