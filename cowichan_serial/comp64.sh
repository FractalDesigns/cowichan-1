#!/bin/sh
g++ -Wall -m64 -O2 -c ../cowichan/cowichan.cpp
g++ -Wall -m64 -O2 -o cowichan_serial *.cpp cowichan.o
