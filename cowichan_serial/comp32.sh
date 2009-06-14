#!/bin/sh
g++ -Wall -m32 -O2 -c ../cowichan/cowichan.cpp
g++ -Wall -m32 -O2 -o cowichan_serial *.cpp cowichan.o
