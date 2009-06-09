#!/bin/sh
g++ -Wall -O2 -c ../cowichan/cowichan.cpp
g++ -Wall -O2 -o serial *.cpp cowichan.o
