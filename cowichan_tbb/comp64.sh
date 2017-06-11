#!/bin/sh
# Tested on Linux Mint 18.1 (Ubuntu 16.04)
# Ubuntu has the --as-needed flag enabled, so pass -ltbb as last argument
# First, sudo apt-get install libtbb-dev
g++ -Wall -m64 -std=c++11 -O3 -D LIN64 -c ../cowichan/cowichan.cpp -ltbb 
g++ -Wall -m64 -std=c++11 -O3 -D LIN64 -o cowichan_tbb *.cpp cowichan.o -ltbb 
