#!/bin/sh
g++ -Wall -m32 -Os -Llib -Iinclude -c ../cowichan/cowichan.cpp
g++ -Wall -m32 -Os -Llib -Iinclude -o cowichan_lt *.cpp cowichan.o
