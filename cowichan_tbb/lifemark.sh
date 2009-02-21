#!/bin/bash
#lifemark.sh

g++ -DBOARD_SIZE=20 -ltbb -o life life.cpp
echo -n "20,  "  && `which time` -f %E 2>&1 ./life 
echo -n ", " && `which time` -f %E 2>&1 ./life
echo -n ", " && `which time` -f %E 2>&1 ./life

g++ -DBOARD_SIZE=40 -ltbb -o life life.cpp
echo -n "40,  "  && `which time` -f %E 2>&1 ./life 
echo -n ", " && `which time` -f %E 2>&1 ./life
echo -n ", " && `which time` -f %E 2>&1 ./life

g++ -DBOARD_SIZE=80 -ltbb -o life life.cpp
echo -n "80,  "  && `which time` -f %E 2>&1 ./life 
echo -n ", " && `which time` -f %E 2>&1 ./life
echo -n ", " && `which time` -f %E 2>&1 ./life

g++ -DBOARD_SIZE=160 -ltbb -o life life.cpp
echo -n "160, "  && `which time` -f %E 2>&1 ./life 
echo -n ", " && `which time` -f %E 2>&1 ./life
echo -n ", " && `which time` -f %E 2>&1 ./life

g++ -DBOARD_SIZE=320 -ltbb -o life life.cpp
echo -n "320, "  && `which time` -f %E 2>&1 ./life
echo -n ", " && `which time` -f %E 2>&1 ./life
echo -n ", " && `which time` -f %E 2>&1 ./life

g++ -DBOARD_SIZE=640 -ltbb -o life life.cpp
echo -n "640, "  && `which time` -f %E 2>&1 ./life 
echo -n ", " && `which time` -f %E 2>&1 ./life
echo -n ", " && `which time` -f %E 2>&1 ./life

