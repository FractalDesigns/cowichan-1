g++ -Wall -O2 -D WIN32 -c ../cowichan/cowichan.cpp
g++ -Wall -O2 -D WIN32 -o serial *.cpp cowichan.o
