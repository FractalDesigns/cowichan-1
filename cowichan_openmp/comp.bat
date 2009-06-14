g++ -Wall -fopenmp -O2 -D WIN32 -c ../cowichan/cowichan.cpp
g++ -Wall -fopenmp -O2 -D WIN32 -o cowichan_openmp *.cpp cowichan.o
