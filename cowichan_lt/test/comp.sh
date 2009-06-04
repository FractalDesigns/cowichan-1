#!/bin/sh
g++ -L../../linuxtuples-1.03 -I../../linuxtuples-1.03 -o mandel_client mandel_client.cpp -llinuxtuples
g++ -L../../linuxtuples-1.03 -I../../linuxtuples-1.03 -o mandel_worker mandel_worker.cpp -llinuxtuples

