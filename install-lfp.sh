#!/bin/bash

cd $1
autoreconf -i -f
mkdir build
cd build
../configure
make
