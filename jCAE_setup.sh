#!/bin/sh

mkdir -p lib ext

cd ext
echo Cloning jCAE
#git clone https://github.com/jeromerobert/jCAE.git
git clone https://github.com/dzufferey/jCAE.git

cd jCAE/occjava

echo Building Swig bindings
mkdir build
cd build
cmake ..
make
cd ..

echo Building Java part
ant

echo Copying files to lib folder
cd ../../..
cp ext/jCAE/occjava/lib/occjava.jar lib/
cp ext/jCAE/occjava/build/libOccJava.so lib/
