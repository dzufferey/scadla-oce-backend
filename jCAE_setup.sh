#!/bin/sh

mkdir -p lib ext

cd ext
echo Cloning jCAE
if test -d jCAE ; then
    cd jCAE
    git pull
    cd ..
else
    #git clone https://github.com/jeromerobert/jCAE.git
    git clone https://github.com/dzufferey/jCAE.git
fi

cd jCAE/occjava

echo Building Swig bindings
mkdir -p build
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
