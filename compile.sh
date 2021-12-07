#!/bin/bash

COMPILE_FLAGS="-no-keep-o-files -no-keep-hi-files"
ghc $COMPILE_FLAGS -main-is $1.main $1.hs
