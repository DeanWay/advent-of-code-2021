#!/bin/bash

cabal install $(cat requirements.txt | tr '\n' ' ') --lib
