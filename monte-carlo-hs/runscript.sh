#!/bin/sh

# help script to run files.
# compile haskell codes with threading enabled.
stack ghc -- -threaded -rtsopts -eventlog -main-is MCserial MCserial.hs
stack ghc -- -threaded -rtsopts -eventlog -main-is MCconcurrent MCconcurrent.hs
stack ghc -- -threaded -rtsopts -eventlog -main-is MCparallel MCparallel.hs
# set n, the nr of iterations in Monte-Carlo method.
n=100000
time ./MCserial $n +RTS -N
time ./MCconcurrent $n +RTS -N
time ./MCparallel $n +RTS -N
# ./MCserial +RTS -N -s
# ./MCparallel +RTS -N -s
