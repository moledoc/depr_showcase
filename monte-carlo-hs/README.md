<!-- README: This project was made to show the parallel aspects of Haskell. Also, it was submitted as a homework in parallel computation course. The Monte-Carlo method was used as an example, since MC method is embarrasingly parallel. In this project speedup was not achieved. Furthermore, the Haskell implementation can be improved upon. -->

## Name

Serial and parallel example of Monte-Carlo method in Haskell.

## Purpose

In this project, the main idea was to explore parallelism in Haskell, using Monte-Carlo mehtod as an example.
The first idea was to use package _mpi-hs_. Due to some unresolved dependencies, I needed to change my direction.
In the end, I used package called _parallel_.
Unfortunately, the serial and parallel implementations are really slow, when the number of iterations in MC method increases. I suspect it is due to inefficient implementation of random number generation.
There is a report that gives (really) brief overview of parallelism in Haskell.
It also presents an argument that this code is slow when number of iterations n is big. This is illustrated with an example written in R.

## Setup

Install Haskell stack. Then run the following commands.

```sh
stack update; stack setup
```

For this project, the following packages are necessary

```sh
stack install random parallel #normaldistribution 
```

If there are problems, then add following to $HOME/.stack/global-project/stack.yaml

```yaml
extra-deps:
- random-1.1
 %% - random-1.2
 %% - normaldistribution-1.1.0.3
```

## Compiling and running

The help programs (Timing.hs, Function.hs, GenUnif.hs) were compiled using command

```sh
stack ghc -- -dynamic <filename>.hs
```

The main programs were compiled and ran using commands.

```sh
stack ghc -- -threaded -rtsopts -eventlog -main-is MC<type> MC<type>.hs
./MC<type> +RTS -N
# or
./MC<type> +RTS -N -s
```

## Author

Written by Meelis Utt

