
TODO

# Setup

Install Haskell stack. Then

```sh
stack update; stack setup
```

For this project, the following packages are necessary

```sh
stack install random normaldistribution parallel
```

If there are problems, then add following to $HOME/.stack/global-project/stack.yaml

```yaml
extra-deps:
- random-1.1
%% - random-1.2
%% - normaldistribution-1.1.0.3
```

## Compiling and running

```sh
stack ghc -- -threaded -rtsopts -eventlog -main-is MCserial MCserial.hs
./MCserial +RTS -N
./MCserial +RTS -N -s
```



## Author

Written by Meelis Utt

