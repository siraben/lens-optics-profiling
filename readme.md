# Note: this issue has been resolved in https://github.com/well-typed/optics/pull/491
# Haskell Lens vs Optics Performance Under Profiling

This repository contains a minimal, reproducible example for comparing
the performance characteristics of the `lens` and `optics` libraries
in Haskell **under profiling**. We increment a counter nested in a
state monad 1 million times to compare the performance of `lens` and
`optics`.

No differences were observed for this example when running without
profiling and measuring with `+RTS -s -RTS`.

## Setup

The code includes two main Haskell files:

1. `Main-lens.hs`: This is the implementation using the `lens` library.
2. `Main-optics.hs`: This is the implementation using the `optics` library.

Both files perform the same operation: they define a nested structure
with three layers (`Box1`, `Box2`, `Box3`), each containing a single
field. The innermost field `_n` is a counter, which is incremented 1
million times using a `State` monad.

## Profiling and Results

We have profiled the two programs with GHC options `-O2 -fprof-late`,
which enables optimization and late cost-centre profiling. The command
to run the profiling is:

```ShellSession
$ cabal run --enable-profiling {lens|optics}-profiling --ghc-options='-O2 -fprof-late' -- +RTS -p -RTS
```

The results show significant differences in performance:

### Optics Profiling

The `optics` version takes 0.39 seconds and allocates 760,052,384
bytes. The profiling report shows that the `Optics.Internal.Optic`
module takes up 41.3% of the time and 56.8% of the allocations.

```plaintext
total time  =        0.39 secs   (390 ticks @ 1000 us, 1 processor)
total alloc = 760,052,384 bytes  (excludes profiling overheads)

COST CENTRE MODULE                SRC                                            %time %alloc

%           Optics.Internal.Optic src/Optics/Internal/Optic.hs:(120,1)-(130,54)   41.3   56.8
```

### Lens Profiling

The `lens` version takes 0.04 seconds and allocates 88,052,104
bytes. The function `increment` in the `Main` module (the one that
performs the increment operation) takes up 78.0% of the time and 18.2%
of the allocations.

```plaintext
total time  =        0.04 secs   (41 ticks @ 1000 us, 1 processor)
total alloc =  88,052,104 bytes  (excludes profiling overheads)

COST CENTRE MODULE    SRC                       %time %alloc

increment   Main      app/Main-lens.hs:26:1-35   78.0   18.2
```

## Conclusion

From this experiment, it appears that the `lens` library has better
performance characteristics than the `optics` library for this
particular operation under profiling. It seems that the `optics`
library functions are not inlined as the `lens` ones are, leading to
more allocations and time taken.

## License
This repository is licensed under the MIT license.
