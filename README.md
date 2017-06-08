# Building

Building hspiral requires GHC 8.0.

Suggested method for building once the Haskell Platform has been installed:

```
# cabal sandbox init
# cabal install --only-dependencies --enable-tests
# cabal configure --enable-tests
# cabal build
# cabal test
```

If you don't want to run the tests, remove the `--enable-tests` flag from the
command sequence above.

Building the tests requires that `libltdl` and FFTW 3 are installed. On Ubuntu,
that can be accomplished as follows:

```
sudo apt-get install libltdl-dev
sudo apt-get install libfftw3-dev
```

Once you have created the cabal sandbox and installed SPL's dependencies, you
can also type `make` to build the executables explicitly.

## Building on a Mac

I installed the `fftw-3` port from MacPorts and then built a sandbox as follows:

```
# cabal install --only-dependencies --extra-include-dirs=/usr/include --extra-include-dirs=/opt/local/include --extra-lib-dirs=/usr/lib --extra-lib-dirs=/opt/local/lib
# cabal configure --extra-include-dirs=/usr/include --extra-include-dirs=/opt/local/include --extra-lib-dirs=/usr/lib --extra-lib-dirs=/opt/local/lib
```

# Command-line Flags

The `test` binary as well as the examples take optional command-line flags.

| Flag               | Description |
| ---                | --- |
| `-o` | Specify output file for generated code. |

## Code generation flags

The following flags control code generation:

| Flag               | Description |
| ---                | --- |
| `-fmax-unroll=INT` | Unroll loops up to the given size. |
| `-fline-pragmas` | Output #line pragmas in generated code. |
| `-fuse-complex` | Use the C99 `_Complex` type in generated code. |
| `-fthree-mult` | Use the 3-mult/5-add form of complex multiplication. |
| `-fstore-intermediate` | Always store the results of intermediate computations.|
| `-fcse` | Perform common subexpression elimination. |
| `-fsplit-complex` | Always split complex numbers into their real and imaginary parts upon assignment. |

Flag can be prefixed by `-fno-` to turn them off, e.g., `-fno-cse` will turn off common subexpression elimination.

By default, the flags `-fstore-intermediate`, `-fcse`, and `-fsplit-complex` are enabled.

The `-fno-split-complex` flag is useful mainly for examining generated complex values; with `-fsplit-complex`, complex values are split into their real and imaginary parts, so things like roots of unity are not nicely displayed.

## Debugging flags

The following flags control debugging output.

| Flag               | Description |
| ---                | --- |
| `-dtrace-cg` | Trace the code generator. |
| `-dgen-comments` | Add comments to blocks of code that specify which transform the block implements. |

# Examples

### `test`

Runs a small test suite.

### `dftgen`

The `dftgen` binary generates C code for the forward DFT of the size it is given
as an argument.

### `voronenko`

Generates code for example(s) from Voronenko's dissertation (right now just the
one on p. 35).

Should be run like this:

```
./voronenko -fmax-unroll=2 -fuse-complex
```

### `opcount`

The `opcount` binary generates operation counts and, optionally, C code, for the
forward DFT of the size it is given as an argument. It always uses the split
radix decomposition.

### `search`

The `search` binary generates operation counts and, optionally, C code, for the
forward DFT of the size it is given as an argument. It performs search to find
the decomposition with the fewest operations.
