# Building

Building hspiral requires GHC 7.10.3 (GHC 8 has not been tested).

Suggested method for building once the Haskell Platform has been installed:

```
# cabal sandbox init
# cabal install --only-dependencies --enable-tests
# cabal configure --enable-tests
# cabal build
# cabal test
```

Building the test requires that FFTW 3 is installed. On Ubuntu, that can be
accomplished as follows:

```
sudo apt-get install libfftw3-dev
```

If you don't want to run the tests, remove the `--enable-tests` flag from the
command sequence above.

Once you have created the cabal sandbox and installed SPL's dependencies, you
can also type `make` to build the executables explicitly.

# Command-line Flags

The `test` binary as well as the examples take optional command-line flags.

| Flag               | Description |
| ---                | --- |
| `-o` | Specify output file for generated code. |
| `-fmax-unroll=INT` | Unroll loops up to the given size. |
| `-fline-pragmas` | Output #line pragmas in generated code. |
| `-fuse-complex` | Use the C99 `_Complex` type in generated code. |
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
