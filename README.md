Building hspiral requires GHC 7.10.3 (GHC 8 has not been tested).

Suggested method for building once the Haskell Platform has been installed:

```
# cabal sandbox init
# cabal install --only-dependencies --enable-tests
# cabal configure --enable-tests
# cabal build
# cabal test
```

Once you have created the cabal sandbox and installed SPL's dependencies, you
can also type `make` to build the `test` and `dftgen` executables explicitly.
