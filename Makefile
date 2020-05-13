TOP=.

TARGETS = test quick dftgen voronenko maple opcount opcounts search moddftgen

.PHONY : all
all : $(TARGETS)

include mk/common.mk

#
# GHC flags
#
GHCFLAGS += \
	-hide-all-packages \
	-package arithmoi \
	-package base \
	-package bytestring \
	-package containers \
	-package directory \
	-package exception-mtl \
	-package exception-transformers \
	-package filepath \
	-package hspec \
	-package hspec-core \
	-package language-c-quote \
	-package libltdl \
	-package logict \
	-package mainland-pretty \
	-package modular-arithmetic \
	-package mtl \
	-package primitive \
	-package process \
	-package random \
	-package ref-fd \
	-package srcloc \
	-package symbol \
	-package temporary \
	-package text \
	-package transformers \
	-package vector \
	-package vector-fftw \
	-package HUnit \
	-package QuickCheck

#
# Source locations
#
SRC = \
	Data/Complex/Cyclotomic.hs \
	Data/FlagSet.hs \
	Data/Heterogeneous.hs \
	Data/Modular/Instances.hs \
	Spiral.hs \
	Spiral/Array.hs \
	Spiral/Array/Base.hs \
	Spiral/Array/Computable.hs \
	Spiral/Array/Mutable.hs \
	Spiral/Array/Operators/Mapping.hs \
	Spiral/Array/Operators/Matrix.hs \
	Spiral/Array/Operators/Permute.hs \
	Spiral/Array/Operators/Reduction.hs \
	Spiral/Array/Repr/Complex.hs \
	Spiral/Array/Repr/Compute.hs \
	Spiral/Array/Repr/Concrete.hs \
	Spiral/Array/Repr/Hidden.hs \
	Spiral/Array/Repr/Slice.hs \
	Spiral/Array/Repr/Transform.hs \
	Spiral/Array/Repr/Virtual.hs \
	Spiral/Array/Shape.hs \
	Spiral/Backend/C.hs \
	Spiral/Backend/C/CExp.hs \
	Spiral/Backend/C/Code.hs \
	Spiral/Backend/C/Monad.hs \
	Spiral/Backend/C/Util.hs \
	Spiral/Config.hs \
	Spiral/Driver.hs \
	Spiral/Driver/Monad.hs \
	Spiral/Driver/Opts.hs \
	Spiral/Exp.hs \
	Spiral/FFT/Bluestein.hs \
	Spiral/FFT/CooleyTukey.hs \
	Spiral/FFT/GoodThomas.hs \
	Spiral/FFT/Rader.hs \
	Spiral/Globals.hs \
	Spiral/Monad.hs \
	Spiral/NumberTheory.hs \
	Spiral/OpCount.hs \
	Spiral/Permutation.hs \
	Spiral/Program.hs \
	Spiral/Program/Monad.hs \
	Spiral/Program/Monad.hs-boot \
	Spiral/Program/Syntax.hs \
	Spiral/RootOfUnity.hs \
	Spiral/SPL.hs \
	Spiral/SPL/Run.hs \
	Spiral/Search.hs \
	Spiral/Search/FFTBreakdowns.hs \
	Spiral/Search/Monad.hs \
	Spiral/Search/OpCount.hs \
	Spiral/Search/SFKT.hs \
	Spiral/Util/Name.hs \
	Spiral/Util/Pretty.hs \
	Spiral/Util/Pretty/Maple.hs \
	Spiral/Util/Trace.hs \
	Spiral/Util/Uniq.hs

TESTSRC = \
  src/test/Test/Codegen.hs \
  src/test/Test/FFTW.hs \
  src/test/Test/Factorization.hs \
  src/test/Test/Gen.hs \
  src/test/Test/Instances.hs \
  src/test/Test/Opcount.hs \
  src/test/Test/SPL.hs

#
# all, clean, and distclean targets
#
.PHONY : clean
clean :
	$(_QUIET)rm -rf $(TARGETS) dft*.c dft*.so moddft_*.c moddft_*.so

.PHONY : distclean
distclean : clean
	$(_QUIET)rm -rf dist

test : src/test/Main.hs $(SRC) $(TESTSRC)
	@mkdir -p obj
	$(_QUIET)$(GHC) $(GHCFLAGS) --make $< -isrc/test -odir obj -hidir obj -o $@

quick : src/test/Quick.hs $(SRC) $(TESTSRC)
	@mkdir -p obj
	$(_QUIET)$(GHC) $(GHCFLAGS) --make $< -isrc/test -odir obj -hidir obj -o $@

dftgen : examples/DFTGen.hs $(SRC)
	@mkdir -p obj
	$(_QUIET)$(GHC) $(GHCFLAGS) --make $< -odir obj -hidir obj -o $@

dftgen.prof : dftgen
	@mkdir -p obj
	$(_QUIET)$(GHC) $(GHCFLAGS) --make examples/DFTGen.hs -odir obj -hidir obj \
		-prof -auto-all -caf-all -osuf p_o -hisuf p_hi -hcsuf p_hc -o $@

voronenko : examples/Voronenko.hs $(SRC)
	@mkdir -p obj
	$(_QUIET)$(GHC) $(GHCFLAGS) --make $< -odir obj -hidir obj -o $@

maple : examples/Maple.hs $(SRC)
	@mkdir -p obj
	$(_QUIET)$(GHC) $(GHCFLAGS) --make $< -odir obj -hidir obj -o $@

opcount : examples/OpCount.hs $(SRC)
	@mkdir -p obj
	$(_QUIET)$(GHC) $(GHCFLAGS) --make $< -odir obj -hidir obj -o $@

opcounts : examples/OpCounts.hs $(SRC)
	@mkdir -p obj
	$(_QUIET)$(GHC) $(GHCFLAGS) --make $< -odir obj -hidir obj -o $@

search : examples/Search.hs $(SRC)
	@mkdir -p obj
	$(_QUIET)$(GHC) $(GHCFLAGS) --make $< -odir obj -hidir obj -o $@

moddftgen : examples/ModDFTGen.hs $(SRC)
	@mkdir -p obj
	$(_QUIET)$(GHC) $(GHCFLAGS) --make $< -odir obj -hidir obj -o $@

#
# Profiling
#

%.ps : %.hp
	hp2ps -c $^ >$@

%.pdf : %.ps
	ps2pdf $^ $@

%.folded : %.prof
	cat $^ | ghc-prof-flamegraph >$@

%.svg : %.folded
	cat $^ | flamegraph.pl >$@
