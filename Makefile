TOP=.

TARGETS = test dftgen voronenko

.PHONY : all
all : $(TARGETS)

include mk/common.mk

#
# GHC flags
#
GHCFLAGS += \
	-hide-all-packages \
	-package base \
	-package bytestring \
	-package containers \
	-package exception-mtl \
	-package exception-transformers \
	-package language-c-quote \
	-package libltdl \
	-package mainland-pretty \
	-package mtl \
	-package primitive \
	-package process \
	-package ref-fd \
	-package srcloc \
	-package symbol \
	-package test-framework \
	-package test-framework-hunit \
	-package test-framework-quickcheck2 \
	-package text \
	-package transformers \
	-package vector \
	-package vector-fftw \
	-package HUnit \
	-package QuickCheck

GHCFLAGS_OPT = -O2 -funbox-strict-fields

ifeq ($(OPT), 1)
GHCFLAGS += $(GHCFLAGS_OPT)
endif

#
# Source locations
#
SRC = \
	Spiral.hs \
	Spiral/Array.hs \
	Spiral/Array/Base.hs \
	Spiral/Array/Operators/IndexSpace.hs \
	Spiral/Array/Operators/Mapping.hs \
	Spiral/Array/Operators/Permute.hs \
	Spiral/Array/Operators/Reduction.hs \
	Spiral/Array/Program.hs \
	Spiral/Array/Repr/Complex.hs \
	Spiral/Array/Repr/Compute.hs \
	Spiral/Array/Repr/Slice.hs \
	Spiral/Array/Repr/Transform.hs \
	Spiral/Array/Repr/Virtual.hs \
	Spiral/Array/Shape.hs \
	Spiral/Backend/C.hs \
	Spiral/Backend/C/CExp.hs \
	Spiral/Backend/C/Code.hs \
	Spiral/Backend/C/Monad.hs \
	Spiral/Backend/C/Util.hs \
	Spiral/Driver.hs \
	Spiral/Driver/Config.hs \
	Spiral/Driver/Globals.hs \
	Spiral/Driver/Monad.hs \
	Spiral/Driver/Opts.hs \
	Spiral/Exp.hs \
	Spiral/ExtendedFloat.hs \
	Spiral/FFT.hs \
	Spiral/SPL.hs \
	Spiral/SPL/Run.hs \
	Spiral/Util/Name.hs \
	Spiral/Util/Pretty.hs \
	Spiral/Util/Trace.hs \
	Spiral/Util/Uniq.hs

TESTSRC = \
  src/test/Test/FFTW.hs \
  src/test/Test/Gen.hs

#
# all, clean, and distclean targets
#
.PHONY : clean
clean :
	$(_QUIET)cabal clean
	$(_QUIET)rm -rf $(TARGETS)

.PHONY : distclean
distclean : clean
	$(_QUIET)rm -rf dist

test : src/test/Main.hs $(SRC) $(TESTSRC) dist/build/autogen/cabal_macros.h
	@mkdir -p obj
	$(_QUIET)$(GHC) $(GHCFLAGS) --make $< -isrc/test -odir obj -hidir obj -o $@

dftgen : examples/DFTGen.hs $(SRC) dist/build/autogen/cabal_macros.h
	@mkdir -p obj
	$(_QUIET)$(GHC) $(GHCFLAGS) --make $< -odir obj -hidir obj -o $@

dftgen.prof : dftgen
	@mkdir -p obj
	$(_QUIET)$(GHC) $(GHCFLAGS) --make examples/DFTGen.hs -odir obj -hidir obj \
		-prof -auto-all -caf-all -osuf p_o -hisuf p_hi -hcsuf p_hc -o $@

voronenko : examples/Voronenko.hs $(SRC) dist/build/autogen/cabal_macros.h
	@mkdir -p obj
	$(_QUIET)$(GHC) $(GHCFLAGS) --make $< -odir obj -hidir obj -o $@

dist/build/autogen/cabal_macros.h :
	cabal build

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
