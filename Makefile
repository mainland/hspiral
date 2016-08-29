TOP=.

.PHONY : all
all : test dftgen

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
	-package mainland-pretty \
	-package mtl \
	-package ref-fd \
	-package srcloc \
	-package test-framework \
	-package test-framework-hunit \
	-package test-framework-quickcheck2 \
	-package text \
	-package transformers \
	-package vector \
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
	Spiral/Backend/C.hs \
	Spiral/Backend/C/CExp.hs \
	Spiral/Backend/C/Code.hs \
	Spiral/Backend/C/Monad.hs \
	Spiral/Backend/C/Types.hs \
	Spiral/Backend/C/Util.hs \
	Spiral/Config.hs \
	Spiral/Driver.hs \
	Spiral/Driver/Opts.hs \
	Spiral/Exp.hs \
	Spiral/ExtendedFloat.hs \
	Spiral/FFT.hs \
	Spiral/Monad.hs \
	Spiral/SPL.hs \
	Spiral/Shape.hs \
	Spiral/Trace.hs \
	Spiral/Util/Lift.hs \
	Spiral/Util/Pretty.hs \
	Spiral/Util/Uniq.hs

#
# all, clean, and distclean targets
#
.PHONY : clean
clean :
	$(_QUIET)cabal clean
	$(_QUIET)rm -rf test

.PHONY : distclean
distclean : clean
	$(_QUIET)rm -rf dist

test : src/Test.hs $(SRC) dist/build/autogen/cabal_macros.h
	@mkdir -p obj
	$(_QUIET)$(GHC) $(GHCFLAGS) --make $< -odir obj -hidir obj -o $@

dftgen : examples/DFTGen.hs $(SRC) dist/build/autogen/cabal_macros.h
	@mkdir -p obj
	$(_QUIET)$(GHC) $(GHCFLAGS) --make $< -odir obj -hidir obj -o $@

dist/build/autogen/cabal_macros.h :
	cabal build
