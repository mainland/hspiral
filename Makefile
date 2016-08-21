TOP=.

.PHONY : all
all : test fft

include mk/common.mk

#
# GHC flags
#
GHCFLAGS += \
	-hide-all-packages \
	-package array \
	-package base \
	-package containers \
	-package exception-mtl \
	-package exception-transformers \
	-package language-c-quote \
	-package mainland-pretty \
	-package mtl \
	-package test-framework \
	-package test-framework-hunit \
	-package test-framework-quickcheck2 \
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
	SPL/Exp.hs \
	SPL/ExtendedFloat.hs \
	SPL/FFT.hs \
	SPL/Lift.hs \
	SPL/Pretty.hs \
	SPL/Syntax.hs

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
	$(_QUIET)$(GHC) $(GHCFLAGS) --make $^ -odir obj -hidir obj -o $@

fft : examples/FFT.hs $(SRC) dist/build/autogen/cabal_macros.h
	@mkdir -p obj
	$(_QUIET)$(GHC) $(GHCFLAGS) --make $^ -odir obj -hidir obj -o $@

dist/build/autogen/cabal_macros.h :
	cabal build
