MAKEFLAGS:=--no-print-directory

_QUIET=@

include $(TOP)/mk/virtual.mk

#
# No built-in rules
#
MAKEFLAGS += --no-builtin-rules

#
# Misc flags
#
GHC=ghc
GHCFLAGS+=-XHaskell2010 -rtsopts -O -Wall -fno-warn-name-shadowing -Werror

RUNGHC=runghc
RUNGHCFLAGS+=-W -fno-warn-unused-imports

HAPPY=happy
HAPPYFLAGS+=-agci

ALEX=alex
ALEXFLAGS+=-gi

#
# GHC optimization flags
#
GHCFLAGS_OPT = -O2 -funbox-strict-fields

ifeq ($(OPT), 1)
GHCFLAGS += $(GHCFLAGS_OPT)
endif

#
# Support cabal sandbox
#
ifneq ($(wildcard $(TOP)/.cabal-sandbox/*-packages.conf.d),)
GHCFLAGS += \
	-no-user-package-db \
	-package-db $(wildcard $(TOP)/.cabal-sandbox/*-packages.conf.d)

RUNGHCFLAGS += \
	-no-user-package-db \
	-package-db --ghc-arg=$(wildcard $(TOP)/.cabal-sandbox/*-packages.conf.d)
endif

#
# Support Cabal's MIN_VERSION
#
RUNGHCFLAGS += -optP-include -optP$(TOP)/dist/build/autogen/cabal_macros.h
GHCFLAGS += -optP-include -optP$(TOP)/dist/build/autogen/cabal_macros.h

#
# Print Makefile variables
#
print-%: ; @echo $*=$($*)

#
# Rules for virtual goals
#
ifeq ($(MAKECMDGOALS),)
$(VIRTUAL_GOALS) : all
	@true
else
$(VIRTUAL_GOALS) :
	@true
endif
