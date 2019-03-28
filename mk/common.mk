MAKEFLAGS:=--no-print-directory

_QUIET=@

include $(TOP)/mk/virtual.mk

#
# Useful variables
#
OS=$(shell uname -s | tr "[:upper:]" "[:lower:]")
ARCH=$(shell uname -m)

#
# No built-in rules
#
MAKEFLAGS += --no-builtin-rules

#
# Misc flags
#
GHC=stack ghc --
GHCFLAGS+=-XHaskell2010 -rtsopts -O -Wall -fno-warn-name-shadowing

RUNGHC=stack runghc --
RUNGHCFLAGS+=-W -fno-warn-unused-imports

HAPPY=stack exec happy --
HAPPYFLAGS+=-agci

ALEX=stack exec alex --
ALEXFLAGS+=-gi

#
# GHC optimization flags
#
GHCFLAGS_OPT = -O2 -funbox-strict-fields

ifeq ($(OPT), 1)
GHCFLAGS += $(GHCFLAGS_OPT)
endif

#
# Support cabal sandbox and stack package databases. We prefer stack.
#
STACK_PKGDB=$(HOME)/.stack/snapshots/$(ARCH)-$(OS)/$(STACK_LTS)/pkgdb
LOCAL_STACK_PKGDB=$(TOP)/.stack-work/install/$(ARCH)-$(OS)/$(STACK_LTS)/pkgdb

ifneq ($(wildcard $(STACK_PKGDB)/*.conf),)
GHCFLAGS += \
  -optP-include -optP$(wildcard $(TOP)/.stack-work/dist/$(ARCH)-$(OS)/Cabal-*/build/autogen/cabal_macros.h) \
	-clear-package-db \
	-global-package-db \
	-package-db=$(STACK_PKGDB) \
	-package-db=$(LOCAL_STACK_PKGDB)

RUNGHCFLAGS += \
  -optP-include -optP$(wildcard $(TOP)/.stack-work/dist/$(ARCH)-$(OS)/Cabal-*/build/autogen/cabal_macros.h) \
	-clear-package-db \
	-global-package-db \
	-package-db --ghc-arg=$(STACK_PKGDB)
else ifneq ($(wildcard $(TOP)/.cabal-sandbox/*-packages.conf.d),)
GHCFLAGS += \
  -optP-include -optP$(TOP)/dist/build/autogen/cabal_macros.h \
	-no-user-package-db \
	-package-db $(wildcard $(TOP)/.cabal-sandbox/*-packages.conf.d)

RUNGHCFLAGS += \
  -optP-include -optP$(TOP)/dist/build/autogen/cabal_macros.h \
	-no-user-package-db \
	-package-db --ghc-arg=$(wildcard $(TOP)/.cabal-sandbox/*-packages.conf.d)
endif

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
