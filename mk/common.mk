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
# Use stack package databases.
#
STACK_PKGDB=$(shell stack path --snapshot-pkg-db)
STACK_LOCAL_PKGDB=$(shell stack path --local-pkg-db)

GHCFLAGS += \
	-clear-package-db \
	-global-package-db \
	-package-db=$(STACK_PKGDB) \
	-package-db=$(STACK_LOCAL_PKGDB)

RUNGHCFLAGS += \
	-clear-package-db \
	-global-package-db \
	-package-db --ghc-arg=$(STACK_PKGDB)

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
