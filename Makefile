.POSIX:

.SUFFIXES:

prefix = /usr/local

DUNE = dune
DUNE_FLAGS =

all: lib

.PHONY: lib
lib:
	$(DUNE) $(DUNE_FLAGS) build

.PHONY: check
check:
	$(DUNE) $(DUNE_FLAGS) runtest

.PHONY: install
install:
	$(DUNE) $(DUNE_FLAGS) install --prefix=$(prefix)
