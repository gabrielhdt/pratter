.POSIX:

.SUFFIXES:

DUNE = dune

all: lib

lib:
	$(DUNE) build

check:
	$(DUNE) runtest
