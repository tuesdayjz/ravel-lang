DUNE ?= opam exec -- dune
OPAM ?= opam
PREFIX ?= $(HOME)/.local
BINDIR ?= $(PREFIX)/bin
DESTDIR ?=

.PHONY: all deps build test install uninstall clean

all: ravel

deps:
	$(OPAM) install . --deps-only --with-test

build:
	$(DUNE) build @all

ravel:
	$(DUNE) build src/main.exe
	cp _build/default/src/main.exe $@

ravel_test:
	$(DUNE) build test/test_main.exe
	cp _build/default/test/test_main.exe $@

test:
	$(DUNE) runtest

install: ravel
	mkdir -p $(DESTDIR)$(BINDIR)
	install -m 755 ravel $(DESTDIR)$(BINDIR)/ravel
	@echo "Installed ravel to $(DESTDIR)$(BINDIR)/ravel"

uninstall:
	rm -f $(DESTDIR)$(BINDIR)/ravel
	@echo "Removed $(DESTDIR)$(BINDIR)/ravel"

clean:
	$(DUNE) clean
	rm -f ravel ravel_test out.txt
