OCAMLOPT ?= ocamlopt
PREFIX ?= $(HOME)/.local
BINDIR ?= $(PREFIX)/bin
DESTDIR ?=
SRC_DIR := src
TEST_DIR := test

COMMON_SOURCES := \
	$(SRC_DIR)/ravel_error.ml \
	$(SRC_DIR)/ast.ml \
	$(SRC_DIR)/token.ml \
	$(SRC_DIR)/interaction_net.ml \
	$(SRC_DIR)/lexer.ml \
	$(SRC_DIR)/parser.ml \
	$(SRC_DIR)/compiler.ml \
	$(SRC_DIR)/driver.ml

CLI_SOURCES := $(COMMON_SOURCES) \
	$(SRC_DIR)/cli.ml \
	$(SRC_DIR)/main.ml

TEST_SOURCES := $(COMMON_SOURCES) \
	$(TEST_DIR)/test_support.ml \
	$(TEST_DIR)/test_parse.ml \
	$(TEST_DIR)/test_eval.ml \
	$(TEST_DIR)/test_error.ml \
	$(TEST_DIR)/test_main.ml

.PHONY: all test install uninstall clean

all: ravel

ravel: $(CLI_SOURCES)
	$(OCAMLOPT) -I $(SRC_DIR) $(CLI_SOURCES) -o $@

ravel_test: $(TEST_SOURCES)
	$(OCAMLOPT) -I $(SRC_DIR) -I $(TEST_DIR) $(TEST_SOURCES) -o $@

test: ravel_test
	./ravel_test

install: ravel
	mkdir -p $(DESTDIR)$(BINDIR)
	install -m 755 ravel $(DESTDIR)$(BINDIR)/ravel
	@echo "Installed ravel to $(DESTDIR)$(BINDIR)/ravel"
	@echo "If 'ravel' is still not found, add this to your shell rc:"
	@echo "  export PATH=\"$(BINDIR):\$$PATH\""

uninstall:
	rm -f $(DESTDIR)$(BINDIR)/ravel
	@echo "Removed $(DESTDIR)$(BINDIR)/ravel"

clean:
	rm -f ravel ravel_test *.cmx *.cmi *.o $(SRC_DIR)/*.cmx $(SRC_DIR)/*.cmi $(SRC_DIR)/*.o $(TEST_DIR)/*.cmx $(TEST_DIR)/*.cmi $(TEST_DIR)/*.o out.txt
