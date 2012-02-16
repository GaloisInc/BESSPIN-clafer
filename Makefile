SRC_DIR  = src
TEST_DIR = test
SUITE_DIR = $(TEST_DIR)/suite
TOOL_DIR = tools

all: build

install: all
	cabal install --bindir=.

build:
	$(MAKE) -C $(SRC_DIR)
	$(MAKE) -C $(TOOL_DIR)
	ghc -isrc -outputdir dist/build --make -O src/clafer -o clafer

prof:
	$(MAKE) -C $(SRC_DIR)
	$(MAKE) -C $(TOOL_DIR)
	ghc -isrc -prof -auto-all -rtsopts -O -outputdir dist/build --make src/clafer -o clafer

tests:
	find $(SUITE_DIR) -type f -name '*.als' -print0 | xargs -0 rm -f
	find $(SUITE_DIR) -type f -name '*.cfr.des' -print0 | xargs -0 rm -f
	find $(SUITE_DIR) -type f -name '*.xml' -print0 | xargs -0 rm -f
	@if test ! -f $(TOOL_DIR)/"alloy4.jar"; then \
		echo "[ERROR] Missing $(TOOL_DIR)/alloy4.jar."; false; \
	fi
	find $(SUITE_DIR) -type f \( -iname "*.cfr" ! -iname "*.des.cfr" \) | xargs -L1 ./clafer -s -v
	@if test ! -f $(TOOL_DIR)/"XsdCheck.class"; then \
		echo "[ERROR] Missing $(TOOL_DIR)/XsdCheck.class."; false; \
	fi
	find $(SUITE_DIR) -type f \( -iname "*.cfr" ! -iname "*.des.cfr" \) | xargs -L1 ./clafer -s -v -m=xml
	find $(SUITE_DIR) -type f \( -iname "*.cfr" ! -iname "*.des.cfr" \) | xargs -L1 ./clafer -s -v -m=clafer

clean:
	$(MAKE) -C $(SRC_DIR) clean
	$(MAKE) -C $(TOOL_DIR) clean
	rm -rf dist
	rm -f clafer
	find . -type f -name '*.o' -print0 | xargs -0 rm -f
	find . -type f -name '*.hi' -print0 | xargs -0 rm -f
	find . -type f -name '*~' -print0 | xargs -0 rm -f