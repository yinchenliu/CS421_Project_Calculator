# Variables
STACK=stack
BUILD_DIR=.stack-work

# Targets
.PHONY: all build test clean ghci run

all: build

build:
	$(STACK) build

test:
	$(STACK) test

clean:
	$(STACK) clean
	rm -rf $(BUILD_DIR)

ghci:
	$(STACK) ghci

run:
	$(STACK) ghci --ghci-options="-e main"


