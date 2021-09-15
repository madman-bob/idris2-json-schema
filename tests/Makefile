.PHONY: test retest clean

json-schema = ${CURDIR}/../build/exec/json-schema

INTERACTIVE ?= --interactive
threads ?= $(shell (nproc || sysctl -n hw.ncpu) 2>/dev/null || echo 1)

test: build/exec/json-schema-tests
	./build/exec/json-schema-tests $(json-schema) $(INTERACTIVE) --timing --failure-file failures --threads $(threads) --only $(only)

retest: build/exec/json-schema-tests
	./build/exec/json-schema-tests $(json-schema) $(INTERACTIVE) --timing --failure-file failures --threads $(threads) --only-file failures --only $(only)

build/exec/json-schema-tests: json-schema-tests.ipkg JSONSchemaTests.idr
	idris2 --build json-schema-tests.ipkg

clean:
	$(RM) failures
	$(RM) -r build
	@find . -type f -name 'output' -exec rm -rf {} \;
