.PHONY: all json-schema test retest clean

all: json-schema

json-schema: build/exec/json-schema

build/exec/json-schema: json-schema.ipkg JSONSchema/* JSONSchema/*/*
	idris2 --build json-schema.ipkg

test:
	make -C tests test

retest:
	make -C tests retest

clean:
	rm -rf build
