.PHONY: all install json-schema test retest clean

all: json-schema

install: json-schema
	idris2 --install json-schema.ipkg

json-schema: build/exec/json-schema

build/exec/json-schema: json-schema.ipkg JSONSchema/* JSONSchema/*/* JSONSchema/*/*/*
	idris2 --build json-schema.ipkg

test:
	make -C tests test

retest:
	make -C tests retest

clean:
	rm -rf build
