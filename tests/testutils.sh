#!/bin/sh

jsonSchema=$1

basicTest() {
  fileName=$1
  shift

  $jsonSchema "$fileName.json" "$@"
  idris2 -p contrib -p json-schema -c --no-color "$fileName.idr"
  cat "$fileName.idr"

  rm -rf build
  rm "$fileName.idr"
}

supportTest() {
  idris2 -p contrib -p json-schema --quiet --no-color "$@" | sed 's/Main> //' | sed 's/\(Main> \)\+/\n/'

  rm -rf build
}
