#!/bin/sh

jsonSchema=$1

basicTest() {
  fileName=$1
  shift

  $jsonSchema "$fileName.json" "$@"
  idris2 -p contrib -c --no-color "$fileName.idr"
  cat "$fileName.idr"

  rm -rf build
  rm "$fileName.idr"
}
