#!/bin/sh

jsonSchema=$1

basicTest() {
  $jsonSchema "$1.json"
  idris2 -p contrib -c --no-color "$1.idr"
  cat "$1.idr"

  rm -rf build
  rm "$1.idr"
}
