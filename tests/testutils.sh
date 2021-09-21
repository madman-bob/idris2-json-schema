#!/bin/sh

jsonSchema=$1

basicTest() {
  $jsonSchema "$1.json"
  cat "$1.idr"

  rm "$1.idr"
}
