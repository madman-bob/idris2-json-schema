# Idris2-JSON-Schema

Generate Idris 2 data types from a [JSON Schema](http://json-schema.org/).

## Build

Run:

```bash
make all
```

## Generate Idris 2 types

Run:

```bash
.build/exec/json-schema schemaFile.json
```

to generate `schemaFile.idr`.

## Example

Suppose we have a JSON schema `filmSchema.json`, given by:

```json
{
  "type": "object",
  "properties": {
    "title": {
      "type": "string"
    },
    "year": {
      "type": "number"
    }
  }
}
```

Then when we run `./build/exec/json-schema filmSchema.json`, we generate the `filmSchema.idr` file, with contents:

```idris
public export
record Main where
    constructor MkMain
    title : String
    year : Double
```
