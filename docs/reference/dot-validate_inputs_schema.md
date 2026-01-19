# Validate project.yaml schema

Validates that the project.yaml structure follows the expected schema.

## Usage

``` r
.validate_inputs_schema(inputs_def)
```

## Arguments

- inputs_def:

  List. Parsed YAML content from project.yaml

## Details

Expected schema:

- Must have a top-level "inputs" key

- "inputs" must be a YAML list (array) or object (named list)

- If array: each element can be a string (ID only) or an object with ID
  as key

- If object: maps input IDs to their definitions

- Each input definition can be an empty object () or null (just an ID)

- Optional fields: "expected" (for validation), fingerprint fields (md5,
  size, etc.)
