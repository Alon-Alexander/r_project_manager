# Validate inputs.local.yaml schema

Validates that the inputs.local.yaml structure follows the expected
schema.

## Usage

``` r
.validate_local_inputs_schema(local_inputs)
```

## Arguments

- local_inputs:

  List. Parsed YAML content from inputs.local.yaml

## Details

Expected schema:

- Must have a top-level "paths" key

- "paths" must be a mapping from input IDs to file paths (strings)
