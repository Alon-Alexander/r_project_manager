# Extract input IDs from inputs.yaml

Extracts the canonical input IDs from the inputs.yaml structure.
Supports both array format (list of strings/objects) and object format
(named list).

## Usage

``` r
.extract_input_ids(inputs_def)
```

## Arguments

- inputs_def:

  List. Parsed YAML content from inputs.yaml

## Value

Character vector of input IDs
