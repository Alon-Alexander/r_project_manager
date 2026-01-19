# Extract input IDs from project.yaml

Extracts the canonical input IDs from the project.yaml structure.
Supports both array format (list of strings/objects) and object format
(named list).

## Usage

``` r
.extract_input_ids(inputs_def)
```

## Arguments

- inputs_def:

  List. Parsed YAML content from project.yaml

## Value

Character vector of input IDs
