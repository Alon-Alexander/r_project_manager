# Check for missing entries in inputs.local.yaml

Checks which input IDs from inputs.yaml are missing from
inputs.local.yaml paths.

## Usage

``` r
.check_missing_entries(input_ids, local_paths)
```

## Arguments

- input_ids:

  Character vector. Input IDs from inputs.yaml

- local_paths:

  Named list. Paths from inputs.local.yaml

## Value

Character vector of missing input IDs
