# Format validation error message

Formats a helpful error message for input validation failures.

## Usage

``` r
.format_validation_error(missing_entries, missing_files_details)
```

## Arguments

- missing_entries:

  Character vector. Input IDs missing from inputs.local.yaml

- missing_files_details:

  List. Output from .check_missing_files()

## Value

Character. Formatted error message
