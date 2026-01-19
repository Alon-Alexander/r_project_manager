# Defining Project Inputs: Different Input Formats for project.yaml

## Introduction

The `project.yaml` file defines the configuration for your project. An
important part for this is defining the inputs for your project in a
portable way. This file can be written in several formats, from simple
lists to detailed definitions with metadata. This vignette demonstrates
all the supported formats.

## Format 1: Simple Array (IDs Only)

The simplest format is an array of input IDs:

``` yaml
inputs:
  - feature_table
  - sample_metadata
  - raw_data
```

This format is useful when you just need to list the inputs without
additional metadata.

### Read and Verify

``` r

# Read and verify
pm <- pm_project(project_dir)
inputs <- pm$parse_inputs()
cat("Inputs defined:", paste(names(inputs), collapse = ", "), "\n")
#> Inputs defined: feature_table, sample_metadata, raw_data
```

## Format 2: Array with Metadata

You can add metadata to individual inputs in the array format:

``` yaml
inputs:
  - feature_table
  - sample_metadata:
      description: "Sample metadata with treatment groups"
  - raw_data:
      description: "Raw experimental data"
      md5: "abc123def456"
      size: 1024
```

This allows you to document inputs and optionally include validation
information (like MD5 checksums or file sizes).

### Read and Verify

``` r

# Read and verify
pm <- pm_project(project_dir)
inputs <- pm$parse_inputs()
cat("Inputs defined:", paste(names(inputs), collapse = ", "), "\n")
#> Inputs defined: feature_table, sample_metadata, raw_data
```

## Format 3: Object Format with Metadata

The object format also supports metadata for each input:

``` yaml
inputs:
  feature_table:
    description: "OTU table with feature counts"
  sample_metadata:
    description: "Sample metadata with treatment groups"
    md5: "def456ghi789"
    size: 1024
  raw_data:
    description: "Raw experimental measurements"
    size: 2048
```

This format is often preferred when you have multiple inputs with
metadata, as it’s more readable.

### Read and Verify

``` r

# Read and verify
pm <- pm_project(project_dir)
inputs <- pm$parse_inputs()
cat("Inputs defined:", paste(names(inputs), collapse = ", "), "\n")
#> Inputs defined: feature_table, sample_metadata, raw_data
```

## Mixed Formats

You can also mix simple IDs with detailed definitions in array format:

``` yaml
inputs:
  - simple_input
  - detailed_input:
      description: "This input has metadata"
      md5: "1234567890abcdef"
  - another_simple_input
```

### Read and Verify

``` r

# Read and verify
pm <- pm_project(project_dir)
inputs <- pm$parse_inputs()
cat("Inputs defined:", paste(names(inputs), collapse = ", "), "\n")
#> Inputs defined: simple_input, detailed_input, another_simple_input
```

## Metadata Fields

The following metadata fields are supported (all optional):

- **`description`**: Human-readable description of the input
- **`md5`**: MD5 checksum for validation (future feature)
- **`size`**: File size in bytes (future feature)

**Note**: MD5 and size validation are planned for future versions.
Currently, these fields are stored but not validated.

## Choosing a Format

- **Use Format 1 (Simple Array)** when you just need to list inputs
  quickly
- **Use Format 2 (Array with Metadata)** when you have a mix of simple
  and detailed inputs
- **Use Format 3 (Object with Metadata)** when all inputs have metadata
  and you want maximum readability

All formats are equivalent in functionality - choose based on your
preference and readability needs.

## Summary

The `project.yaml` file is flexible and supports multiple formats:

1.  **Simple array**: Just list input IDs
2.  **Array with metadata**: Add descriptions and validation info to
    specific inputs
3.  **Object with metadata**: Combine object format with detailed
    metadata
4.  **Mixed**: Combine simple and detailed definitions

All formats work with the same `parse_inputs()` method, so you can
choose the format that best fits your project’s needs.

For more information on using inputs in your workflow, see the [Getting
Started](https://alon-alexander.github.io/r_project_manager/articles/getting-started.md)
vignette.
