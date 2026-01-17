# Validate input files exist

Validates that all input files referenced in inputs.local.yaml actually
exist. Also checks that all inputs defined in inputs.yaml have
corresponding entries in inputs.local.yaml.

## Usage

``` r
.validate_input_files(project_path, inputs_file, local_inputs_file)
```

## Arguments

- project_path:

  Character. Path to the project directory

- inputs_file:

  Character. Path to inputs.yaml file

- local_inputs_file:

  Character. Path to inputs.local.yaml file

## Value

Invisibly returns TRUE if validation passes

## Details

This function:

- Reads and validates the schema of both YAML files

- Checks that all input IDs from inputs.yaml have entries in
  inputs.local.yaml

- Checks that all files referenced in inputs.local.yaml exist

- Provides helpful error messages with guidance on how to fix issues
