# Validate input files exist

Validates that all input files referenced in inputs.local.yaml actually
exist. Also checks that all inputs defined in project.yaml have
corresponding entries in inputs.local.yaml.

## Usage

``` r
.validate_input_files(project_path, configuration_file, local_inputs_file)
```

## Arguments

- project_path:

  Character. Path to the project directory

- configuration_file:

  Character. Path to project.yaml file

- local_inputs_file:

  Character. Path to inputs.local.yaml file

## Value

Invisibly returns TRUE if validation passes

## Details

This function:

- Reads and validates the schema of both YAML files

- Checks that all input IDs from project.yaml have entries in
  inputs.local.yaml

- Checks that all files referenced in inputs.local.yaml exist

- Provides helpful error messages with guidance on how to fix issues
