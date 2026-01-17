# Check for missing files

Checks which files referenced in inputs.local.yaml do not exist.

## Usage

``` r
.check_missing_files(local_paths, project_path)
```

## Arguments

- local_paths:

  Named list. Paths from inputs.local.yaml (input ID -\> path)

- project_path:

  Character. Path to the project directory

## Value

List with components:

- ids: Character vector of input IDs with missing files

- abs_paths: Character vector of resolved absolute paths

- original_paths: Character vector of original paths from YAML
