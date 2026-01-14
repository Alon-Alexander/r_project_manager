# pm 0.1.1

## New Features

- Add `pm_read_file()` function to read files based on their extension
- Support reading CSV, TSV, Parquet (`.parquet`, `.pqt`), RDS, and RData (`.rdata`, `.rda`) files
- Add `read()` method to `PMData` class for reading data files
- For RData files, objects are loaded into a new environment and returned
- Add comprehensive tests for file reading functionality
- Add `arrow` package as suggested dependency for Parquet file support

# pm 0.1.0

## New Features

- Add `PMData` R6 class to represent input data files with ID and path
- Add `parse_inputs()` method to `PMProject` for parsing inputs.yaml and inputs.local.yaml
- Implement YAML schema validation for inputs.yaml (supports both array and object formats)
- Implement YAML schema validation for inputs.local.yaml
- Support flexible input definitions: inputs can be just IDs (strings) or objects with metadata fields
- Support extra fields in input definitions (md5, size, description)
- Handle relative and absolute paths in inputs.local.yaml (automatic conversion to absolute paths)
- Add comprehensive edge case tests for input parsing
- Add great error messages for invalid YAML structures
- Add yaml package as dependency

# pm 0.0.1

- Implement `PMProject` R6 class for project folder management with validation
- Add `pm_create_project()` and `pm_project()` functions for creating and loading projects
- Add project template structure with standard files and directories
- Establish GitHub Actions workflows for CI/CD (R-CMD-check, test coverage, version/NEWS validation)
- Add comprehensive test suite using testthat
- Add dependencies: R6, chk (imports); testthat, withr (suggests)
