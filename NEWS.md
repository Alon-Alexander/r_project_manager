# pm 0.1.3

## New Features

- Add `PMAnalysis` R6 class to manage analysis folders within projects
- Add `create_analysis()` method to `PMProject` for creating new analyses from template
- Add `list_analyses()` method to `PMProject` to get all analysis names
- Add `get_analysis()` method to `PMProject` to retrieve analysis objects by name
- Analysis template structure includes: README.md, code/, outputs/, intermediate/, logs/, and .gitignore
- Template-based analysis creation using `inst/extdata/template_analysis` directory
- Support for `dot_` prefix convention in templates (files starting with `dot_` are renamed to start with `.` when copied)
- `PMAnalysis` can be constructed from project object + name, or directly from path
- `PMAnalysis$new()` automatically validates the analysis structure on initialization

# pm 0.1.2

## Improvements

- Enhanced input file validation in `PMProject$validate()` method
- Improved error messages for missing input files with detailed information:
  - Shows both original path (from YAML) and resolved absolute path
  - Provides actionable guidance on how to fix issues
  - Includes examples for adding missing entries
  - Better handling of edge cases (empty inputs.local.yaml, missing paths key)
- Fixed bug where validation could fail when inputs.local.yaml is empty or missing paths key
- Added comprehensive test coverage for validation edge cases and error message formats

# pm 0.1.1

## New Features

- Add `pm_read_file()` function to read files based on their extension
- Support reading CSV, TSV, Parquet (`.parquet`, `.pqt`), RDS, and RData (`.rdata`, `.rda`) files
- Add `read()` method to `PMData` class for reading data files
- For RData files, objects are loaded into a new environment and returned
- Add `pm_write_file()` function to write files based on their extension
- Support writing CSV, TSV, Parquet (`.parquet`, `.pqt`), RDS, and RData (`.rdata`, `.rda`) files
- Add `write()` method to `PMData` class for writing data files
- For RData files, can write multiple objects using `...` (named or unnamed)
- Validate object types: tabular formats (CSV, TSV, Parquet) require data.frame objects
- Add comprehensive tests for file reading and writing functionality
- Add round-trip integration tests for read/write operations through PMData
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
