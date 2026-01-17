# Project class to manage a project folder

This object can be used to manage a project folder. Use it to create
analyses, freeze input files, etc.

## Public fields

- `path`:

  Full path to the project's folder

## Methods

### Public methods

- [`PMProject$new()`](#method-PMProject-new)

- [`PMProject$print()`](#method-PMProject-print)

- [`PMProject$validate()`](#method-PMProject-validate)

- [`PMProject$parse_inputs()`](#method-PMProject-parse_inputs)

- [`PMProject$list_analyses()`](#method-PMProject-list_analyses)

- [`PMProject$get_analysis()`](#method-PMProject-get_analysis)

- [`PMProject$get_artifact()`](#method-PMProject-get_artifact)

- [`PMProject$create_analysis()`](#method-PMProject-create_analysis)

- [`PMProject$clone()`](#method-PMProject-clone)

------------------------------------------------------------------------

### Method `new()`

Create a PMProject object in an existing project's folder

#### Usage

    PMProject$new(path)

#### Arguments

- `path`:

  Path to the project

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print method for PMProject

#### Usage

    PMProject$print()

------------------------------------------------------------------------

### Method `validate()`

Validate the project folder. Makes sure all expected files and folder
exist and are valid. Also validates that all input files referenced in
inputs.local.yaml exist.

#### Usage

    PMProject$validate()

------------------------------------------------------------------------

### Method `parse_inputs()`

Parse inputs.yaml and inputs.local.yaml into PMData objects. Reads the
inputs definition file and the local paths file and combines them to
create a list of PMData objects.

#### Usage

    PMProject$parse_inputs()

#### Returns

A list of PMData objects, one for each input defined in inputs.yaml that
has a corresponding path in inputs.local.yaml

#### Examples

    folder <- withr::local_tempdir()
    pm <- pm_create_project(folder)

    # After configuring inputs.yaml and inputs.local.yaml:
    # data_list <- pm$parse_inputs()

------------------------------------------------------------------------

### Method `list_analyses()`

Get all analysis names from the project. Returns the names of all valid
analysis folders in the analyses directory.

#### Usage

    PMProject$list_analyses()

#### Returns

Character vector of analysis names

#### Examples

    folder <- withr::local_tempdir()
    pm <- pm_create_project(folder)
    invisible(pm$create_analysis("data_preparation"))
    invisible(pm$create_analysis("modeling"))
    pm$list_analyses()

------------------------------------------------------------------------

### Method `get_analysis()`

Get an analysis object by name. Returns a PMAnalysis object for the
specified analysis name.

#### Usage

    PMProject$get_analysis(name)

#### Arguments

- `name`:

  Character. Name of the analysis (folder name within analyses/).

#### Returns

`PMAnalysis` object for the specified analysis

#### Examples

    folder <- withr::local_tempdir()
    pm <- pm_create_project(folder)
    invisible(pm$create_analysis("data_preparation"))
    analysis <- pm$get_analysis("data_preparation")
    analysis

------------------------------------------------------------------------

### Method `get_artifact()`

Get an artifact (output file) from an analysis by ID. Searches for files
with the given ID (filename without extension) in analysis output
directories.

#### Usage

    PMProject$get_artifact(id, analysis_name = NULL)

#### Arguments

- `id`:

  Character. The artifact ID (filename without extension).

- `analysis_name`:

  Character. Optional name of the analysis to search in. If not
  provided, searches all analyses and fails if not exactly one match is
  found.

#### Returns

A `PMData` object with the artifact's ID and path.

#### Examples

    folder <- withr::local_tempdir()
    pm <- pm_create_project(folder)
    analysis <- pm$create_analysis("data_preparation")

    # Create a test output file
    output <- analysis$get_output_path("results.csv", type = "table")
    output$write(data.frame(x = 1:5))

    # Get artifact from specific analysis
    artifact <- pm$get_artifact("results", analysis_name = "data_preparation")

    # Get artifact without specifying analysis (if unique across all analyses)
    artifact <- pm$get_artifact("results")

------------------------------------------------------------------------

### Method `create_analysis()`

Create a new analysis from template. Creates a new analysis folder with
template structure including README.md, directories (code/, outputs/,
intermediate/, logs/), and .gitignore file.

#### Usage

    PMProject$create_analysis(name)

#### Arguments

- `name`:

  Character. Name of the analysis (will be the folder name).

#### Returns

`PMAnalysis` object representing the newly created analysis

#### Examples

    folder <- withr::local_tempdir()
    pm <- pm_create_project(folder)
    analysis <- pm$create_analysis("data_preparation")
    analysis

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    PMProject$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# Create a valid project to load
folder <- withr::local_tempdir()
invisible(pm_create_project(folder))

# Load the project
pm <- PMProject$new(folder)
pm
#> PMProject:
#>   Path: /tmp/Rtmp8iXsCo/file1be17d1663da
#>   Analyses: 0


## ------------------------------------------------
## Method `PMProject$parse_inputs`
## ------------------------------------------------

folder <- withr::local_tempdir()
pm <- pm_create_project(folder)

# After configuring inputs.yaml and inputs.local.yaml:
# data_list <- pm$parse_inputs()

## ------------------------------------------------
## Method `PMProject$list_analyses`
## ------------------------------------------------

folder <- withr::local_tempdir()
pm <- pm_create_project(folder)
invisible(pm$create_analysis("data_preparation"))
invisible(pm$create_analysis("modeling"))
pm$list_analyses()
#> [1] "data_preparation" "modeling"        

## ------------------------------------------------
## Method `PMProject$get_analysis`
## ------------------------------------------------

folder <- withr::local_tempdir()
pm <- pm_create_project(folder)
invisible(pm$create_analysis("data_preparation"))
analysis <- pm$get_analysis("data_preparation")
analysis
#> PMAnalysis:
#>   Name: data_preparation
#>   Path: /tmp/Rtmp8iXsCo/file1be121c7fb49/analyses/data_preparation
#>   Project: /tmp/Rtmp8iXsCo/file1be121c7fb49

## ------------------------------------------------
## Method `PMProject$get_artifact`
## ------------------------------------------------

folder <- withr::local_tempdir()
pm <- pm_create_project(folder)
analysis <- pm$create_analysis("data_preparation")

# Create a test output file
output <- analysis$get_output_path("results.csv", type = "table")
output$write(data.frame(x = 1:5))

# Get artifact from specific analysis
artifact <- pm$get_artifact("results", analysis_name = "data_preparation")

# Get artifact without specifying analysis (if unique across all analyses)
artifact <- pm$get_artifact("results")

## ------------------------------------------------
## Method `PMProject$create_analysis`
## ------------------------------------------------

folder <- withr::local_tempdir()
pm <- pm_create_project(folder)
analysis <- pm$create_analysis("data_preparation")
analysis
#> PMAnalysis:
#>   Name: data_preparation
#>   Path: /tmp/Rtmp8iXsCo/file1be12b91592c/analyses/data_preparation
#>   Project: /tmp/Rtmp8iXsCo/file1be12b91592c
```
