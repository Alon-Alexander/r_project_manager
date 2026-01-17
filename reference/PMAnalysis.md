# Analysis class to manage an analysis folder

This object can be used to manage an analysis folder within a project.
Each analysis contains code, outputs, intermediate results, and logs.

## Public fields

- `path`:

  Full path to the analysis's folder

- `name`:

  Name of the analysis (folder name within analyses/)

- `project_path`:

  Full path to the project's folder (if created from project)

## Methods

### Public methods

- [`PMAnalysis$new()`](#method-PMAnalysis-new)

- [`PMAnalysis$validate()`](#method-PMAnalysis-validate)

- [`PMAnalysis$print()`](#method-PMAnalysis-print)

- [`PMAnalysis$get_artifact()`](#method-PMAnalysis-get_artifact)

- [`PMAnalysis$get_intermediate_artifact()`](#method-PMAnalysis-get_intermediate_artifact)

- [`PMAnalysis$list_outputs()`](#method-PMAnalysis-list_outputs)

- [`PMAnalysis$get_output_path()`](#method-PMAnalysis-get_output_path)

- [`PMAnalysis$clone()`](#method-PMAnalysis-clone)

------------------------------------------------------------------------

### Method `new()`

Create a PMAnalysis object

#### Usage

    PMAnalysis$new(project = NULL, name = NULL, path = NULL)

#### Arguments

- `project`:

  PMProject object. If provided, creates analysis within that project.

- `name`:

  Character. Name of the analysis (folder name). Required if project is
  provided.

- `path`:

  Character. Full path to the analysis folder. Required if project is
  not provided.

------------------------------------------------------------------------

### Method `validate()`

Validate the analysis folder. Makes sure all expected files and folders
exist.

#### Usage

    PMAnalysis$validate()

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print method for PMAnalysis

#### Usage

    PMAnalysis$print()

------------------------------------------------------------------------

### Method `get_artifact()`

Get an artifact (output file) from another analysis by ID. Searches for
files with the given ID (filename without extension) in analysis output
directories.

#### Usage

    PMAnalysis$get_artifact(id, analysis_name = NULL)

#### Arguments

- `id`:

  Character. The artifact ID (filename without extension).

- `analysis_name`:

  Character. Optional name of the analysis to search in. If not
  provided, uses the current analysis's name. If explicitly set to
  `NULL`, searches all analyses and fails if not exactly one match is
  found.

#### Returns

A `PMData` object with the artifact's ID and path.

#### Examples

    folder <- withr::local_tempdir()
    pm <- pm_create_project(folder)
    analysis1 <- pm$create_analysis("data_preparation")
    analysis2 <- pm$create_analysis("modeling")

    # Create a test output file in analysis1
    output <- analysis1$get_output_path("results.csv", type = "table")
    write.csv(data.frame(x = 1:5), output$path)

    # Get artifact from analysis2 (which gets it from analysis1)
    artifact <- analysis2$get_artifact("results", analysis_name = "data_preparation")

    # Get artifact from current analysis (default behavior)
    artifact <- analysis1$get_artifact("results")

    # Get artifact without specifying analysis (if unique across all analyses)
    artifact <- analysis2$get_artifact("results", analysis_name = NULL)

------------------------------------------------------------------------

### Method `get_intermediate_artifact()`

Get an intermediate artifact from the current analysis's intermediate
folder. Searches for existing files with the given ID (filename without
extension) in the intermediate directory. If an existing file is found,
returns it. If no existing file is found, returns the path via
`get_output_path()` (the file may not exist yet).

#### Usage

    PMAnalysis$get_intermediate_artifact(id)

#### Arguments

- `id`:

  Character. The artifact ID (filename without extension).

#### Returns

A `PMData` object with the artifact's ID and path.

#### Examples

    folder <- withr::local_tempdir()
    pm <- pm_create_project(folder)
    analysis <- pm$create_analysis("data_preparation")

    # Get artifact from current analysis's intermediate folder
    # If file exists, returns it; otherwise returns path for new file
    artifact <- analysis$get_intermediate_artifact("temp_data")
    if (artifact$exists()) {
      data <- artifact$read()
    } else {
      # File doesn't exist yet, can write to it
      artifact$write(data.frame(x = 1:5))
    }

------------------------------------------------------------------------

### Method `list_outputs()`

List all output files in the analysis. Returns a list of PMData objects
for all files in the outputs or intermediate directory.

#### Usage

    PMAnalysis$list_outputs(intermediate = FALSE)

#### Arguments

- `intermediate`:

  Logical. If TRUE, lists files in intermediate/ folder; if FALSE, in
  outputs/ folder.

#### Returns

A list of `PMData` objects, one for each file found. Each object has:

- `id`: The file name without extension

- `path`: The full absolute path to the file

#### Examples

    folder <- withr::local_tempdir()
    pm <- pm_create_project(folder)
    analysis <- pm$create_analysis("my_analysis")

    # Create some output files
    output1 <- analysis$get_output_path("results.csv", type = "table")
    output2 <- analysis$get_output_path("plot.png", type = "figure")

    # List all outputs
    outputs <- analysis$list_outputs()
    length(outputs)  # Number of output files

    # List intermediate files
    intermediates <- analysis$list_outputs(intermediate = TRUE)

------------------------------------------------------------------------

### Method `get_output_path()`

Get output path for a file, returning a PMData object.

#### Usage

    PMAnalysis$get_output_path(name, type = NULL, intermediate = FALSE)

#### Arguments

- `name`:

  Character. Name of the output file (with or without extension).

- `type`:

  Character. Optional type of output (table, object, image, figure,
  parquet, csv). If provided and name has no extension, an appropriate
  extension will be added. If provided and name has an extension, the
  extension will be validated against the type.

- `intermediate`:

  Logical. If TRUE, file goes in intermediate/ folder; if FALSE, in
  outputs/ folder.

#### Returns

A `PMData` object with:

- `id`: The file name without extension

- `path`: The full absolute path to the output file

#### Examples

    folder <- withr::local_tempdir()
    pm <- pm_create_project(folder)
    analysis <- pm$create_analysis("my_analysis")

    # Get output path for a CSV file
    output <- analysis$get_output_path("results.csv", type = "table")
    output$id    # "results"
    output$path  # full path to results.csv in outputs/

    # Get intermediate path without extension (will add .parquet for table type)
    intermediate <- analysis$get_output_path("temp_data", type = "table", intermediate = TRUE)
    intermediate$id    # "temp_data"
    intermediate$path  # full path to temp_data.parquet in intermediate/

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    PMAnalysis$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# Create a project and analysis
folder <- withr::local_tempdir()
pm <- pm_create_project(folder)

# Create a new analysis
analysis <- pm$create_analysis("data_preparation")
analysis
#> PMAnalysis:
#>   Name: data_preparation
#>   Path: /tmp/Rtmp8iXsCo/file1be177141936/analyses/data_preparation
#>   Project: /tmp/Rtmp8iXsCo/file1be177141936

# Load an existing analysis from project
analysis <- pm$get_analysis("data_preparation")

# Load an existing analysis from path
analysis <- PMAnalysis$new(path = file.path(folder, "analyses", "data_preparation"))


## ------------------------------------------------
## Method `PMAnalysis$get_artifact`
## ------------------------------------------------

folder <- withr::local_tempdir()
pm <- pm_create_project(folder)
analysis1 <- pm$create_analysis("data_preparation")
analysis2 <- pm$create_analysis("modeling")

# Create a test output file in analysis1
output <- analysis1$get_output_path("results.csv", type = "table")
write.csv(data.frame(x = 1:5), output$path)

# Get artifact from analysis2 (which gets it from analysis1)
artifact <- analysis2$get_artifact("results", analysis_name = "data_preparation")

# Get artifact from current analysis (default behavior)
artifact <- analysis1$get_artifact("results")

# Get artifact without specifying analysis (if unique across all analyses)
artifact <- analysis2$get_artifact("results", analysis_name = NULL)

## ------------------------------------------------
## Method `PMAnalysis$get_intermediate_artifact`
## ------------------------------------------------

folder <- withr::local_tempdir()
pm <- pm_create_project(folder)
analysis <- pm$create_analysis("data_preparation")

# Get artifact from current analysis's intermediate folder
# If file exists, returns it; otherwise returns path for new file
artifact <- analysis$get_intermediate_artifact("temp_data")
if (artifact$exists()) {
  data <- artifact$read()
} else {
  # File doesn't exist yet, can write to it
  artifact$write(data.frame(x = 1:5))
}

## ------------------------------------------------
## Method `PMAnalysis$list_outputs`
## ------------------------------------------------

folder <- withr::local_tempdir()
pm <- pm_create_project(folder)
analysis <- pm$create_analysis("my_analysis")

# Create some output files
output1 <- analysis$get_output_path("results.csv", type = "table")
output2 <- analysis$get_output_path("plot.png", type = "figure")

# List all outputs
outputs <- analysis$list_outputs()
length(outputs)  # Number of output files
#> [1] 0

# List intermediate files
intermediates <- analysis$list_outputs(intermediate = TRUE)

## ------------------------------------------------
## Method `PMAnalysis$get_output_path`
## ------------------------------------------------

folder <- withr::local_tempdir()
pm <- pm_create_project(folder)
analysis <- pm$create_analysis("my_analysis")

# Get output path for a CSV file
output <- analysis$get_output_path("results.csv", type = "table")
output$id    # "results"
#> [1] "results"
output$path  # full path to results.csv in outputs/
#> [1] "/tmp/Rtmp8iXsCo/file1be13c1e4886/analyses/my_analysis/outputs/results.csv"

# Get intermediate path without extension (will add .parquet for table type)
intermediate <- analysis$get_output_path("temp_data", type = "table", intermediate = TRUE)
intermediate$id    # "temp_data"
#> [1] "temp_data"
intermediate$path  # full path to temp_data.parquet in intermediate/
#> [1] "/tmp/Rtmp8iXsCo/file1be13c1e4886/analyses/my_analysis/intermediate/temp_data.parquet"
```
