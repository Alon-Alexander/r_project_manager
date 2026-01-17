# Data input class

Represents a single input data file with its ID and path. This is a
minimal representation that will be extended later with reading/writing
capabilities.

## Public fields

- `id`:

  Character. The canonical identifier for this input (e.g.,
  "feature_table")

- `path`:

  Character. The absolute path to the input file

## Methods

### Public methods

- [`PMData$new()`](#method-PMData-new)

- [`PMData$print()`](#method-PMData-print)

- [`PMData$read()`](#method-PMData-read)

- [`PMData$exists()`](#method-PMData-exists)

- [`PMData$write()`](#method-PMData-write)

- [`PMData$clone()`](#method-PMData-clone)

------------------------------------------------------------------------

### Method `new()`

Create a PMData object

#### Usage

    PMData$new(id, path)

#### Arguments

- `id`:

  Character. The canonical identifier for this input

- `path`:

  Character. The absolute path to the input file

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print method for PMData

#### Usage

    PMData$print()

------------------------------------------------------------------------

### Method `read()`

Read the data file

#### Usage

    PMData$read(...)

#### Arguments

- `...`:

  Additional arguments passed to
  [`pm_read_file`](https://alon-alexander.github.io/r_project_manager/reference/pm_read_file.md)

#### Returns

The contents of the file. For RData files, returns a new environment
containing all objects from the file.

#### Examples

    \dontrun{
    data <- PMData$new(id = "feature_table", path = "/path/to/file.csv")
    contents <- data$read()
    }

------------------------------------------------------------------------

### Method [`exists()`](https://rdrr.io/r/base/exists.html)

Check if the data file exists

#### Usage

    PMData$exists()

#### Returns

Logical. `TRUE` if the file exists, `FALSE` otherwise.

#### Examples

    withr::with_tempdir({
      data <- PMData$new(id = "feature_table", path = "file.csv")
      data$exists()  # FALSE - file doesn't exist yet

      df <- data.frame(x = 1:5, y = letters[1:5])
      data$write(df)
      data$exists()  # TRUE - file now exists
    })

------------------------------------------------------------------------

### Method [`write()`](https://rdrr.io/r/base/write.html)

Write data to the file

#### Usage

    PMData$write(x, ...)

#### Arguments

- `x`:

  The object to write. For tabular formats (CSV, TSV, Parquet), must be
  a data.frame. For RData files, can accept multiple objects via `...`.

- `...`:

  Additional arguments passed to
  [`pm_write_file`](https://alon-alexander.github.io/r_project_manager/reference/pm_write_file.md).
  For RData files, additional objects can be provided here.

#### Returns

Invisibly returns the file path.

#### Examples

    withr::with_tempdir({
      data <- PMData$new(id = "feature_table", path = "file.csv")
      df <- data.frame(x = 1:5, y = letters[1:5])
      data$write(df)

      # For RData files, can write multiple objects
      data_rdata <- PMData$new(id = "results", path = "results.RData")
      obj1 <- data.frame(x = 1:3)
      obj2 <- c("a", "b", "c")
      data_rdata$write(obj1, obj2, obj3 = 42)
    })

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    PMData$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
data <- PMData$new(id = "feature_table", path = "/path/to/file.biom")
data$id
#> [1] "feature_table"
data$path
#> [1] "/path/to/file.biom"


## ------------------------------------------------
## Method `PMData$read`
## ------------------------------------------------

if (FALSE) { # \dontrun{
data <- PMData$new(id = "feature_table", path = "/path/to/file.csv")
contents <- data$read()
} # }

## ------------------------------------------------
## Method `PMData$exists`
## ------------------------------------------------

withr::with_tempdir({
  data <- PMData$new(id = "feature_table", path = "file.csv")
  data$exists()  # FALSE - file doesn't exist yet

  df <- data.frame(x = 1:5, y = letters[1:5])
  data$write(df)
  data$exists()  # TRUE - file now exists
})
#> [1] TRUE

## ------------------------------------------------
## Method `PMData$write`
## ------------------------------------------------

withr::with_tempdir({
  data <- PMData$new(id = "feature_table", path = "file.csv")
  df <- data.frame(x = 1:5, y = letters[1:5])
  data$write(df)

  # For RData files, can write multiple objects
  data_rdata <- PMData$new(id = "results", path = "results.RData")
  obj1 <- data.frame(x = 1:3)
  obj2 <- c("a", "b", "c")
  data_rdata$write(obj1, obj2, obj3 = 42)
})
```
