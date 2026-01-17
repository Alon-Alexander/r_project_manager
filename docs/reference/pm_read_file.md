# Read a file based on its extension

Reads a file based on its file extension. Supports multiple file formats
including tabular data (CSV, TSV), serialized R objects (RDS), and R
data files (RData/RData).

## Usage

``` r
pm_read_file(file, ...)
```

## Arguments

- file:

  Character. Path to the file to read

- ...:

  Additional arguments passed to the underlying read function (e.g.,
  `header`, `sep`, `stringsAsFactors` for CSV/TSV files)

## Value

The contents of the file. For RData files, returns a new environment
containing all objects from the file.

## Details

Supported file formats:

- **CSV**: Comma-separated values files (`.csv`) - uses
  [`read.csv()`](https://rdrr.io/r/utils/read.table.html)

- **TSV**: Tab-separated values files (`.tsv`) - uses
  [`read.table()`](https://rdrr.io/r/utils/read.table.html) with
  `sep = "\t"`

- **Parquet**: Apache Parquet files (`.parquet`, `.pqt`) - requires the
  `arrow` package

- **RDS**: R serialized data files (`.rds`) - uses
  [`readRDS()`](https://rdrr.io/r/base/readRDS.html)

- **RData/RData/Rda**: R data files (`.rdata`, `.RData`, `.rda`,
  `.Rda`) - uses [`load()`](https://rdrr.io/r/base/load.html) into a new
  environment

For RData files, all objects are loaded into a new environment which is
returned. You can access objects using `env$object_name` or
`as.list(env)`.

## Examples

``` r
if (FALSE) { # \dontrun{
# Read a CSV file
data <- pm_read_file("data.csv")

# Read a TSV file
data <- pm_read_file("data.tsv")

# Read an RDS file
obj <- pm_read_file("data.rds")

# Read an RData file (returns environment)
env <- pm_read_file("data.RData")
my_object <- env$my_object
} # }
```
