# Working with File Formats: Read and Write Operations

## Introduction

The `pm` package provides seamless read and write operations for various
file formats through the `PMData` class and its `read()` and
[`write()`](https://rdrr.io/r/base/write.html) methods. This vignette
demonstrates how to work with different file types using
`get_output_path()` and `get_artifact()`.

## Supported File Types

The package supports several file formats organized by type:

### Table Formats

- **CSV** (`.csv`) - Comma-separated values
- **TSV** (`.tsv`) - Tab-separated values  
- **Parquet** (`.parquet`, `.pqt`) - Apache Parquet format (requires
  `arrow` package)

### Object Formats

- **RDS** (`.rds`) - R serialized data files
- **RData** (`.rdata`, `.rda`) - R data files (can contain multiple
  objects)

### Image Formats

- **PNG, JPEG, SVG, GIF, TIFF, BMP** - Various image formats (for
  `type = "image"` or `type = "figure"`)

## Using get_output_path() with Type Specification

The `get_output_path()` method allows you to specify a type, and the
package will automatically choose an appropriate file extension:

``` r

# Create a temporary project for demonstration
project_dir <- file.path(tempdir(), "file_formats_demo")
pm <- pm_create_project(project_dir)
analysis <- pm$create_analysis("demo")
```

### Writing Tables

When you specify `type = "table"`, the default format is Parquet, but
you can also use CSV or TSV:

``` r

# Create sample data
sample_data <- data.frame(
  id = 1:10,
  value = rnorm(10),
  category = letters[1:10]
)

# Write as table (defaults to .parquet)
parquet_output <- analysis$get_output_path("table_parquet", type = "table")
parquet_output$write(sample_data)
cat("Written to:", basename(parquet_output$path), "\n")
#> Written to: table_parquet.parquet

# Write as CSV explicitly (no need for type when extension is provided)
csv_output <- analysis$get_output_path("table_csv.csv")
csv_output$write(sample_data)
cat("Written to:", basename(csv_output$path), "\n")
#> Written to: table_csv.csv

# Write as TSV (if both type and extension are given, they are validated to be matching)
tsv_output <- analysis$get_output_path("table_tsv.tsv", type = "table")
tsv_output$write(sample_data)
cat("Written to:", basename(tsv_output$path), "\n")
#> Written to: table_tsv.tsv
```

### Writing Objects

For R objects, use `type = "object"`:

``` r

# Create a complex object
my_model <- list(
  coefficients = c(intercept = 1.5, slope = 2.3),
  data = sample_data,
  metadata = list(created = Sys.Date(), version = "1.0")
)

# Write as RDS (single object)
rds_output <- analysis$get_output_path("model", type = "object")
rds_output$write(my_model)
cat("Written to:", basename(rds_output$path), "\n")
#> Written to: model.rdata

# Write as RData (can contain multiple objects)
rdata_output <- analysis$get_output_path("model_rdata.RData", type = "object")
rdata_output$write(my_model, sample_data, metadata = list(version = "1.0"))
cat("Written to:", basename(rdata_output$path), "\n")
#> Written to: model_rdata.RData
```

## Reading Files with get_artifact()

The `get_artifact()` method finds files by their ID (filename without
extension), and `read()` automatically detects the format:

``` r

# Read the Parquet table (automatically detects Parquet format)
parquet_data <- analysis$get_artifact("table_parquet")$read()
head(parquet_data)
#>   id        value category
#> 1  1 -1.400043517        a
#> 2  2  0.255317055        b
#> 3  3 -2.437263611        c
#> 4  4 -0.005571287        d
#> 5  5  0.621552721        e
#> 6  6  1.148411606        f

# Read the CSV (automatically detects CSV format)
csv_data <- analysis$get_artifact("table_csv")$read()
head(csv_data)
#>   id        value category
#> 1  1 -1.400043517        a
#> 2  2  0.255317055        b
#> 3  3 -2.437263611        c
#> 4  4 -0.005571287        d
#> 5  5  0.621552721        e
#> 6  6  1.148411606        f

# Read the TSV (automatically detects TSV format)
tsv_data <- analysis$get_artifact("table_tsv")$read()
head(tsv_data)
#>   id        value category
#> 1  1 -1.400043517        a
#> 2  2  0.255317055        b
#> 3  3 -2.437263611        c
#> 4  4 -0.005571287        d
#> 5  5  0.621552721        e
#> 6  6  1.148411606        f

# Read the RDS object (automatically detects RDS format)
model <- analysis$get_artifact("model")$read()
str(model, max.level = 2)
#> <environment: 0x10d9e0f28>

# Read RData (returns an environment)
rdata_env <- analysis$get_artifact("model_rdata")$read()
cat("Objects in RData file:", paste(names(rdata_env), collapse = ", "), "\n")
#> Objects in RData file: metadata, my_model, sample_data
```

## The Power of Type-Agnostic Reading

One of the key benefits is that you don’t need to know the file format
when reading:

``` r

# Get artifact by ID - no need to know if it's .csv, .parquet, or .tsv
artifact <- analysis$get_artifact("table_csv")

# Read it - format is automatically detected
data <- artifact$read()

# Works regardless of the actual file format!
cat("Successfully read", nrow(data), "rows\n")
#> Successfully read 10 rows

# Same works for Parquet or TSV - just change the ID
parquet_artifact <- analysis$get_artifact("table_parquet")
parquet_data <- parquet_artifact$read()
cat("Parquet file also read successfully:", nrow(parquet_data), "rows\n")
#> Parquet file also read successfully: 10 rows
```

This means: - **You can change file formats** (e.g., from CSV to
Parquet) without changing your reading code - **You don’t need to
remember file extensions** - just use the ID - **The package handles
format detection** automatically

## Working with Intermediate Files

You can also use `intermediate = TRUE` to save files in the intermediate
folder:

``` r

# Save intermediate result
intermediate_output <- analysis$get_output_path(
  "temp_calculation",
  type = "table",
  intermediate = TRUE
)
intermediate_output$write(sample_data)

cat("Intermediate file saved to:", basename(intermediate_output$path), "\n")
#> Intermediate file saved to: temp_calculation.parquet

# Note: get_artifact() only searches outputs/, not intermediate/
# To access intermediate files, use get_intermediate_artifact() or list_outputs(intermediate = TRUE)
intermediates <- analysis$list_outputs(intermediate = TRUE)
cat("Intermediate files:", paste(sapply(intermediates, function(x) x$id), collapse = ", "), "\n")
#> Intermediate files: temp_calculation
```

## Complete Workflow Example

Here’s a complete example showing the workflow:

``` r

# 1. Create output path with type specification
output <- analysis$get_output_path("analysis_results", type = "table")

# 2. Write data (format determined by extension/type)
processed_data <- data.frame(
  x = 1:5,
  y = 2:6,
  z = 3:7
)
output$write(processed_data)

# 3. Later, retrieve and read (no need to know format,
#    but can pass parameters for specific needs)
artifact <- analysis$get_artifact("analysis_results")
reloaded_data <- artifact$read(as_tibble=TRUE)

# 4. Verify it worked
identical(processed_data, as.data.frame(reloaded_data))
#> [1] TRUE
```

## Summary

- Use `get_output_path(id, type = "table")` to get paths with
  appropriate extensions
- Use `get_output_path(id, type = "object")` for R objects
- The [`write()`](https://rdrr.io/r/base/write.html) method
  automatically handles different formats
- The `read()` method automatically detects file formats
- You don’t need to know file extensions when using `get_artifact()` and
  `read()`
- This makes your code format-agnostic and easier to maintain

For more details on project workflows, see the [Getting
Started](https://alon-alexander.github.io/r_project_manager/articles/getting-started.md)
vignette.
