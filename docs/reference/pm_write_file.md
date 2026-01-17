# Write an object to a file based on its extension

Writes an object to a file based on the file extension. Supports
multiple file formats including tabular data (CSV, TSV), serialized R
objects (RDS), and R data files (RData/RData).

## Usage

``` r
pm_write_file(file, x, ..., object_names = NULL)
```

## Arguments

- file:

  Character. Path to the file to write

- x:

  The object to write. For tabular formats (CSV, TSV, Parquet), must be
  a data.frame or similar tabular object. For RData files, can accept
  multiple objects via `...`.

- ...:

  Additional arguments passed to the underlying write function. For
  RData files, additional objects can be provided here.

- object_names:

  Character vector. Optional. **Internal parameter.** For RData files,
  explicitly specify the names for objects. If provided, must match the
  number of objects (x + objects in ...). If not provided, names are
  extracted from the call. This parameter is intended for use when
  forwarding a `...` call from a parent frame where object names need to
  be preserved. See `PMData$write()` for example usage.

## Value

Invisibly returns the file path.

## Details

Supported file formats:

- **CSV**: Comma-separated values files (`.csv`) - uses
  [`write.csv()`](https://rdrr.io/r/utils/write.table.html). Requires a
  data.frame.

- **TSV**: Tab-separated values files (`.tsv`) - uses
  [`write.table()`](https://rdrr.io/r/utils/write.table.html) with
  `sep = "\t"`. Requires a data.frame.

- **Parquet**: Apache Parquet files (`.parquet`, `.pqt`) - requires the
  `arrow` package. Requires a data.frame.

- **RDS**: R serialized data files (`.rds`) - uses
  [`saveRDS()`](https://rdrr.io/r/base/readRDS.html). Accepts any R
  object.

- **RData/RData/Rda**: R data files (`.rdata`, `.RData`, `.rda`,
  `.Rda`) - uses [`save()`](https://rdrr.io/r/base/save.html). Can
  accept multiple objects:
  `pm_write_file("file.RData", obj1, obj2, obj3)`.

For tabular formats (CSV, TSV, Parquet), the function validates that the
object is a data.frame or similar tabular structure.

The `object_names` parameter is an internal parameter used when
forwarding `...` calls from a parent frame. When a wrapper function
(like `PMData$write()`) needs to preserve object names from the original
call, it can capture the names and pass them via `object_names`. This
ensures that RData files contain objects with the correct names even
when called through wrapper functions. See the implementation of
`PMData$write()` for an example of how to use this parameter.

## Examples

``` r
# Example usage:
withr::with_tempdir({

# Write a data.frame to CSV
df <- data.frame(x = 1:5, y = letters[1:5])
pm_write_file("data.csv", df)

# Write a data.frame to TSV
pm_write_file("data.tsv", df)

# Write any object to RDS
my_list <- list(a = 1, b = 2)
pm_write_file("data.rds", my_list)

# Write multiple objects to RData
obj1 <- data.frame(x = 1:3)
obj2 <- c("a", "b", "c")
pm_write_file("data.RData", obj1, obj2, obj3 = 42)

})
```
