# Create a PMProject object for a given project folder

Create a PMProject object for a given project folder

## Usage

``` r
pm_project(path)
```

## Arguments

- path:

  Path to the project

## Value

`PMProject` object representing the given project folder

## Examples

``` r
# Create a valid project to load
folder <- withr::local_tempdir()
invisible(pm_create_project(folder))

# Load the project
pm <- pm_project(folder)
pm
#> PMProject:
#>   Path: /private/var/folders/0t/mvk3x4hx0pl31l5lcl11krcc0000gn/T/RtmpeDGQmV/filef9eebc584bd
#>   Analyses: 0
```
