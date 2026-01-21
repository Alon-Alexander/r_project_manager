# Create a new project from template in the given folder

Creates a new clean project in the given folder and returns a PMProject
object. Also works on an empty folder, but fails if the folder contains
irrelevant content.

## Usage

``` r
pm_create_project(path)
```

## Arguments

- path:

  Path to the folder in which to create the project

## Value

`PMProject` object representing the newly created project

## Note

If the folder already exists and has contents, does not override it, but
instead returns the existing project from that folder.

## Examples

``` r
empty_folder <- withr::local_tempdir()
pm <- pm_create_project(empty_folder)
pm
#> PMProject:
#>   Path: /private/var/folders/0t/mvk3x4hx0pl31l5lcl11krcc0000gn/T/RtmpewLzrE/file1026772fc6293
#>   Analyses: 0
```
