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
#>   Path: /private/var/folders/1y/16vztvbs6hx360mw9b8qdn280000gn/T/RtmpuRdKh6/file81fa4d8ec5ab
#>   Analyses: 0
```
