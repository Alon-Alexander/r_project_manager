# Infer an analysis object based on the current directory

Find the relevant analysis object based on the current directory.
Currently supported being called from an analysis folder inside
"analyses" folder, or from the "code" folder of an analysis.

## Usage

``` r
pm_infer_analysis()
```

## Value

`PMAnalysis` object representing the inferred analysis

## Examples

``` r
empty_folder <- withr::local_tempdir()
pm <- pm_create_project(empty_folder)
pm$create_analysis("my_analysis")
#> PMAnalysis:
#>   Name: my_analysis
#>   Path: /private/var/folders/1y/16vztvbs6hx360mw9b8qdn280000gn/T/RtmpTMTVin/file53dd4f324467/analyses/my_analysis
#>   Project: /private/var/folders/1y/16vztvbs6hx360mw9b8qdn280000gn/T/RtmpTMTVin/file53dd4f324467

# Infer from analysis folder
withr::with_dir(file.path(empty_folder, "analyses", "my_analysis"), {
  analysis1 <- pm_infer_analysis()
  analysis1
})
#> PMAnalysis:
#>   Name: my_analysis
#>   Path: /private/var/folders/1y/16vztvbs6hx360mw9b8qdn280000gn/T/RtmpTMTVin/file53dd4f324467/analyses/my_analysis
#>   Project: /private/var/folders/1y/16vztvbs6hx360mw9b8qdn280000gn/T/RtmpTMTVin/file53dd4f324467

# Infer from code folder
withr::with_dir(file.path(empty_folder, "analyses", "my_analysis", "code"), {
  analysis2 <- pm_infer_analysis()
  analysis2
})
#> PMAnalysis:
#>   Name: my_analysis
#>   Path: /private/var/folders/1y/16vztvbs6hx360mw9b8qdn280000gn/T/RtmpTMTVin/file53dd4f324467/analyses/my_analysis
#>   Project: /private/var/folders/1y/16vztvbs6hx360mw9b8qdn280000gn/T/RtmpTMTVin/file53dd4f324467
```
