# Infer an analysis object based on the calling script

Find the relevant analysis object based on the call stack of the script
that invoked this function. Supported when called from a script in an
analysis folder inside the "analyses" folder, or from a script in the
code folder of an analysis (including nested subfolders). Falls back to
the `Rscript` entry file from command-line arguments, then to the
current working directory when the caller file cannot be determined
otherwise.

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
#>   Path: /private/var/folders/0t/mvk3x4hx0pl31l5lcl11krcc0000gn/T/RtmpER8NpN/file83ed55add0e2/analyses/my_analysis
#>   Project: /private/var/folders/0t/mvk3x4hx0pl31l5lcl11krcc0000gn/T/RtmpER8NpN/file83ed55add0e2

# Infer from analysis folder
withr::with_dir(file.path(empty_folder, "analyses", "my_analysis"), {
  analysis1 <- pm_infer_analysis()
  analysis1
})
#> PMAnalysis:
#>   Name: my_analysis
#>   Path: /private/var/folders/0t/mvk3x4hx0pl31l5lcl11krcc0000gn/T/RtmpER8NpN/file83ed55add0e2/analyses/my_analysis
#>   Project: /private/var/folders/0t/mvk3x4hx0pl31l5lcl11krcc0000gn/T/RtmpER8NpN/file83ed55add0e2

# Infer from code folder
withr::with_dir(file.path(empty_folder, "analyses", "my_analysis", "code"), {
  analysis2 <- pm_infer_analysis()
  analysis2
})
#> PMAnalysis:
#>   Name: my_analysis
#>   Path: /private/var/folders/0t/mvk3x4hx0pl31l5lcl11krcc0000gn/T/RtmpER8NpN/file83ed55add0e2/analyses/my_analysis
#>   Project: /private/var/folders/0t/mvk3x4hx0pl31l5lcl11krcc0000gn/T/RtmpER8NpN/file83ed55add0e2
```
