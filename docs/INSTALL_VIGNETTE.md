# Installing the Package with Vignettes

To view the vignette, you need to install the package with vignettes
built.

## Option 1: Using devtools (Recommended)

``` r

# Install devtools if you don't have it
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

# Install the package with vignettes
devtools::install(".", build_vignettes = TRUE)

# View the vignette
vignette("getting-started", package = "pm")
```

## Option 2: Using R CMD INSTALL

From the terminal:

``` bash
cd /Users/alon/Projects/r_project_manager
R CMD INSTALL --build-vignettes .
```

Then in R:

``` r

vignette("getting-started", package = "pm")
```

## Option 3: Build vignettes separately, then install

``` r

# Build vignettes first
tools::buildVignettes(dir = ".", tangle = FALSE)

# Then install (vignettes will be included)
devtools::install(".")
```

## Troubleshooting

If you get an error about Pandoc: - Install Pandoc:
`brew install pandoc` (on macOS) - Or install RStudio which includes
Pandoc

If you get “No docs found”: - Make sure the package is installed:
`"pm" %in% rownames(installed.packages())` - Make sure vignettes were
built during installation - Check that the vignette HTML exists:
`file.exists(system.file("doc", "getting-started.html", package = "pm"))`
