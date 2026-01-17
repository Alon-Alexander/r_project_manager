# Getting Started with pm: A Complete Workflow Example

## Introduction

The `pm` package provides a structured approach to managing research
projects in R. This vignette demonstrates a complete workflow:

1.  Creating and configuring a new project
2.  Creating and preprocessing data
3.  Saving intermediate results
4.  Loading data from another analysis
5.  Using the artifact discovery features

## Creating a Project

First, let’s create a new project. We’ll use a temporary directory for
this example:

``` r
# Create a temporary directory for our project
project_dir <- file.path(tempdir(), "my_research_project")

# Create the project structure
pm <- pm_create_project(project_dir)
pm
#> PMProject:
#>   Path: /tmp/RtmpwUa9FA/my_research_project
#>   Analyses: 0
```

The project has been created with the standard structure:

- `inputs.yaml` - portable input definitions
- `inputs.local.yaml` - local path mappings (machine-specific)
- `analyses/` - directory for all analyses
- `README.md` - project documentation

## Creating Data

For this example, we’ll create some sample data that simulates a
research dataset. In a real scenario, this might come from external
sources or experiments.

``` r
# Create a temporary location for our "raw" data
data_dir <- file.path(tempdir(), "raw_data")
dir.create(data_dir, showWarnings = FALSE)

# Generate sample data: a simple dataset with measurements
set.seed(42)
raw_data <- data.frame(
  sample_id = paste0("S", sprintf("%03d", 1:100)),
  treatment = rep(c("Control", "Treatment"), each = 50),
  measurement_1 = rnorm(100, mean = 10, sd = 2),
  measurement_2 = rnorm(100, mean = 15, sd = 3),
  measurement_3 = rnorm(100, mean = 20, sd = 4)
)

# Save the raw data
raw_data_path <- file.path(data_dir, "raw_measurements.csv")
write.csv(raw_data, raw_data_path, row.names = FALSE)

cat("Created raw data file:", raw_data_path, "\n")
#> Created raw data file: /tmp/RtmpwUa9FA/raw_data/raw_measurements.csv
cat("Data dimensions:", nrow(raw_data), "rows,", ncol(raw_data), "columns\n")
#> Data dimensions: 100 rows, 5 columns
```

## Setting Up Project Inputs

Now we need to configure the project to recognize our raw data file.
We’ll edit the input configuration files directly in a text editor.

**Note**: For more information on the different ways to define
`inputs.yaml`, see the [Input
Definitions](https://alon-alexander.github.io/r_project_manager/articles/input-definitions.md)
vignette.

### Editing `inputs.yaml`

Open `inputs.yaml` in your project directory and add the input
definition. The file should look like this:

``` yaml
inputs:
  - raw_measurements
```

This file defines the *portable* input definitions that can be shared
across different machines. It describes what the input is, but not where
it’s located.

NOTE: This is the simplest format of `inputs.yaml`, but you can also for
example provide a description for each input.

### Editing `inputs.local.yaml`

Open `inputs.local.yaml` in your project directory and add the path
mapping. The file should look like this (with your actual file path):

``` yaml
paths:
  raw_measurements: "/tmp/RtmpXXXXXX/raw_data/raw_measurements.csv"
```

**Note**: Replace the path above with the actual path to your data file.
In this example, we’re using a temporary path, but in practice you would
use the real path to your data file.

This file maps the input ID (`raw_measurements`) to the actual file path
on your machine. This file is typically gitignored since paths differ
between machines.

### Verifying the Configuration

After editing both files, we can verify the configuration works:

``` r
# Reload the project to pick up the changes
pm <- pm_project(project_dir)
pm
#> PMProject:
#>   Path: /tmp/RtmpwUa9FA/my_research_project
#>   Analyses: 0
#>   Inputs: 1

# Parse inputs to verify they're configured correctly
inputs <- pm$parse_inputs()
raw_data_input <- inputs$raw_measurements

# Display the input object
raw_data_input
#> PMData:
#>   ID: raw_measurements
#>   Path: /tmp/RtmpwUa9FA/raw_data/raw_measurements.csv
```

## Analysis 1: Data Preprocessing

Let’s create our first analysis for data preprocessing. This analysis
will:

- Load the raw data
- Perform some transformations
- Save intermediate results
- Create final preprocessed outputs

``` r
# Create the preprocessing analysis
prep_analysis <- pm$create_analysis("data_preprocessing")
prep_analysis
#> PMAnalysis:
#>   Name: data_preprocessing
#>   Path: /tmp/RtmpwUa9FA/my_research_project/analyses/data_preprocessing
#>   Project: /tmp/RtmpwUa9FA/my_research_project
```

### Loading Input Data

We can load the input data using the project’s `parse_inputs()` method:

``` r
# Parse and load inputs
inputs <- pm$parse_inputs()
raw_data_input <- inputs$raw_measurements

# Display the input object
raw_data_input
#> PMData:
#>   ID: raw_measurements
#>   Path: /tmp/RtmpwUa9FA/raw_data/raw_measurements.csv

# Read the data (as simple as that!)
raw_data_loaded <- raw_data_input$read()
head(raw_data_loaded)
#>   sample_id treatment measurement_1 measurement_2 measurement_3
#> 1      S001   Control     12.741917      18.60290      11.99628
#> 2      S002   Control      8.870604      18.13425      21.33511
#> 3      S003   Control     10.726257      11.99037      24.68530
#> 4      S004   Control     11.265725      20.54545      28.23816
#> 5      S005   Control     10.808537      12.99968      14.49255
#> 6      S006   Control      9.787751      15.31654      15.39658
```

### Data Preprocessing Steps

Now let’s perform some preprocessing. We’ll:

1.  Calculate summary statistics (save as intermediate)
2.  Normalize measurements (save as intermediate)
3.  Create a final cleaned dataset (save as output)

#### Step 1: Calculate summary statistics

``` r
summary_stats <- data.frame(
  variable = c("measurement_1", "measurement_2", "measurement_3"),
  mean = c(
    mean(raw_data_loaded$measurement_1),
    mean(raw_data_loaded$measurement_2),
    mean(raw_data_loaded$measurement_3)
  ),
  sd = c(
    sd(raw_data_loaded$measurement_1),
    sd(raw_data_loaded$measurement_2),
    sd(raw_data_loaded$measurement_3)
  )
)

# Save summary stats as an intermediate result
summary_stats_path <- prep_analysis$get_output_path(
  "summary_stats",
  type = "table",
  intermediate = TRUE
)
summary_stats_path$write(summary_stats)
cat("Saved intermediate: summary_stats\n")
#> Saved intermediate: summary_stats
```

#### Step 2: Normalize measurements (z-scores)

``` r
normalized_data <- raw_data_loaded
normalized_data$measurement_1_norm <- scale(normalized_data$measurement_1)[, 1]
normalized_data$measurement_2_norm <- scale(normalized_data$measurement_2)[, 1]
normalized_data$measurement_3_norm <- scale(normalized_data$measurement_3)[, 1]

# Save normalized data as an intermediate result
prep_analysis$get_output_path(
  "normalized_data",
  type = "table",
  intermediate = TRUE
)$write(normalized_data)
cat("Saved intermediate: normalized_data\n")
#> Saved intermediate: normalized_data
```

#### Step 3: Create final cleaned dataset (remove outliers, add derived columns)

``` r
cleaned_data <- normalized_data
cleaned_data$total_measurement <- 
  cleaned_data$measurement_1 + 
  cleaned_data$measurement_2 + 
  cleaned_data$measurement_3

# Remove outliers (beyond 3 standard deviations)
cleaned_data <- cleaned_data[
  abs(cleaned_data$measurement_1_norm) < 3 &
  abs(cleaned_data$measurement_2_norm) < 3 &
  abs(cleaned_data$measurement_3_norm) < 3,
]

# Save as final output
cleaned_data_path <- prep_analysis$get_output_path(
  "cleaned_data",
  type = "table"
)
cleaned_data_path$write(cleaned_data)
cat("Saved output: cleaned_data\n")
#> Saved output: cleaned_data
cat("Final dataset:", nrow(cleaned_data), "rows\n")
#> Final dataset: 99 rows
```

### Listing Outputs

We can list all outputs from this analysis:

``` r
# List final outputs
outputs <- prep_analysis$list_outputs()
cat("Final outputs:\n")
#> Final outputs:
for (output in outputs) {
  cat("  -", output$id, ":", basename(output$path), "\n")
}
#>   - cleaned_data : cleaned_data.parquet

# List intermediate outputs
intermediates <- prep_analysis$list_outputs(intermediate = TRUE)
cat("\nIntermediate outputs:\n")
#> 
#> Intermediate outputs:
for (intermediate in intermediates) {
  cat("  -", intermediate$id, ":", basename(intermediate$path), "\n")
}
#>   - normalized_data : normalized_data.parquet 
#>   - summary_stats : summary_stats.parquet
```

## Analysis 2: Statistical Analysis

Now let’s create a second analysis that uses the preprocessed data from
the first analysis. This demonstrates how analyses can depend on each
other.

``` r
# Create the statistical analysis
stats_analysis <- pm$create_analysis("statistical_analysis")
stats_analysis
#> PMAnalysis:
#>   Name: statistical_analysis
#>   Path: /tmp/RtmpwUa9FA/my_research_project/analyses/statistical_analysis
#>   Project: /tmp/RtmpwUa9FA/my_research_project
```

### Using get_artifact to Find Data

The `get_artifact()` method allows us to find outputs from other
analyses without needing to know the exact file path or format. This is
one of the key features of the package.

#### Method 1: Searching With Explicit Analysis Name

If we know which analysis produced the artifact, we can specify it:

``` r
# Get the cleaned data from the preprocessing analysis
cleaned_artifact <- stats_analysis$get_artifact(
  "cleaned_data",
  analysis_name = "data_preprocessing"
)

cleaned_artifact
#> PMData:
#>   ID: cleaned_data
#>   Path: /tmp/RtmpwUa9FA/my_research_project/analyses/data_preprocessing/outputs/cleaned_data.parquet
```

#### Method 2: Search All Analyses (When Unique)

If the artifact ID is unique across all analyses (very recommended
anyway!), we can search all analyses by explicitly passing `NULL`:

``` r
# Since "cleaned_data" only exists in one analysis, we can search automatically
# Note: must explicitly pass NULL to search all analyses; default is current analysis
cleaned_artifact_auto <- stats_analysis$get_artifact("cleaned_data", analysis_name = NULL)

# Both methods return the same artifact
identical(cleaned_artifact$path, cleaned_artifact_auto$path)
#> [1] TRUE
```

#### Method 3: Get from Current Analysis (Default)

By default, `get_artifact()` searches in the current analysis. This is
useful when you want to get an artifact from the same analysis:

``` r
# Get artifact from current analysis (default behavior)
# This would look for "cleaned_data" in "statistical_analysis"
# (In this example, it won't find it since it's in "data_preprocessing")
```

#### Using `get_artifact()` on the Project Object

You can also call `get_artifact()` directly on the project object
(`pm`). The result is identical to calling it on an analysis object:

``` r
# Use get_artifact() on the project object to find "cleaned_data"
cleaned_artifact_from_project <- pm$get_artifact(
  "cleaned_data",
  analysis_name = "data_preprocessing"
)

# Check that the artifact is the same as before
identical(cleaned_artifact$path, cleaned_artifact_from_project$path)
#> [1] TRUE
```

This is convenient if you want to access artifacts globally, without
referencing a specific analysis instance.

### Using the read() Method

The `read()` method automatically detects the file format and reads it
appropriately. You don’t need to know whether it’s a CSV, Parquet, RDS,
or any other supported format.

**Note**: For detailed information on supported file formats and type
specifications, see the [File
Formats](https://alon-alexander.github.io/r_project_manager/articles/file-formats.md)
vignette.

``` r
# Read the artifact - no need to know the file format!
cleaned_data_loaded <- cleaned_artifact$read()

# The data is automatically loaded in the correct format
head(cleaned_data_loaded)
#>   sample_id treatment measurement_1 measurement_2 measurement_3
#> 1      S001   Control     12.741917      18.60290      11.99628
#> 2      S002   Control      8.870604      18.13425      21.33511
#> 3      S003   Control     10.726257      11.99037      24.68530
#> 4      S004   Control     11.265725      20.54545      28.23816
#> 5      S005   Control     10.808537      12.99968      14.49255
#> 6      S006   Control      9.787751      15.31654      15.39658
#>   measurement_1_norm measurement_2_norm measurement_3_norm total_measurement
#> 1          1.2852880          1.4250020         -1.9572635          43.34110
#> 2         -0.5734950          1.2522317          0.3383886          48.33997
#> 3          0.3174834         -1.0127756          1.1619263          47.40193
#> 4          0.5765053          2.1411438          2.0352826          60.04933
#> 5          0.3569895         -0.6406842         -1.3436351          38.30077
#> 6         -0.1331333          0.2134519         -1.1214097          40.50087
cat("Data loaded successfully with", nrow(cleaned_data_loaded), "rows\n")
#> Data loaded successfully with 99 rows
```

This is particularly powerful because:

- You don’t need to remember file paths
- You don’t need to know the file format
- You don’t need to retype/google search how to read different file
  types every time
- If the format changes (e.g., from CSV to Parquet), your code still
  works

### Performing Statistical Analysis

Now let’s perform some statistical analysis on the cleaned data:

``` r
# Perform t-test comparing treatments
control_data <- cleaned_data_loaded[
  cleaned_data_loaded$treatment == "Control",
  "total_measurement"
]
treatment_data <- cleaned_data_loaded[
  cleaned_data_loaded$treatment == "Treatment",
  "total_measurement"
]

t_test_result <- t.test(control_data, treatment_data)

# Create results summary
results_summary <- data.frame(
  test = "t-test: Control vs Treatment",
  statistic = t_test_result$statistic,
  p_value = t_test_result$p.value,
  control_mean = mean(control_data),
  treatment_mean = mean(treatment_data),
  control_sd = sd(control_data),
  treatment_sd = sd(treatment_data)
)

# Save results
stats_analysis$get_output_path("statistical_results.csv")$write(results_summary)

print(results_summary)
#>                           test  statistic   p_value control_mean treatment_mean
#> t t-test: Control vs Treatment -0.6619564 0.5095986     44.31905       45.01555
#>   control_sd treatment_sd
#> t   5.530649     4.913533
```

### Understanding Intermediate vs Output Files

It’s important to understand the distinction between intermediate and
output files:

- **Intermediate files** (`intermediate/` folder): Temporary results
  used within an analysis, typically for caching expensive computations.
  These can be accessed from the current analysis using
  `get_artifact(id, intermediate = TRUE)`, but are **not** searchable
  from other analyses.
- **Output files** (`outputs/` folder): Final results meant to be shared
  across analyses. These **are** findable.

Let’s see what’s available in each:

``` r
# List what's in outputs/ (searchable via get_artifact)
prep_outputs <- prep_analysis$list_outputs(intermediate = FALSE)
for (out in prep_outputs) {
  cat("  -", out$id, ":", basename(out$path), "\n")
}
#>   - cleaned_data : cleaned_data.parquet

# List what's in intermediate/ (NOT searchable via get_artifact)
prep_intermediates <- prep_analysis$list_outputs(intermediate = TRUE)
for (int in prep_intermediates) {
  cat("  -", int$id, ":", basename(int$path), "\n")
}
#>   - normalized_data : normalized_data.parquet 
#>   - summary_stats : summary_stats.parquet
```

#### How to Use Intermediate Results: Example

Intermediate results are useful for caching expensive or time-consuming
steps, so you don’t need to recompute them every time. You can check if
an intermediate output already exists, and if so, load it; otherwise,
compute it and save for next time. Here’s how:

``` r
# Define the ID for your intermediate result
intermediate_id <- "filtered_data"

# Get the artifact (will find existing file or return path for new file)
filtered_artifact <- prep_analysis$get_artifact(intermediate_id, intermediate = TRUE)

# If the intermediate result exists, load it; otherwise, compute and save it
if (filtered_artifact$exists()) {
  # Load cached version
  filtered_data <- filtered_artifact$read()
} else {
  # Compute your intermediate result here
  filtered_data <- subset(cleaned_data_loaded, total_measurement > 15)

  # Save as intermediate result
  filtered_artifact$write(filtered_data)
}
```

By using this pattern, you can make your analyses faster and more robust
to changes, by reusing cached results when available.

## Working with Multiple Artifacts

Let’s create another output in the preprocessing analysis and
demonstrate how `get_artifact` handles multiple artifacts:

``` r
# Create a correlation matrix as another output
correlation_matrix <- cor(cleaned_data_loaded[, 
  c("measurement_1", "measurement_2", "measurement_3")
])

# Save as RDS (object format)
prep_analysis$get_output_path("correlation_matrix", type = "object")$write(correlation_matrix)

cat("Saved correlation matrix\n")
#> Saved correlation matrix

# Now we have multiple outputs in the preprocessing analysis
all_outputs <- prep_analysis$list_outputs(intermediate = FALSE)
cat("\nAll outputs in preprocessing analysis:\n")
#> 
#> All outputs in preprocessing analysis:
for (out in all_outputs) {
  cat("  -", out$id, ":", basename(out$path), "\n")
}
#>   - cleaned_data : cleaned_data.parquet 
#>   - correlation_matrix : correlation_matrix.rdata
```

### Reading Different File Formats

The `read()` method works seamlessly with different file formats:

``` r
# Read the CSV file (cleaned_data)
cleaned <- stats_analysis$get_artifact("cleaned_data", analysis_name = NULL)$read()
cat("Cleaned data type:", class(cleaned), "\n")
#> Cleaned data type: tbl_df tbl data.frame

# Read the RDS file (correlation_matrix)
corr_matrix <- stats_analysis$get_artifact("correlation_matrix", analysis_name = NULL)$read()
cat("Correlation matrix type:", class(corr_matrix), "\n")
#> Correlation matrix type: environment
print(corr_matrix)
#> <environment: 0x55b92ee55a80>
```

## Summary

This vignette demonstrated:

1.  **Project Creation**: Using
    [`pm_create_project()`](https://alon-alexander.github.io/r_project_manager/reference/pm_create_project.md)
    to set up a structured project
2.  **Input Configuration**: Setting up inputs using the two-layer
    system (portable + local)
3.  **Analysis Creation**: Creating multiple analyses with
    `create_analysis()`
4.  **Intermediate Results**: Saving intermediate results using
    `intermediate = TRUE`
5.  **Artifact Discovery**: Using `get_artifact()` to find outputs from
    other analyses
    - With explicit analysis name
    - With automatic search when unique
6.  **Automatic Reading**: Using `$read()` to automatically load files
    without knowing their format
7.  **Workflow Chaining**: Creating analyses that depend on outputs from
    previous analyses

The key benefits of this approach:

- **No hardcoded paths**: Artifacts are discovered by ID, not file paths
- **Format-agnostic**: `read()` handles different file formats
  automatically
- **Organized structure**: Clear separation between intermediate and
  final outputs
- **Reproducible**: All inputs and outputs are tracked and discoverable
- **Flexible**: Easy to add new analyses that build on previous work

## Next Steps

- Explore the `PMProject` and `PMAnalysis` classes for more methods
- Learn about input validation and schema checking
- Set up your own project structure for your research workflow
- Use the intermediate folder for caching expensive computations

## Related Vignettes

For more detailed information on specific topics:

- **[Working with File
  Formats](https://alon-alexander.github.io/r_project_manager/articles/file-formats.md)**:
  Learn about the different file formats supported by `pm`, how to use
  `get_output_path()` with type specifications, and the power of
  format-agnostic reading with `get_artifact()` and `read()`.

- **[Defining Project
  Inputs](https://alon-alexander.github.io/r_project_manager/articles/input-definitions.md)**:
  Explore the different ways you can define `inputs.yaml`, from simple
  arrays to detailed object definitions with metadata.
