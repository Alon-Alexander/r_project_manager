# Extracted from test_parse_inputs.R:42

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "pm", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
it("Can parse valid inputs.yaml and inputs.local.yaml", {
    dir <- .get_good_project_path()

    # Create valid inputs.yaml
    writeLines(
      c(
        "inputs:",
        "  - feature_table",
        "  - sample_metadata:",
        "    description: Sample metadata",
        "    md5: abc123def456",
        "    size: 1024"
      ),
      file.path(dir, "inputs.yaml")
    )

    # Create valid inputs.local.yaml
    local_inputs_yaml <- list(
      paths = list(
        feature_table = "/path/to/feature_table.biom",
        sample_metadata = "/path/to/metadata.tsv"
      )
    )
    yaml::write_yaml(local_inputs_yaml, file.path(dir, "inputs.local.yaml"))

    # Parse inputs
    proj <- pm::PMProject$new(dir)
    data_list <- proj$parse_inputs()

    # Check results
    expect_type(data_list, "list")
    expect_length(data_list, 2)
    expect_true("feature_table" %in% names(data_list))
    expect_true("sample_metadata" %in% names(data_list))

    # Check PMData objects
    expect_s3_class(data_list$feature_table, "PMData")
    expect_s3_class(data_list$sample_metadata, "PMData")
    expect_equal(data_list$feature_table$id, "feature_table")
    expect_equal(data_list$feature_table$path, "/path/to/feature_table.biom")
    expect_equal(data_list$sample_metadata$id, "sample_metadata")
    expect_equal(data_list$sample_metadata$path, "/path/to/metadata.tsv")
  })
