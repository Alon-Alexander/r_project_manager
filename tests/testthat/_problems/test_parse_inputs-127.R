# Extracted from test_parse_inputs.R:127

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
    expect_equal(data_list$feature_table$path, normalizePath("/path/to/feature_table.biom", mustWork = FALSE))
    expect_equal(data_list$sample_metadata$id, "sample_metadata")
    expect_equal(data_list$sample_metadata$path, normalizePath("/path/to/metadata.tsv", mustWork = FALSE))
  })
it("Handles relative paths in inputs.local.yaml", {
    dir <- .get_good_project_path()

    # Create valid inputs.yaml
    inputs_yaml <- list(
      inputs = list(
        test_input = list()
      )
    )
    yaml::write_yaml(inputs_yaml, file.path(dir, "inputs.yaml"))

    # Create a test file in the project directory
    test_file <- file.path(dir, "test_data.tsv")
    file.create(test_file)

    # Create inputs.local.yaml with relative path
    local_inputs_yaml <- list(
      paths = list(
        test_input = "test_data.tsv"
      )
    )
    yaml::write_yaml(local_inputs_yaml, file.path(dir, "inputs.local.yaml"))

    # Parse inputs
    proj <- pm::PMProject$new(dir)
    data_list <- proj$parse_inputs()

    # Check that relative path was converted to absolute
    expected_path <- normalizePath(file.path(dir, "test_data.tsv"), mustWork = FALSE)
    expect_equal(data_list$test_input$path, expected_path)
    expect_equal(basename(data_list$test_input$path), "test_data.tsv")
  })
it("Handles fingerprint fields in inputs.yaml", {
    dir <- .get_good_project_path()

    # Create inputs.yaml with fingerprint fields
    inputs_yaml <- list(
      inputs = list(
        test_input = list(
          md5 = "abc123def456",
          size = 1024,
          description = "Test input file"
        )
      )
    )
    yaml::write_yaml(inputs_yaml, file.path(dir, "inputs.yaml"))

    # Create inputs.local.yaml
    local_inputs_yaml <- list(
      paths = list(
        test_input = "/path/to/test.tsv"
      )
    )
    yaml::write_yaml(local_inputs_yaml, file.path(dir, "inputs.local.yaml"))

    # Should parse without errors
    proj <- pm::PMProject$new(dir)
    data_list <- proj$parse_inputs()
    expect_length(data_list, 1)
  })
it("Allows inputs with just an ID (empty object)", {
    dir <- .get_good_project_path()

    # Create inputs.yaml with empty object (just an ID)
    writeLines(c(
      "inputs:",
      "  - minimal_input"
    ), file.path(dir, "inputs.yaml"))

    # Create inputs.local.yaml
    local_inputs_yaml <- list(
      paths = list(
        minimal_input = "/path/to/minimal.tsv"
      )
    )
    yaml::write_yaml(local_inputs_yaml, file.path(dir, "inputs.local.yaml"))

    # Should parse without errors
    proj <- pm::PMProject$new(dir)
    data_list <- proj$parse_inputs()

    # Check results
    expect_length(data_list, 1)
    expect_true("minimal_input" %in% names(data_list))
    expect_s3_class(data_list$minimal_input, "PMData")
    expect_equal(data_list$minimal_input$id, "minimal_input")
    expect_equal(data_list$minimal_input$path, normalizePath("/path/to/minimal.tsv", mustWork = FALSE))
  })
