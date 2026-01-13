describe("parse_inputs method works correctly", {
  it("Can parse valid inputs.yaml and inputs.local.yaml", {
    dir <- .get_good_project_path()

    # Create valid inputs.yaml
    inputs_yaml <- list(
      `inputs.feature_table` = list(
        description = "OTU table in BIOM format"
      ),
      `inputs.sample_metadata` = list(
        description = "Sample metadata",
        md5 = "abc123def456",
        size = 1024
      )
    )
    yaml::write_yaml(inputs_yaml, file.path(dir, "inputs.yaml"))

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

  it("Handles relative paths in inputs.local.yaml", {
    dir <- .get_good_project_path()

    # Create valid inputs.yaml
    inputs_yaml <- list(
      `inputs.test_input` = list(
        type = "tsv"
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
      `inputs.test_input` = list(
        type = "tsv",
        md5 = "abc123def456",
        size = 1024,
        description = "Test input file"
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

  it("Errors when input ID is missing from inputs.local.yaml", {
    dir <- .get_good_project_path()

    # Create inputs.yaml with two inputs
    inputs_yaml <- list(
      `inputs.input1` = list(type = "tsv"),
      `inputs.input2` = list(type = "tsv")
    )
    yaml::write_yaml(inputs_yaml, file.path(dir, "inputs.yaml"))

    # Create inputs.local.yaml with only one input
    local_inputs_yaml <- list(
      paths = list(
        input1 = "/path/to/input1.tsv"
      )
    )
    yaml::write_yaml(local_inputs_yaml, file.path(dir, "inputs.local.yaml"))

    # Should error
    proj <- pm::PMProject$new(dir)
    expect_error(
      proj$parse_inputs(),
      regexp = "is defined in inputs.yaml but missing from inputs.local.yaml paths"
    )
  })

  it("Errors when inputs.yaml has invalid YAML syntax", {
    dir <- .get_good_project_path()

    # Create invalid inputs.yaml (malformed YAML)
    writeLines("invalid: yaml: content: bad", file.path(dir, "inputs.yaml"))

    # Create valid inputs.local.yaml
    local_inputs_yaml <- list(
      paths = list(
        test_input = "/path/to/test.tsv"
      )
    )
    yaml::write_yaml(local_inputs_yaml, file.path(dir, "inputs.local.yaml"))

    proj <- pm::PMProject$new(dir)
    expect_error(
      proj$parse_inputs(),
      regexp = "must be a YAML object"
    )
  })

  it("Errors when inputs.yaml has no inputs.* keys", {
    dir <- .get_good_project_path()

    # Create inputs.yaml without inputs.* keys
    inputs_yaml <- list(
      other_key = "value"
    )
    yaml::write_yaml(inputs_yaml, file.path(dir, "inputs.yaml"))

    # Create valid inputs.local.yaml
    local_inputs_yaml <- list(
      paths = list(
        test_input = "/path/to/test.tsv"
      )
    )
    yaml::write_yaml(local_inputs_yaml, file.path(dir, "inputs.local.yaml"))

    proj <- pm::PMProject$new(dir)
    expect_error(
      proj$parse_inputs(),
      regexp = "must contain at least one key starting with 'inputs.'"
    )
  })

  it("Errors when input definition is not a YAML object", {
    dir <- .get_good_project_path()

    # Create inputs.yaml with non-object value
    inputs_yaml <- list(
      `inputs.test_input` = "not an object"
    )
    yaml::write_yaml(inputs_yaml, file.path(dir, "inputs.yaml"))

    # Create valid inputs.local.yaml
    local_inputs_yaml <- list(
      paths = list(
        test_input = "/path/to/test.tsv"
      )
    )
    yaml::write_yaml(local_inputs_yaml, file.path(dir, "inputs.local.yaml"))

    proj <- pm::PMProject$new(dir)
    expect_error(
      proj$parse_inputs(),
      regexp = "must be a YAML object"
    )
  })

  it("Errors when inputs.local.yaml has invalid YAML syntax", {
    dir <- .get_good_project_path()

    # Create valid inputs.yaml
    inputs_yaml <- list(
      `inputs.test_input` = list(type = "tsv")
    )
    yaml::write_yaml(inputs_yaml, file.path(dir, "inputs.yaml"))

    # Create invalid inputs.local.yaml (malformed YAML)
    writeLines("invalid: yaml: content: bad", file.path(dir, "inputs.local.yaml"))

    proj <- pm::PMProject$new(dir)
    expect_error(
      proj$parse_inputs(),
      regexp = "must be a YAML object"
    )
  })

  it("Errors when inputs.local.yaml is missing 'paths' key", {
    dir <- .get_good_project_path()

    # Create valid inputs.yaml
    inputs_yaml <- list(
      `inputs.test_input` = list(type = "tsv")
    )
    yaml::write_yaml(inputs_yaml, file.path(dir, "inputs.yaml"))

    # Create inputs.local.yaml without 'paths' key
    local_inputs_yaml <- list(
      other_key = "value"
    )
    yaml::write_yaml(local_inputs_yaml, file.path(dir, "inputs.local.yaml"))

    proj <- pm::PMProject$new(dir)
    expect_error(
      proj$parse_inputs(),
      regexp = "must have a top-level 'paths' key"
    )
  })

  it("Errors when 'paths' is not a YAML object", {
    dir <- .get_good_project_path()

    # Create valid inputs.yaml
    inputs_yaml <- list(
      `inputs.test_input` = list(type = "tsv")
    )
    yaml::write_yaml(inputs_yaml, file.path(dir, "inputs.yaml"))

    # Create inputs.local.yaml with 'paths' as non-object
    local_inputs_yaml <- list(
      paths = "not an object"
    )
    yaml::write_yaml(local_inputs_yaml, file.path(dir, "inputs.local.yaml"))

    proj <- pm::PMProject$new(dir)
    expect_error(
      proj$parse_inputs(),
      regexp = "'paths' in inputs.local.yaml must be a YAML object"
    )
  })

  it("Errors when path value is not a string", {
    dir <- .get_good_project_path()

    # Create valid inputs.yaml
    inputs_yaml <- list(
      `inputs.test_input` = list(type = "tsv")
    )
    yaml::write_yaml(inputs_yaml, file.path(dir, "inputs.yaml"))

    # Create inputs.local.yaml with non-string path
    local_inputs_yaml <- list(
      paths = list(
        test_input = 123  # Not a string
      )
    )
    yaml::write_yaml(local_inputs_yaml, file.path(dir, "inputs.local.yaml"))

    proj <- pm::PMProject$new(dir)
    expect_error(
      proj$parse_inputs(),
      regexp = "must be character"
    )
  })

  it("Validates fingerprint field types correctly", {
    dir <- .get_good_project_path()

    # Create inputs.yaml with invalid md5 (not a string)
    inputs_yaml <- list(
      `inputs.test_input` = list(
        type = "tsv",
        md5 = 123  # Should be string
      )
    )
    yaml::write_yaml(inputs_yaml, file.path(dir, "inputs.yaml"))

    # Create valid inputs.local.yaml
    local_inputs_yaml <- list(
      paths = list(
        test_input = "/path/to/test.tsv"
      )
    )
    yaml::write_yaml(local_inputs_yaml, file.path(dir, "inputs.local.yaml"))

    proj <- pm::PMProject$new(dir)
    expect_error(
      proj$parse_inputs(),
      regexp = "must be character"
    )
  })

  it("Validates size field is a number", {
    dir <- .get_good_project_path()

    # Create inputs.yaml with invalid size (not a number)
    inputs_yaml <- list(
      `inputs.test_input` = list(
        type = "tsv",
        size = "not a number"  # Should be number
      )
    )
    yaml::write_yaml(inputs_yaml, file.path(dir, "inputs.yaml"))

    # Create valid inputs.local.yaml
    local_inputs_yaml <- list(
      paths = list(
        test_input = "/path/to/test.tsv"
      )
    )
    yaml::write_yaml(local_inputs_yaml, file.path(dir, "inputs.local.yaml"))

    proj <- pm::PMProject$new(dir)
    expect_error(
      proj$parse_inputs(),
      regexp = "must be a number"
    )
  })
})

