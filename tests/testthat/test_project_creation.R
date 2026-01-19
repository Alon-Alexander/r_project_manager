describe("Creating project objects works as expected", {

  it("Can create project object on a valid directory", {
    dir <- .get_good_project_path()
    proj <- pm::PMProject$new(dir)
    expect_s3_class(proj, "PMProject")
    expect_equal(proj$path, normalizePath(dir))

    # Also test the wrapper
    proj <- pm::pm_project(dir)
    expect_s3_class(proj, "PMProject")
    expect_equal(proj$path, normalizePath(dir))
  })

  it("Errors when there is a missing important file", {
    for (missing_file in c("README.md", "project.yaml", "inputs.local.yaml")) {
      dir <- .get_good_project_path()

      # Explicitly remove the missing file
      file.remove(file.path(dir, missing_file))

      expect_error(
        pm::PMProject$new(dir),
        regexp = "must specify existing files",
        label = missing_file
      )
      expect_error(
        pm::pm_project(dir),
        regexp = "must specify existing files",
        label = missing_file
      )
    }
  })

  it("Fails for non existing folder", {
    expect_error(
      PMProject$new("this_is_a/non/existing_folder"),
      regexp = "must specify existing directories"
    )
  })
})

describe("Creating new project with pm_create_project works", {
  it("Works when output directory does not exist", {
    dir <- withr::local_tempdir()

    # Works with trailing slash
    pm::pm_create_project(file.path(dir, "my_new_project/"))

    # Works without trailing slash
    path <- file.path(dir, "another_project")
    pm::pm_create_project(path)

    # Validate some files
    expect_true(dir.exists(path))
    expect_true(file.exists(file.path(path, "project.yaml")))
    expect_true(file.exists(file.path(path, "inputs.local.yaml")))
    expect_true(file.exists(file.path(path, "README.md")))
    expect_true(file.exists(file.path(path, ".gitignore")))
    expect_true(dir.exists(file.path(path, "analyses")))
  })

  it("Works when output directory exists and empty", {
    dir <- withr::local_tempdir()
    proj <- pm_create_project(dir)
    expect_s3_class(proj, "PMProject")
    expect_equal(proj$path, normalizePath(dir))

    expect_true(file.exists(file.path(dir, "project.yaml")))
    expect_true(file.exists(file.path(dir, "inputs.local.yaml")))
    expect_true(file.exists(file.path(dir, "README.md")))
    expect_true(file.exists(file.path(dir, ".gitignore")))
    expect_true(dir.exists(file.path(dir, "analyses")))
  })

  it("Fails if there is irrelevant content in the folder", {
    dir <- withr::local_tempdir()

    file.create(file.path(dir, "some_file.txt"))

    expect_error(
      pm_create_project(dir),
      regexp = "which exists but contains an invalid project"
    )
  })
})

describe("Input file validation in PMProject$validate()", {
  it("Passes validation when all input files exist", {
    dir <- .get_good_project_path()

    # Create valid project.yaml
    inputs_yaml <- list(
      inputs = list(
        feature_table = list(),
        sample_metadata = list()
      )
    )
    yaml::write_yaml(inputs_yaml, file.path(dir, "project.yaml"))

    # Create test files
    file1 <- file.path(dir, "feature_table.csv")
    file2 <- file.path(dir, "sample_metadata.tsv")
    file.create(file1)
    file.create(file2)

    # Create valid inputs.local.yaml with relative paths
    local_inputs_yaml <- list(
      paths = list(
        feature_table = "feature_table.csv",
        sample_metadata = "sample_metadata.tsv"
      )
    )
    yaml::write_yaml(local_inputs_yaml, file.path(dir, "inputs.local.yaml"))

    # Should not error
    expect_silent(proj <- pm::PMProject$new(dir))
    expect_s3_class(proj, "PMProject")
  })

  it("Passes validation with absolute paths", {
    dir <- .get_good_project_path()

    # Create valid project.yaml
    inputs_yaml <- list(
      inputs = list(
        test_input = list()
      )
    )
    yaml::write_yaml(inputs_yaml, file.path(dir, "project.yaml"))

    # Create test file outside project directory
    temp_file <- withr::local_tempfile(fileext = ".csv")
    file.create(temp_file)

    # Create valid inputs.local.yaml with absolute path
    local_inputs_yaml <- list(
      paths = list(
        test_input = temp_file
      )
    )
    yaml::write_yaml(local_inputs_yaml, file.path(dir, "inputs.local.yaml"))

    # Should not error
    expect_silent(proj <- pm::PMProject$new(dir))
    expect_s3_class(proj, "PMProject")
  })

  it("Errors when input ID is missing from inputs.local.yaml", {
    dir <- .get_good_project_path()

    # Create project.yaml with two inputs
    inputs_yaml <- list(
      inputs = list(
        feature_table = list(),
        sample_metadata = list()
      )
    )
    yaml::write_yaml(inputs_yaml, file.path(dir, "project.yaml"))

    # Create inputs.local.yaml with only one input
    local_inputs_yaml <- list(
      paths = list(
        feature_table = "feature_table.csv"
      )
    )
    yaml::write_yaml(local_inputs_yaml, file.path(dir, "inputs.local.yaml"))

    # Should error with helpful message
    expect_error(
      pm::PMProject$new(dir),
      regexp = "missing from inputs.local.yaml"
    )
    expect_error(
      pm::PMProject$new(dir),
      regexp = "sample_metadata"
    )
    expect_error(
      pm::PMProject$new(dir),
      regexp = "To fix this, add the following entries"
    )
  })

  it("Errors when multiple input IDs are missing from inputs.local.yaml", {
    dir <- .get_good_project_path()

    # Create project.yaml with three inputs
    inputs_yaml <- list(
      inputs = list(
        input1 = list(),
        input2 = list(),
        input3 = list()
      )
    )
    yaml::write_yaml(inputs_yaml, file.path(dir, "project.yaml"))

    # Create inputs.local.yaml with only one input
    local_inputs_yaml <- list(
      paths = list(
        input1 = "input1.csv"
      )
    )
    yaml::write_yaml(local_inputs_yaml, file.path(dir, "inputs.local.yaml"))

    # Should error mentioning all missing inputs
    expect_error(
      pm::PMProject$new(dir),
      regexp = "missing from inputs.local.yaml"
    )
    expect_error(
      pm::PMProject$new(dir),
      regexp = "input2"
    )
    expect_error(
      pm::PMProject$new(dir),
      regexp = "input3"
    )
  })

  it("Errors when input file does not exist (relative path)", {
    dir <- .get_good_project_path()

    # Create valid project.yaml
    inputs_yaml <- list(
      inputs = list(
        missing_file = list()
      )
    )
    yaml::write_yaml(inputs_yaml, file.path(dir, "project.yaml"))

    # Create inputs.local.yaml pointing to non-existent file
    local_inputs_yaml <- list(
      paths = list(
        missing_file = "nonexistent_file.csv"
      )
    )
    yaml::write_yaml(local_inputs_yaml, file.path(dir, "inputs.local.yaml"))

    # Should error with helpful message
    expect_error(
      pm::PMProject$new(dir),
      regexp = "do not exist"
    )
    expect_error(
      pm::PMProject$new(dir),
      regexp = "missing_file"
    )
    expect_error(
      pm::PMProject$new(dir),
      regexp = "nonexistent_file.csv"
    )
    expect_error(
      pm::PMProject$new(dir),
      regexp = "To fix this, you can either:"
    )
  })

  it("Errors when input file does not exist (absolute path)", {
    dir <- .get_good_project_path()

    # Create valid project.yaml
    inputs_yaml <- list(
      inputs = list(
        missing_file = list()
      )
    )
    yaml::write_yaml(inputs_yaml, file.path(dir, "project.yaml"))

    # Create inputs.local.yaml pointing to non-existent absolute path
    nonexistent_path <- file.path(tempdir(), "nonexistent_file.csv")
    local_inputs_yaml <- list(
      paths = list(
        missing_file = nonexistent_path
      )
    )
    yaml::write_yaml(local_inputs_yaml, file.path(dir, "inputs.local.yaml"))

    # Should error with helpful message
    expect_error(
      pm::PMProject$new(dir),
      regexp = "do not exist"
    )
    expect_error(
      pm::PMProject$new(dir),
      regexp = "missing_file"
    )
    # Check for the filename in the error message (path may be normalized)
    expect_error(
      pm::PMProject$new(dir),
      regexp = "nonexistent_file"
    )
  })

  it("Errors when multiple input files do not exist", {
    dir <- .get_good_project_path()

    # Create valid project.yaml
    inputs_yaml <- list(
      inputs = list(
        missing1 = list(),
        missing2 = list()
      )
    )
    yaml::write_yaml(inputs_yaml, file.path(dir, "project.yaml"))

    # Create inputs.local.yaml pointing to non-existent files
    local_inputs_yaml <- list(
      paths = list(
        missing1 = "nonexistent1.csv",
        missing2 = "nonexistent2.tsv"
      )
    )
    yaml::write_yaml(local_inputs_yaml, file.path(dir, "inputs.local.yaml"))

    # Should error mentioning all missing files
    expect_error(
      pm::PMProject$new(dir),
      regexp = "do not exist"
    )
    expect_error(
      pm::PMProject$new(dir),
      regexp = "missing1"
    )
    expect_error(
      pm::PMProject$new(dir),
      regexp = "missing2"
    )
  })

  it("Errors when both missing entries and missing files occur", {
    dir <- .get_good_project_path()

    # Create project.yaml with three inputs
    inputs_yaml <- list(
      inputs = list(
        missing_entry = list(),
        missing_file = list(),
        valid_input = list()
      )
    )
    yaml::write_yaml(inputs_yaml, file.path(dir, "project.yaml"))

    # Create a valid file
    valid_file <- file.path(dir, "valid_file.csv")
    file.create(valid_file)

    # Create inputs.local.yaml with one missing entry and one missing file
    local_inputs_yaml <- list(
      paths = list(
        missing_file = "nonexistent.csv",
        valid_input = "valid_file.csv"
      )
    )
    yaml::write_yaml(local_inputs_yaml, file.path(dir, "inputs.local.yaml"))

    # Should error mentioning both issues
    expect_error(
      pm::PMProject$new(dir),
      regexp = "missing from inputs.local.yaml"
    )
    expect_error(
      pm::PMProject$new(dir),
      regexp = "missing_entry"
    )
    expect_error(
      pm::PMProject$new(dir),
      regexp = "do not exist"
    )
    expect_error(
      pm::PMProject$new(dir),
      regexp = "missing_file"
    )
  })

  it("Works with array format in project.yaml", {
    dir <- .get_good_project_path()

    # Create project.yaml in array format
    writeLines(
      c(
        "inputs:",
        "  - feature_table",
        "  - sample_metadata"
      ),
      file.path(dir, "project.yaml")
    )

    # Create test files
    file1 <- file.path(dir, "feature_table.csv")
    file2 <- file.path(dir, "sample_metadata.tsv")
    file.create(file1)
    file.create(file2)

    # Create valid inputs.local.yaml
    local_inputs_yaml <- list(
      paths = list(
        feature_table = "feature_table.csv",
        sample_metadata = "sample_metadata.tsv"
      )
    )
    yaml::write_yaml(local_inputs_yaml, file.path(dir, "inputs.local.yaml"))

    # Should not error
    expect_silent(proj <- pm::PMProject$new(dir))
    expect_s3_class(proj, "PMProject")
  })

  it("Validates input files when validate() is called explicitly", {
    dir <- .get_good_project_path()

    # Create valid project.yaml
    inputs_yaml <- list(
      inputs = list(
        test_input = list()
      )
    )
    yaml::write_yaml(inputs_yaml, file.path(dir, "project.yaml"))

    # Create test file
    test_file <- file.path(dir, "test_file.csv")
    file.create(test_file)

    # Create valid inputs.local.yaml
    local_inputs_yaml <- list(
      paths = list(
        test_input = "test_file.csv"
      )
    )
    yaml::write_yaml(local_inputs_yaml, file.path(dir, "inputs.local.yaml"))

    # Create project (should pass)
    proj <- pm::PMProject$new(dir)

    # Remove the file
    file.remove(test_file)

    # Calling validate() explicitly should now error
    expect_error(
      proj$validate(),
      regexp = "do not exist"
    )
    expect_error(
      proj$validate(),
      regexp = "test_input"
    )
  })

  it("Handles nested relative paths correctly", {
    dir <- .get_good_project_path()

    # Create a subdirectory
    subdir <- file.path(dir, "data")
    dir.create(subdir)

    # Create valid project.yaml
    inputs_yaml <- list(
      inputs = list(
        test_input = list()
      )
    )
    yaml::write_yaml(inputs_yaml, file.path(dir, "project.yaml"))

    # Create test file in subdirectory
    test_file <- file.path(subdir, "test_file.csv")
    file.create(test_file)

    # Create inputs.local.yaml with relative path to subdirectory
    local_inputs_yaml <- list(
      paths = list(
        test_input = "data/test_file.csv"
      )
    )
    yaml::write_yaml(local_inputs_yaml, file.path(dir, "inputs.local.yaml"))

    # Should not error
    expect_silent(proj <- pm::PMProject$new(dir))
    expect_s3_class(proj, "PMProject")
  })

  it("Errors when inputs.local.yaml is empty but inputs are defined", {
    dir <- .get_good_project_path()

    # Create valid project.yaml
    inputs_yaml <- list(
      inputs = list(
        test_input = list()
      )
    )
    yaml::write_yaml(inputs_yaml, file.path(dir, "project.yaml"))

    # Create empty inputs.local.yaml
    local_inputs_yaml <- list()
    yaml::write_yaml(local_inputs_yaml, file.path(dir, "inputs.local.yaml"))

    # Should error with helpful message
    expect_error(
      pm::PMProject$new(dir),
      regexp = "missing from inputs.local.yaml"
    )
    expect_error(
      pm::PMProject$new(dir),
      regexp = "test_input"
    )
    expect_error(
      pm::PMProject$new(dir),
      regexp = "To fix this, add the following entries"
    )
  })

  it("Errors when inputs.local.yaml has no paths key but inputs are defined", {
    dir <- .get_good_project_path()

    # Create valid project.yaml
    inputs_yaml <- list(
      inputs = list(
        test_input = list()
      )
    )
    yaml::write_yaml(inputs_yaml, file.path(dir, "project.yaml"))

    # Create inputs.local.yaml without paths key
    local_inputs_yaml <- list(
      other_key = "value"
    )
    yaml::write_yaml(local_inputs_yaml, file.path(dir, "inputs.local.yaml"))

    # Should error (schema validation should catch missing paths key)
    expect_error(
      pm::PMProject$new(dir),
      regexp = "must have a top-level 'paths' key"
    )
  })

  it("Provides detailed error messages showing both original and resolved paths", {
    dir <- .get_good_project_path()

    # Create valid project.yaml
    inputs_yaml <- list(
      inputs = list(
        test_input = list()
      )
    )
    yaml::write_yaml(inputs_yaml, file.path(dir, "project.yaml"))

    # Create inputs.local.yaml with relative path
    local_inputs_yaml <- list(
      paths = list(
        test_input = "data/nonexistent.csv"
      )
    )
    yaml::write_yaml(local_inputs_yaml, file.path(dir, "inputs.local.yaml"))

    # Should error with detailed message showing both paths
    error_msg <- tryCatch(
      pm::PMProject$new(dir),
      error = function(e) conditionMessage(e)
    )
    expect_true(is.character(error_msg))
    expect_true(grepl("Path in inputs.local.yaml", error_msg) || grepl("Path:", error_msg))
    expect_true(grepl("data/nonexistent.csv", error_msg))
    expect_true(grepl("Resolved to", error_msg) || grepl("File not found", error_msg))
  })

  it("Provides detailed error messages for absolute paths", {
    dir <- .get_good_project_path()

    # Create valid project.yaml
    inputs_yaml <- list(
      inputs = list(
        test_input = list()
      )
    )
    yaml::write_yaml(inputs_yaml, file.path(dir, "project.yaml"))

    # Create inputs.local.yaml with absolute path
    nonexistent_path <- file.path(tempdir(), "nonexistent_file.csv")
    local_inputs_yaml <- list(
      paths = list(
        test_input = nonexistent_path
      )
    )
    yaml::write_yaml(local_inputs_yaml, file.path(dir, "inputs.local.yaml"))

    # Should error with detailed message
    error_msg <- tryCatch(
      pm::PMProject$new(dir),
      error = function(e) conditionMessage(e)
    )
    expect_true(is.character(error_msg))
    expect_true(grepl("test_input", error_msg))
    expect_true(grepl("File not found", error_msg))
    expect_true(grepl("To fix this", error_msg))
  })

  it("Shows count of missing entries in error message", {
    dir <- .get_good_project_path()

    # Create project.yaml with multiple inputs
    inputs_yaml <- list(
      inputs = list(
        input1 = list(),
        input2 = list(),
        input3 = list()
      )
    )
    yaml::write_yaml(inputs_yaml, file.path(dir, "project.yaml"))

    # Create empty inputs.local.yaml
    local_inputs_yaml <- list()
    yaml::write_yaml(local_inputs_yaml, file.path(dir, "inputs.local.yaml"))

    # Should error showing count
    error_msg <- tryCatch(
      pm::PMProject$new(dir),
      error = function(e) conditionMessage(e)
    )
    expect_true(grepl("3 input IDs", error_msg))
    expect_true(grepl("input1", error_msg))
    expect_true(grepl("input2", error_msg))
    expect_true(grepl("input3", error_msg))
  })

  it("Shows count of missing files in error message", {
    dir <- .get_good_project_path()

    # Create valid project.yaml
    inputs_yaml <- list(
      inputs = list(
        missing1 = list(),
        missing2 = list(),
        missing3 = list()
      )
    )
    yaml::write_yaml(inputs_yaml, file.path(dir, "project.yaml"))

    # Create inputs.local.yaml pointing to non-existent files
    local_inputs_yaml <- list(
      paths = list(
        missing1 = "file1.csv",
        missing2 = "file2.tsv",
        missing3 = "file3.rds"
      )
    )
    yaml::write_yaml(local_inputs_yaml, file.path(dir, "inputs.local.yaml"))

    # Should error showing count
    error_msg <- tryCatch(
      pm::PMProject$new(dir),
      error = function(e) conditionMessage(e)
    )
    expect_true(grepl("3 input files", error_msg))
    expect_true(grepl("missing1", error_msg))
    expect_true(grepl("missing2", error_msg))
    expect_true(grepl("missing3", error_msg))
  })

  it("Provides example fix in error message for missing entries", {
    dir <- .get_good_project_path()

    # Create valid project.yaml
    inputs_yaml <- list(
      inputs = list(
        example_input = list()
      )
    )
    yaml::write_yaml(inputs_yaml, file.path(dir, "project.yaml"))

    # Create empty inputs.local.yaml
    local_inputs_yaml <- list()
    yaml::write_yaml(local_inputs_yaml, file.path(dir, "inputs.local.yaml"))

    # Should error with example
    error_msg <- tryCatch(
      pm::PMProject$new(dir),
      error = function(e) conditionMessage(e)
    )
    expect_true(grepl("Example:", error_msg))
    expect_true(grepl("paths:", error_msg))
    expect_true(grepl("example_input", error_msg))
  })

  it("Handles case where inputs.local.yaml is completely missing paths section", {
    dir <- .get_good_project_path()

    # Create valid project.yaml
    inputs_yaml <- list(
      inputs = list(
        test_input = list()
      )
    )
    yaml::write_yaml(inputs_yaml, file.path(dir, "project.yaml"))

    # Create inputs.local.yaml as empty list (no paths key)
    # This should be caught by schema validation, but let's test the file validation too
    local_inputs_yaml <- list()
    yaml::write_yaml(local_inputs_yaml, file.path(dir, "inputs.local.yaml"))

    # Should error
    expect_error(
      pm::PMProject$new(dir),
      regexp = "missing from inputs.local.yaml"
    )
  })

  it("Error message includes helpful note about relative paths", {
    dir <- .get_good_project_path()

    # Create valid project.yaml
    inputs_yaml <- list(
      inputs = list(
        test_input = list()
      )
    )
    yaml::write_yaml(inputs_yaml, file.path(dir, "project.yaml"))

    # Create inputs.local.yaml with relative path
    local_inputs_yaml <- list(
      paths = list(
        test_input = "relative/path/file.csv"
      )
    )
    yaml::write_yaml(local_inputs_yaml, file.path(dir, "inputs.local.yaml"))

    # Should error with note about relative paths
    error_msg <- tryCatch(
      pm::PMProject$new(dir),
      error = function(e) conditionMessage(e)
    )
    expect_true(grepl("relative paths", error_msg, ignore.case = TRUE))
    expect_true(grepl("project directory", error_msg, ignore.case = TRUE))
  })

  it("Works correctly when all inputs have valid files", {
    dir <- .get_good_project_path()

    # Create project.yaml with multiple inputs
    inputs_yaml <- list(
      inputs = list(
        input1 = list(),
        input2 = list(),
        input3 = list()
      )
    )
    yaml::write_yaml(inputs_yaml, file.path(dir, "project.yaml"))

    # Create test files
    file1 <- file.path(dir, "file1.csv")
    file2 <- file.path(dir, "file2.tsv")
    file3 <- file.path(dir, "file3.rds")
    file.create(file1)
    file.create(file2)
    file.create(file3)

    # Create valid inputs.local.yaml
    local_inputs_yaml <- list(
      paths = list(
        input1 = "file1.csv",
        input2 = "file2.tsv",
        input3 = "file3.rds"
      )
    )
    yaml::write_yaml(local_inputs_yaml, file.path(dir, "inputs.local.yaml"))

    # Should not error
    expect_silent(proj <- pm::PMProject$new(dir))
    expect_s3_class(proj, "PMProject")
  })

  it("Validates files when validate() is called after file deletion", {
    dir <- .get_good_project_path()

    # Create valid project.yaml
    inputs_yaml <- list(
      inputs = list(
        input1 = list(),
        input2 = list()
      )
    )
    yaml::write_yaml(inputs_yaml, file.path(dir, "project.yaml"))

    # Create test files
    file1 <- file.path(dir, "file1.csv")
    file2 <- file.path(dir, "file2.tsv")
    file.create(file1)
    file.create(file2)

    # Create valid inputs.local.yaml
    local_inputs_yaml <- list(
      paths = list(
        input1 = "file1.csv",
        input2 = "file2.tsv"
      )
    )
    yaml::write_yaml(local_inputs_yaml, file.path(dir, "inputs.local.yaml"))

    # Create project (should pass)
    proj <- pm::PMProject$new(dir)

    # Remove one file
    file.remove(file1)

    # Calling validate() explicitly should now error
    expect_error(
      proj$validate(),
      regexp = "do not exist"
    )
    expect_error(
      proj$validate(),
      regexp = "input1"
    )
    expect_error(
      proj$validate(),
      regexp = "file1.csv"
    )
  })
})

describe("PMProject print method works correctly", {
  it("Prints basic project information with no analyses", {
    dir <- .get_good_project_path()
    pm <- pm::PMProject$new(dir)

    # Capture output
    output <- capture.output(print(pm))

    expect_true(any(grepl("PMProject:", output)))
    expect_true(any(grepl("Path:", output)))
    expect_true(any(grepl(normalizePath(dir), output, fixed = TRUE)))
    expect_true(any(grepl("Analyses: 0", output)))
  })

  it("Prints project information with analyses", {
    dir <- .get_good_project_path()
    pm <- pm::PMProject$new(dir)
    pm$create_analysis("analysis1")
    pm$create_analysis("analysis2")

    # Capture output
    output <- capture.output(print(pm))

    expect_true(any(grepl("PMProject:", output)))
    expect_true(any(grepl("Analyses: 2", output)))
    expect_true(any(grepl("- analysis1", output)))
    expect_true(any(grepl("- analysis2", output)))
  })

  it("Truncates analysis list when there are more than 5 analyses", {
    dir <- .get_good_project_path()
    pm <- pm::PMProject$new(dir)

    # Create 7 analyses
    for (i in 1:7) {
      pm$create_analysis(paste0("analysis", i))
    }

    # Capture output
    output <- capture.output(print(pm))

    expect_true(any(grepl("Analyses: 7", output)))
    expect_true(any(grepl("- analysis1", output)))
    expect_true(any(grepl("- analysis2", output)))
    expect_true(any(grepl("- analysis3", output)))
    expect_true(any(grepl("- analysis4", output)))
    expect_false(any(grepl("- analysis5", output)))  # Should not show 5th
    expect_true(any(grepl("... and 3 more", output)))
  })

  it("Shows inputs count when inputs are configured", {
    dir <- .get_good_project_path()
    pm <- pm::PMProject$new(dir)

    # Capture output
    output <- capture.output(print(pm))

    expect_true(any(grepl("PMProject:", output)))
    expect_true(any(grepl("Inputs:", output)))
    # Should show 2 inputs (test_input and test_tsv from helper)
    expect_true(any(grepl("Inputs: 2", output)))
  })

  it("Returns self invisibly", {
    dir <- .get_good_project_path()
    pm <- pm::PMProject$new(dir)

    # Should return self invisibly
    capture.output(result <- print(pm))
    expect_identical(result, pm)
  })

  it("Handles errors gracefully when listing analyses fails", {
    dir <- .get_good_project_path()
    pm <- pm::PMProject$new(dir)

    # Remove analyses directory to simulate error
    unlink(file.path(dir, "analyses"), recursive = TRUE)

    # Should not error, but show 0 analyses
    output <- capture.output(print(pm))
    expect_true(any(grepl("Analyses: 0", output)))
  })

  it("Shows exactly 5 analyses when there are exactly 5", {
    dir <- .get_good_project_path()
    pm <- pm::PMProject$new(dir)

    # Create exactly 5 analyses
    for (i in 1:5) {
      pm$create_analysis(paste0("analysis", i))
    }

    # Capture output
    output <- capture.output(print(pm))

    expect_true(any(grepl("Analyses: 5", output)))
    expect_true(any(grepl("- analysis1", output)))
    expect_true(any(grepl("- analysis2", output)))
    expect_true(any(grepl("- analysis3", output)))
    expect_true(any(grepl("- analysis4", output)))
    expect_true(any(grepl("- analysis5", output)))
    expect_false(any(grepl("... and", output)))  # Should not show truncation
  })
})