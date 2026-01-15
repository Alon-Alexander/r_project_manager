describe("PMAnalysis class works as expected", {
  it("Can create PMAnalysis from project object and name", {
    dir <- .get_good_project_path()
    pm <- pm::PMProject$new(dir)

    # Create an analysis first
    analysis <- pm$create_analysis("test_analysis")

    # Now create PMAnalysis object
    analysis_obj <- pm::PMAnalysis$new(project = pm, name = "test_analysis")

    expect_s3_class(analysis_obj, "PMAnalysis")
    expect_equal(analysis_obj$name, "test_analysis")
    expect_equal(analysis_obj$path, normalizePath(file.path(dir, "analyses", "test_analysis")))
    expect_equal(analysis_obj$project_path, normalizePath(dir))
  })

  it("PMAnalysis object has correct fields", {
    dir <- .get_good_project_path()
    pm <- pm::PMProject$new(dir)
    analysis <- pm$create_analysis("test_analysis")

    expect_true(!is.null(analysis$path))
    expect_true(!is.null(analysis$name))
    expect_true(!is.null(analysis$project_path))
    expect_type(analysis$path, "character")
    expect_type(analysis$name, "character")
    expect_type(analysis$project_path, "character")
  })

  it("Can create PMAnalysis from path", {
    dir <- .get_good_project_path()
    pm <- pm::PMProject$new(dir)

    # Create an analysis first
    analysis <- pm$create_analysis("test_analysis")

    # Create PMAnalysis from path
    analysis_path <- file.path(dir, "analyses", "test_analysis")
    analysis_obj <- pm::PMAnalysis$new(path = analysis_path)

    expect_s3_class(analysis_obj, "PMAnalysis")
    expect_equal(analysis_obj$name, "test_analysis")
    expect_equal(analysis_obj$path, normalizePath(analysis_path))
    # Should infer project path when path is within analyses directory
    expect_equal(analysis_obj$project_path, normalizePath(dir))
  })

  it("Can create PMAnalysis from absolute path", {
    dir <- .get_good_project_path()
    pm <- pm::PMProject$new(dir)

    # Create an analysis first
    analysis <- pm$create_analysis("test_analysis")

    # Create PMAnalysis from absolute path
    analysis_path <- normalizePath(analysis$path)
    analysis_obj <- pm::PMAnalysis$new(path = analysis_path)

    expect_s3_class(analysis_obj, "PMAnalysis")
    expect_equal(analysis_obj$name, "test_analysis")
    expect_equal(analysis_obj$path, analysis_path)
    # Should infer project path when path is within analyses directory
    expect_equal(analysis_obj$project_path, normalizePath(dir))
  })

  it("Errors when creating PMAnalysis from invalid path", {
    invalid_path <- file.path(tempdir(), "nonexistent_analysis")

    expect_error(
      pm::PMAnalysis$new(path = invalid_path),
      regexp = "must specify existing directories"
    )
  })

  it("Errors when creating PMAnalysis with missing required files", {
    dir <- .get_good_project_path()
    pm <- pm::PMProject$new(dir)

    # Create analysis directory manually without required files
    analysis_path <- file.path(dir, "analyses", "incomplete_analysis")
    dir.create(analysis_path, recursive = TRUE)
    dir.create(file.path(analysis_path, "code"))
    dir.create(file.path(analysis_path, "outputs"))
    dir.create(file.path(analysis_path, "intermediate"))
    dir.create(file.path(analysis_path, "logs"))
    # Missing README.md

    expect_error(
      pm::PMAnalysis$new(project = pm, name = "incomplete_analysis"),
      regexp = "must specify existing files"
    )
  })

  it("Errors when creating PMAnalysis with missing required directories", {
    dir <- .get_good_project_path()
    pm <- pm::PMProject$new(dir)

    # Create analysis directory manually without required directories
    analysis_path <- file.path(dir, "analyses", "incomplete_analysis")
    dir.create(analysis_path, recursive = TRUE)
    writeLines("# incomplete_analysis", file.path(analysis_path, "README.md"))
    dir.create(file.path(analysis_path, "code"))
    # Missing outputs, intermediate, logs

    expect_error(
      pm::PMAnalysis$new(project = pm, name = "incomplete_analysis"),
      regexp = "must specify existing directories"
    )
  })

  it("Requires either project+name or path", {
    expect_error(
      pm::PMAnalysis$new(),
      regexp = "Must provide either"
    )

    # When project is provided but name is NULL, it will fail at chk::chk_scalar
    # But if project is not a PMProject, it will fail at chk::chk_s3_class first
    dir <- .get_good_project_path()
    pm <- pm::PMProject$new(dir)

    expect_error(
      pm::PMAnalysis$new(project = pm, name = NULL),
      regexp = "scalar|NULL"
    )

    expect_error(
      pm::PMAnalysis$new(project = "not_a_project", name = "test"),
      regexp = "class|PMProject"
    )
  })

  it("Has print method", {
    dir <- .get_good_project_path()
    pm <- pm::PMProject$new(dir)
    analysis <- pm$create_analysis("test_analysis")

    # Print method should output analysis information
    output <- capture.output(print(analysis))
    expect_true(any(grepl("PMAnalysis", output)))
    expect_true(any(grepl("test_analysis", output)))
    expect_true(any(grepl("Name:", output)))
    expect_true(any(grepl("Path:", output)))
  })
})

describe("PMProject$create_analysis() works correctly", {
  it("Creates analysis with correct structure", {
    dir <- .get_good_project_path()
    pm <- pm::PMProject$new(dir)

    analysis <- pm$create_analysis("test_analysis")

    expect_s3_class(analysis, "PMAnalysis")
    expect_equal(analysis$name, "test_analysis")

    # Check directory structure
    analysis_path <- file.path(dir, "analyses", "test_analysis")
    expect_true(dir.exists(analysis_path))
    expect_true(file.exists(file.path(analysis_path, "README.md")))
    expect_true(file.exists(file.path(analysis_path, ".gitignore")))
    expect_true(dir.exists(file.path(analysis_path, "code")))
    expect_true(dir.exists(file.path(analysis_path, "outputs")))
    expect_true(dir.exists(file.path(analysis_path, "intermediate")))
    expect_true(dir.exists(file.path(analysis_path, "logs")))
  })

  it("Creates README.md with correct content", {
    dir <- .get_good_project_path()
    pm <- pm::PMProject$new(dir)

    analysis <- pm$create_analysis("my_analysis")
    readme_path <- file.path(analysis$path, "README.md")

    expect_true(file.exists(readme_path))
    readme_content <- readLines(readme_path)
    expect_true(any(grepl("^# my_analysis", readme_content)))
    expect_true(any(grepl("## Description", readme_content)))
    expect_true(any(grepl("## Code Files", readme_content)))
    expect_true(any(grepl("## Outputs", readme_content)))
  })

  it("Creates .gitignore with correct content", {
    dir <- .get_good_project_path()
    pm <- pm::PMProject$new(dir)

    analysis <- pm$create_analysis("test_analysis")
    gitignore_path <- file.path(analysis$path, ".gitignore")

    expect_true(file.exists(gitignore_path))
    gitignore_content <- readLines(gitignore_path)
    expect_true(any(grepl("outputs/", gitignore_content)))
    expect_true(any(grepl("intermediate/", gitignore_content)))
    expect_true(any(grepl("logs/", gitignore_content)))
  })

  it("Returns existing analysis if already created", {
    dir <- .get_good_project_path()
    pm <- pm::PMProject$new(dir)

    analysis1 <- pm$create_analysis("test_analysis")
    analysis2 <- pm$create_analysis("test_analysis")

    expect_equal(analysis1$path, analysis2$path)
    expect_equal(analysis1$name, analysis2$name)
  })

  it("Errors when analysis folder exists but is invalid", {
    dir <- .get_good_project_path()
    pm <- pm::PMProject$new(dir)

    # Create invalid analysis directory (just an empty directory)
    analysis_path <- file.path(dir, "analyses", "invalid_analysis")
    dir.create(analysis_path, recursive = TRUE)
    # Missing all required files/directories

    expect_error(
      pm$create_analysis("invalid_analysis"),
      regexp = "already exists but is not a valid analysis|exists but is not valid"
    )
  })

  it("Can create multiple analyses", {
    dir <- .get_good_project_path()
    pm <- pm::PMProject$new(dir)

    analysis1 <- pm$create_analysis("analysis1")
    analysis2 <- pm$create_analysis("analysis2")
    analysis3 <- pm$create_analysis("analysis3")

    expect_equal(analysis1$name, "analysis1")
    expect_equal(analysis2$name, "analysis2")
    expect_equal(analysis3$name, "analysis3")

    # All should exist
    expect_true(dir.exists(analysis1$path))
    expect_true(dir.exists(analysis2$path))
    expect_true(dir.exists(analysis3$path))
  })

  it("Validates analysis name parameter", {
    dir <- .get_good_project_path()
    pm <- pm::PMProject$new(dir)

    expect_error(
      pm$create_analysis(NULL),
      regexp = "scalar|NULL"
    )

    expect_error(
      pm$create_analysis(c("name1", "name2")),
      regexp = "scalar"
    )

    expect_error(
      pm$create_analysis(123),
      regexp = "class|character"
    )
  })
})

describe("PMProject$list_analyses() works correctly", {
  it("Returns empty vector when no analyses exist", {
    dir <- .get_good_project_path()
    pm <- pm::PMProject$new(dir)

    analyses <- pm$list_analyses()
    expect_type(analyses, "character")
    expect_length(analyses, 0)
    expect_equal(analyses, character(0))
  })

  it("Returns empty vector when analyses directory doesn't exist", {
    dir <- .get_good_project_path()
    pm <- pm::PMProject$new(dir)

    # Remove analyses directory (shouldn't happen in practice, but test robustness)
    # Note: This will make the project invalid, but list_analyses should handle it gracefully
    analyses_dir <- file.path(dir, "analyses")
    if (dir.exists(analyses_dir)) {
      unlink(analyses_dir, recursive = TRUE)
    }

    # list_analyses should handle missing directory gracefully
    analyses <- pm$list_analyses()
    expect_type(analyses, "character")
    expect_length(analyses, 0)
    expect_equal(analyses, character(0))
  })

  it("Returns all valid analysis names", {
    dir <- .get_good_project_path()
    pm <- pm::PMProject$new(dir)

    pm$create_analysis("analysis1")
    pm$create_analysis("analysis2")
    pm$create_analysis("analysis3")

    analyses <- pm$list_analyses()
    expect_type(analyses, "character")
    expect_length(analyses, 3)
    expect_true("analysis1" %in% analyses)
    expect_true("analysis2" %in% analyses)
    expect_true("analysis3" %in% analyses)
  })

  it("Only returns valid analyses", {
    dir <- .get_good_project_path()
    pm <- pm::PMProject$new(dir)

    # Create valid analysis
    pm$create_analysis("valid_analysis")

    # Create invalid analysis directory
    invalid_path <- file.path(dir, "analyses", "invalid_analysis")
    dir.create(invalid_path, recursive = TRUE)
    # Missing required files

    analyses <- pm$list_analyses()
    expect_length(analyses, 1)
    expect_true("valid_analysis" %in% analyses)
    expect_false("invalid_analysis" %in% analyses)
  })

  it("Returns all analyses regardless of creation order", {
    dir <- .get_good_project_path()
    pm <- pm::PMProject$new(dir)

    pm$create_analysis("z_analysis")
    pm$create_analysis("a_analysis")
    pm$create_analysis("m_analysis")

    analyses <- pm$list_analyses()
    # Check all are present (order may vary by filesystem)
    expect_length(analyses, 3)
    expect_true("a_analysis" %in% analyses)
    expect_true("m_analysis" %in% analyses)
    expect_true("z_analysis" %in% analyses)
  })
})

describe("PMProject$get_analysis() works correctly", {
  it("Returns analysis object for existing analysis", {
    dir <- .get_good_project_path()
    pm <- pm::PMProject$new(dir)

    created_analysis <- pm$create_analysis("test_analysis")
    retrieved_analysis <- pm$get_analysis("test_analysis")

    expect_s3_class(retrieved_analysis, "PMAnalysis")
    expect_equal(retrieved_analysis$name, "test_analysis")
    expect_equal(retrieved_analysis$path, created_analysis$path)
  })

  it("Errors when analysis does not exist", {
    dir <- .get_good_project_path()
    pm <- pm::PMProject$new(dir)

    expect_error(
      pm$get_analysis("nonexistent_analysis"),
      regexp = "does not exist in project"
    )
  })

  it("Errors when analysis exists but is invalid", {
    dir <- .get_good_project_path()
    pm <- pm::PMProject$new(dir)

    # Create invalid analysis directory (just an empty directory)
    analysis_path <- file.path(dir, "analyses", "invalid_analysis")
    dir.create(analysis_path, recursive = TRUE)
    # Missing all required files/directories

    expect_error(
      pm$get_analysis("invalid_analysis"),
      regexp = "exists but is not valid|must specify existing"
    )
  })

  it("Validates analysis name parameter", {
    dir <- .get_good_project_path()
    pm <- pm::PMProject$new(dir)

    expect_error(
      pm$get_analysis(NULL),
      regexp = "scalar|NULL"
    )

    expect_error(
      pm$get_analysis(c("name1", "name2")),
      regexp = "scalar"
    )

    expect_error(
      pm$get_analysis(123),
      regexp = "class|character"
    )
  })
})

describe("Analysis template structure", {
  it("Template files are copied correctly", {
    dir <- .get_good_project_path()
    pm <- pm::PMProject$new(dir)

    analysis <- pm$create_analysis("test_analysis")

    # Check all template files exist
    expect_true(file.exists(file.path(analysis$path, "README.md")))
    expect_true(file.exists(file.path(analysis$path, ".gitignore")))

    # Check all template directories exist
    expect_true(dir.exists(file.path(analysis$path, "code")))
    expect_true(dir.exists(file.path(analysis$path, "outputs")))
    expect_true(dir.exists(file.path(analysis$path, "intermediate")))
    expect_true(dir.exists(file.path(analysis$path, "logs")))
  })

  it("dot_ prefix files are renamed correctly", {
    dir <- .get_good_project_path()
    pm <- pm::PMProject$new(dir)

    analysis <- pm$create_analysis("test_analysis")

    # dot_gitignore should become .gitignore
    expect_true(file.exists(file.path(analysis$path, ".gitignore")))
    expect_false(file.exists(file.path(analysis$path, "dot_gitignore")))
  })

  it("README.md placeholder is replaced with analysis name", {
    dir <- .get_good_project_path()
    pm <- pm::PMProject$new(dir)

    analysis_name <- "my_custom_analysis"
    analysis <- pm$create_analysis(analysis_name)

    readme_content <- readLines(file.path(analysis$path, "README.md"))
    first_line <- readme_content[1]

    expect_equal(first_line, paste("#", analysis_name))
    # Check that placeholder is not present anywhere in the file
    # Use fixed = TRUE to match literal braces
    placeholder_pattern <- "{{ANALYSIS_NAME}}"
    expect_false(any(grepl(placeholder_pattern, readme_content, fixed = TRUE)))
  })
})

describe("Analysis integration with project", {
  it("Analysis path is relative to project", {
    dir <- .get_good_project_path()
    pm <- pm::PMProject$new(dir)

    analysis <- pm$create_analysis("test_analysis")

    expected_path <- file.path(pm$path, "analyses", "test_analysis")
    expect_equal(analysis$path, normalizePath(expected_path))
    expect_equal(analysis$project_path, normalizePath(dir))
  })

  it("Can create and retrieve multiple analyses", {
    dir <- .get_good_project_path()
    pm <- pm::PMProject$new(dir)

    # Create multiple analyses
    analysis1 <- pm$create_analysis("analysis1")
    analysis2 <- pm$create_analysis("analysis2")
    analysis3 <- pm$create_analysis("analysis3")

    # List them
    analyses_list <- pm$list_analyses()
    expect_length(analyses_list, 3)

    # Retrieve each one
    retrieved1 <- pm$get_analysis("analysis1")
    retrieved2 <- pm$get_analysis("analysis2")
    retrieved3 <- pm$get_analysis("analysis3")

    expect_equal(retrieved1$path, analysis1$path)
    expect_equal(retrieved2$path, analysis2$path)
    expect_equal(retrieved3$path, analysis3$path)
  })

  it("Analysis validation happens on creation", {
    dir <- .get_good_project_path()
    pm <- pm::PMProject$new(dir)

    # Creating an analysis should validate it automatically
    analysis <- pm$create_analysis("test_analysis")

    # Should be able to create PMAnalysis object (validation passes)
    analysis_obj <- pm::PMAnalysis$new(project = pm, name = "test_analysis")
    expect_s3_class(analysis_obj, "PMAnalysis")
  })

  it("Analysis can be accessed after project object is used", {
    dir <- .get_good_project_path()
    pm <- pm::PMProject$new(dir)

    # Create analysis
    analysis1 <- pm$create_analysis("test_analysis")
    path1 <- analysis1$path

    # Create new project object pointing to same directory
    pm2 <- pm::PMProject$new(dir)

    # Should be able to retrieve the same analysis
    analysis2 <- pm2$get_analysis("test_analysis")
    expect_equal(analysis2$path, path1)
  })

  it("Handles analysis names with special characters", {
    dir <- .get_good_project_path()
    pm <- pm::PMProject$new(dir)

    # Test with analysis name containing underscores and numbers
    analysis <- pm$create_analysis("analysis_01_test")
    expect_equal(analysis$name, "analysis_01_test")
    expect_true(dir.exists(analysis$path))

    # Should be able to retrieve it
    retrieved <- pm$get_analysis("analysis_01_test")
    expect_equal(retrieved$path, analysis$path)
  })
})
