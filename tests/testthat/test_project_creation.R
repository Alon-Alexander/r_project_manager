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
    for (missing_file in c("README.md", "inputs.yaml", "inputs.local.yaml")) {
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
    expect_true(file.exists(file.path(path, "inputs.yaml")))
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

    expect_true(file.exists(file.path(dir, "inputs.yaml")))
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