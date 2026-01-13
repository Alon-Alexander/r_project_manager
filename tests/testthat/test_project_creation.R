describe("Creating project objects works as expected", {
  good_project_path <- "./data/good_project"

  it("Can create project object on a valid directory", {
    proj <- pm::PMProject$new(good_project_path)
  })

  it("Errors when there is a missing important file", {
    for (missing_file in c("README.md", "inputs.yaml", "inputs.local.yaml")) {
      dir <- withr::local_tempdir()
      file.copy(good_project_path, dir, recursive = TRUE)
      dir <- file.path(dir, basename(good_project_path))

      # Explicitly remove the missing file
      file.remove(file.path(dir, missing_file))

      expect_error(
        pm::PMProject$new(dir),
        regexp = "must specify existing files",
        label = missing_file
      )
    }
  })

  it ("Fails for non existing folder", {
    expect_error(
      PMProject$new("this_is_a/non/existing_folder"),
      regexp = "must specify existing directories"
    )
  })
})