# Extracted from test_file_reading.R:520

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "pm", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
it("Can write CSV files", {
    csv_file <- withr::local_tempfile(fileext = ".csv")
    test_data <- data.frame(
      x = 1:5,
      y = letters[1:5],
      z = c(TRUE, FALSE, TRUE, FALSE, TRUE)
    )

    # Write the file
    result <- pm::pm_write_file(csv_file, test_data)

    # Check that file was created
    expect_true(file.exists(csv_file))
    expect_equal(result, csv_file)

    # Read it back and verify
    read_back <- read.csv(csv_file)
    expect_equal(nrow(read_back), 5)
    expect_equal(ncol(read_back), 3)
    expect_equal(read_back$x, 1:5)
    expect_equal(read_back$y, letters[1:5])
  })
it("Can write TSV files", {
    tsv_file <- withr::local_tempfile(fileext = ".tsv")
    test_data <- data.frame(
      a = 1:3,
      b = c("one", "two", "three")
    )

    # Write the file
    pm::pm_write_file(tsv_file, test_data)

    # Read it back and verify
    read_back <- read.table(tsv_file, sep = "\t", header = TRUE, stringsAsFactors = FALSE)
    expect_equal(nrow(read_back), 3)
    expect_equal(ncol(read_back), 2)
    expect_equal(read_back$a, 1:3)
    expect_equal(read_back$b, c("one", "two", "three"))
  })
it("Can write RDS files", {
    rds_file <- withr::local_tempfile(fileext = ".rds")
    test_object <- list(
      data = 1:10,
      name = "test",
      nested = list(a = 1, b = 2)
    )

    # Write the file
    pm::pm_write_file(rds_file, test_object)

    # Read it back and verify
    read_back <- readRDS(rds_file)
    expect_type(read_back, "list")
    expect_equal(read_back$data, 1:10)
    expect_equal(read_back$name, "test")
    expect_equal(read_back$nested$a, 1)
    expect_equal(read_back$nested$b, 2)
  })
it("Can write RData files with single object", {
    rdata_file <- withr::local_tempfile(fileext = ".RData")
    test_obj <- data.frame(x = 1:3, y = 4:6)

    # Write the file
    pm::pm_write_file(rdata_file, test_obj)

    # Read it back and verify
    env <- new.env()
    load(rdata_file, envir = env)
    expect_true("test_obj" %in% ls(env))
    expect_equal(env$test_obj, test_obj)
  })
it("Can write RData files with multiple unnamed objects", {
    rdata_file <- withr::local_tempfile(fileext = ".RData")
    obj1 <- data.frame(x = 1:2)
    obj2 <- c("a", "b")

    # Write the file
    pm::pm_write_file(rdata_file, obj1, obj2)

    # Read it back and verify
    env <- new.env()
    load(rdata_file, envir = env)
    expect_true("obj1" %in% ls(env))
    expect_true("obj2" %in% ls(env))
    expect_equal(env$obj1, obj1)
    expect_equal(env$obj2, obj2)
  })
it("Can write RData files with multiple named objects", {
    rdata_file <- withr::local_tempfile(fileext = ".RData")
    obj1 <- data.frame(x = 1:2)
    obj2 <- c("a", "b")
    obj3 <- 42

    # Write the file with named arguments
    pm::pm_write_file(rdata_file, obj1, obj2, obj3 = obj3)

    # Read it back and verify
    env <- new.env()
    load(rdata_file, envir = env)
    expect_true("obj1" %in% ls(env))
    expect_true("obj2" %in% ls(env))
    expect_true("obj3" %in% ls(env))
    expect_equal(env$obj1, obj1)
    expect_equal(env$obj2, obj2)
    expect_equal(env$obj3, obj3)
  })
it("Can write .rda files (lowercase extension)", {
    rda_file <- withr::local_tempfile(fileext = ".rda")
    test_obj <- list(a = 1, b = 2)

    # Write the file
    pm::pm_write_file(rda_file, test_obj)

    # Read it back and verify
    env <- new.env()
    load(rda_file, envir = env)
    expect_true("test_obj" %in% ls(env))
    expect_equal(env$test_obj, test_obj)
  })
it("Can write Parquet files when arrow package is available", {
    if (!requireNamespace("arrow", quietly = TRUE)) {
      skip("arrow package not available")
    }

    parquet_file <- withr::local_tempfile(fileext = ".parquet")
    test_data <- data.frame(
      id = 1:5,
      value = rnorm(5)
    )

    # Write the file
    pm::pm_write_file(parquet_file, test_data)

    # Read it back and verify
    read_back <- arrow::read_parquet(parquet_file)
    expect_s3_class(read_back, "data.frame")
    expect_equal(nrow(read_back), 5)
    expect_equal(ncol(read_back), 2)
    expect_equal(read_back$id, 1:5)
  })
it("Can write .pqt files (alternative Parquet extension)", {
    if (!requireNamespace("arrow", quietly = TRUE)) {
      skip("arrow package not available")
    }

    pqt_file <- withr::local_tempfile(fileext = ".pqt")
    test_data <- data.frame(
      x = letters[1:3],
      y = 1:3
    )

    # Write the file
    pm::pm_write_file(pqt_file, test_data)

    # Read it back and verify
    read_back <- arrow::read_parquet(pqt_file)
    expect_s3_class(read_back, "data.frame")
    expect_equal(nrow(read_back), 3)
    expect_equal(read_back$x, letters[1:3])
    expect_equal(read_back$y, 1:3)
  })
it("Errors when arrow package is not available for Parquet files", {
    parquet_file <- withr::local_tempfile(fileext = ".parquet")
    test_data <- data.frame(x = 1:3)

    # Use mockery to mock requireNamespace() and force it to return FALSE
    mock_ns <- function(pkg, ...) {
      if (pkg == "arrow") return(FALSE)
      base::requireNamespace(pkg, ...)
    }
    mockery::stub(pm::pm_read_file, "requireNamespace", mock_ns)

    # Should error with helpful message
    expect_error(
      pm::pm_write_file(parquet_file, test_data),
      regexp = "Writing Parquet files requires the 'arrow' package"
    )
  })
