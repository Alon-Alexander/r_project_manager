# Extracted from test_file_reading.R:155

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "pm", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
it("Can read CSV files", {
    # Create a temporary CSV file
    csv_file <- withr::local_tempfile(fileext = ".csv")
    test_data <- data.frame(
      x = 1:5,
      y = letters[1:5],
      z = c(TRUE, FALSE, TRUE, FALSE, TRUE)
    )
    write.csv(test_data, csv_file, row.names = FALSE)

    # Read it back
    result <- pm::pm_read_file(csv_file)

    # Check results
    expect_s3_class(result, "data.frame")
    expect_equal(nrow(result), 5)
    expect_equal(ncol(result), 3)
    expect_equal(result$x, 1:5)
    expect_equal(result$y, letters[1:5])
    expect_equal(result$z, c(TRUE, FALSE, TRUE, FALSE, TRUE))
  })
it("Can read TSV files", {
    # Create a temporary TSV file
    tsv_file <- withr::local_tempfile(fileext = ".tsv")
    test_data <- data.frame(
      a = 1:3,
      b = c("one", "two", "three")
    )
    write.table(test_data, tsv_file, sep = "\t", row.names = FALSE, quote = FALSE)

    # Read it back
    result <- pm::pm_read_file(tsv_file)

    # Check results
    expect_s3_class(result, "data.frame")
    expect_equal(nrow(result), 3)
    expect_equal(ncol(result), 2)
    expect_equal(result$a, 1:3)
    expect_equal(result$b, c("one", "two", "three"))
  })
it("Can read RDS files", {
    # Create a temporary RDS file
    rds_file <- withr::local_tempfile(fileext = ".rds")
    test_object <- list(
      data = 1:10,
      name = "test",
      nested = list(a = 1, b = 2)
    )
    saveRDS(test_object, rds_file)

    # Read it back
    result <- pm::pm_read_file(rds_file)

    # Check results
    expect_type(result, "list")
    expect_equal(result$data, 1:10)
    expect_equal(result$name, "test")
    expect_equal(result$nested$a, 1)
    expect_equal(result$nested$b, 2)
  })
it("Can read RData files and returns environment", {
    # Create a temporary RData file
    rdata_file <- withr::local_tempfile(fileext = ".RData")
    obj1 <- data.frame(x = 1:3, y = 4:6)
    obj2 <- c("a", "b", "c")
    obj3 <- 42
    save(obj1, obj2, obj3, file = rdata_file)

    # Read it back
    result <- pm::pm_read_file(rdata_file)

    # Check that it's an environment
    expect_type(result, "environment")

    # Check that objects are in the environment
    expect_true("obj1" %in% ls(result))
    expect_true("obj2" %in% ls(result))
    expect_true("obj3" %in% ls(result))

    # Check object values
    expect_equal(result$obj1, obj1)
    expect_equal(result$obj2, obj2)
    expect_equal(result$obj3, obj3)
  })
it("Can read .rda files (lowercase extension)", {
    # Create a temporary .rda file
    rda_file <- withr::local_tempfile(fileext = ".rda")
    test_obj <- list(a = 1, b = 2)
    save(test_obj, file = rda_file)

    # Read it back
    result <- pm::pm_read_file(rda_file)

    # Check that it's an environment
    expect_type(result, "environment")
    expect_true("test_obj" %in% ls(result))
    expect_equal(result$test_obj, test_obj)
  })
it("Can read .RData files (mixed case extension)", {
    # Create a temporary .RData file
    rdata_file <- withr::local_tempfile(fileext = ".RData")
    test_obj <- c(1, 2, 3)
    save(test_obj, file = rdata_file)

    # Read it back
    result <- pm::pm_read_file(rdata_file)

    # Check that it's an environment
    expect_type(result, "environment")
    expect_true("test_obj" %in% ls(result))
    expect_equal(result$test_obj, test_obj)
  })
it("Can read Parquet files when arrow package is available", {
    # Check if arrow is available
    if (!requireNamespace("arrow", quietly = TRUE)) {
      skip("arrow package not available")
    }

    # Create a temporary Parquet file
    parquet_file <- withr::local_tempfile(fileext = ".parquet")
    test_data <- data.frame(
      id = 1:5,
      value = rnorm(5)
    )
    arrow::write_parquet(test_data, parquet_file)

    # Read it back
    result <- pm::pm_read_file(parquet_file)

    # Check results
    expect_s3_class(result, "data.frame")
    expect_equal(nrow(result), 5)
    expect_equal(ncol(result), 2)
    expect_equal(result$id, 1:5)
  })
it("Errors when arrow package is not available for Parquet files", {
    # Create a temporary Parquet file
    parquet_file <- withr::local_tempfile(fileext = ".parquet")
    # Create a dummy file (we won't actually read it)
    file.create(parquet_file)

    # Use mockery to mock requireNamespace() and force it to return FALSE
    mock_ns <- function(pkg, ...) {
      if (pkg == "arrow") return(FALSE)
      base::requireNamespace(pkg, ...)
    }
    mockery::stub(pm::pm_read_file, "requireNamespace", mock_ns)

    expect_error(
      pm::pm_read_file(parquet_file),
      regexp = "Reading Parquet files requires the 'arrow' package"
    )
  })
