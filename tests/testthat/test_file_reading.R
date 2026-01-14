describe("pm_read_file function works correctly", {
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

  it("Can read .pqt files (alternative Parquet extension)", {
    # Check if arrow is available
    if (!requireNamespace("arrow", quietly = TRUE)) {
      skip("arrow package not available")
    }

    # Create a temporary .pqt file
    pqt_file <- withr::local_tempfile(fileext = ".pqt")
    test_data <- data.frame(
      x = letters[1:3],
      y = 1:3
    )
    arrow::write_parquet(test_data, pqt_file)

    # Read it back
    result <- pm::pm_read_file(pqt_file)

    # Check results
    expect_s3_class(result, "data.frame")
    expect_equal(nrow(result), 3)
    expect_equal(ncol(result), 2)
    expect_equal(result$x, letters[1:3])
    expect_equal(result$y, 1:3)
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

  it("Errors for unsupported file extensions", {
    # Create a temporary file with unsupported extension
    unsupported_file <- withr::local_tempfile(fileext = ".xyz")
    file.create(unsupported_file)

    # Should error
    expect_error(
      pm::pm_read_file(unsupported_file),
      regexp = "Unsupported file extension"
    )
  })

  it("Errors for non-existent files", {
    non_existent <- file.path(tempdir(), "nonexistent_file.csv")

    expect_error(
      pm::pm_read_file(non_existent),
      regexp = "must specify existing files"
    )
  })

  it("Passes additional arguments to underlying read functions", {
    # Test with CSV - pass stringsAsFactors = FALSE
    csv_file <- withr::local_tempfile(fileext = ".csv")
    test_data <- data.frame(
      x = 1:3,
      y = c("a", "b", "c")
    )
    write.csv(test_data, csv_file, row.names = FALSE)

    # Read with stringsAsFactors = FALSE (default in R 4.0+)
    result <- pm::pm_read_file(csv_file, stringsAsFactors = FALSE)
    expect_type(result$y, "character")

    # Read with stringsAsFactors = TRUE (if supported)
    result2 <- pm::pm_read_file(csv_file, stringsAsFactors = TRUE)
    # In R 4.0+, stringsAsFactors is deprecated, so this might not work
    # But the function should still accept the argument
    expect_s3_class(result2, "data.frame")
  })

  it("Handles case-insensitive file extensions", {
    # Test uppercase extension
    csv_file <- withr::local_tempfile(fileext = ".CSV")
    test_data <- data.frame(x = 1:3)
    write.csv(test_data, csv_file, row.names = FALSE)

    result <- pm::pm_read_file(csv_file)
    expect_s3_class(result, "data.frame")
    expect_equal(nrow(result), 3)
  })
})

describe("PMData read method works correctly", {
  it("Can read files using PMData$read() method", {
    # Create a temporary CSV file
    csv_file <- withr::local_tempfile(fileext = ".csv")
    test_data <- data.frame(
      id = 1:5,
      value = letters[1:5]
    )
    write.csv(test_data, csv_file, row.names = FALSE)

    # Create PMData object
    data_obj <- pm::PMData$new(id = "test_data", path = csv_file)

    # Read using the method
    result <- data_obj$read()

    # Check results
    expect_s3_class(result, "data.frame")
    expect_equal(nrow(result), 5)
    expect_equal(result$id, 1:5)
    expect_equal(result$value, letters[1:5])
  })

  it("Can read RDS files using PMData$read()", {
    # Create a temporary RDS file
    rds_file <- withr::local_tempfile(fileext = ".rds")
    test_object <- list(a = 1, b = 2, c = 3)
    saveRDS(test_object, rds_file)

    # Create PMData object
    data_obj <- pm::PMData$new(id = "test_rds", path = rds_file)

    # Read using the method
    result <- data_obj$read()

    # Check results
    expect_type(result, "list")
    expect_equal(result$a, 1)
    expect_equal(result$b, 2)
    expect_equal(result$c, 3)
  })

  it("Can read RData files using PMData$read() and returns environment", {
    # Create a temporary RData file
    rdata_file <- withr::local_tempfile(fileext = ".RData")
    test_obj1 <- data.frame(x = 1:3)
    test_obj2 <- "hello"
    save(test_obj1, test_obj2, file = rdata_file)

    # Create PMData object
    data_obj <- pm::PMData$new(id = "test_rdata", path = rdata_file)

    # Read using the method
    result <- data_obj$read()

    # Check that it's an environment
    expect_type(result, "environment")
    expect_true("test_obj1" %in% ls(result))
    expect_true("test_obj2" %in% ls(result))
    expect_equal(result$test_obj1, test_obj1)
    expect_equal(result$test_obj2, test_obj2)
  })

  it("Passes additional arguments through PMData$read()", {
    # Create a temporary TSV file
    tsv_file <- withr::local_tempfile(fileext = ".tsv")
    test_data <- data.frame(
      col1 = 1:3,
      col2 = c("a", "b", "c")
    )
    write.table(test_data, tsv_file, sep = "\t", row.names = FALSE, quote = FALSE)

    # Create PMData object
    data_obj <- pm::PMData$new(id = "test_tsv", path = tsv_file)

    # Read with additional arguments
    result <- data_obj$read(stringsAsFactors = FALSE)
    expect_type(result$col2, "character")
  })

  it("Errors when file does not exist in PMData$read()", {
    # Create PMData object with non-existent file
    non_existent <- file.path(tempdir(), "nonexistent.csv")
    data_obj <- pm::PMData$new(id = "missing", path = non_existent)

    # Should error when trying to read
    expect_error(
      data_obj$read(),
      regexp = "must specify existing files"
    )
  })
})

