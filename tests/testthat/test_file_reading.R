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

describe("pm_write_file function works correctly", {
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
    mockery::stub(pm::pm_write_file, "requireNamespace", mock_ns)

    # Should error with helpful message
    expect_error(
      pm::pm_write_file(parquet_file, test_data),
      regexp = "Writing Parquet files requires the 'arrow' package"
    )
  })

  it("Errors for unsupported file extensions", {
    unsupported_file <- withr::local_tempfile(fileext = ".xyz")
    test_data <- data.frame(x = 1:3)

    # Should error
    expect_error(
      pm::pm_write_file(unsupported_file, test_data),
      regexp = "Unsupported file extension"
    )
  })

  it("Errors when object is not a data.frame for CSV files", {
    csv_file <- withr::local_tempfile(fileext = ".csv")
    test_list <- list(a = 1, b = 2)

    # Should error
    expect_error(
      pm::pm_write_file(csv_file, test_list),
      regexp = "For CSV files, 'x' must be a data.frame"
    )
  })

  it("Errors when object is not a data.frame for TSV files", {
    tsv_file <- withr::local_tempfile(fileext = ".tsv")
    test_vector <- 1:5

    # Should error
    expect_error(
      pm::pm_write_file(tsv_file, test_vector),
      regexp = "For TSV files, 'x' must be a data.frame"
    )
  })

  it("Errors when object is not a data.frame for Parquet files", {
    if (!requireNamespace("arrow", quietly = TRUE)) {
      skip("arrow package not available")
    }

    parquet_file <- withr::local_tempfile(fileext = ".parquet")
    test_matrix <- matrix(1:9, nrow = 3)

    # Should error
    expect_error(
      pm::pm_write_file(parquet_file, test_matrix),
      regexp = "For Parquet files, 'x' must be a data.frame"
    )
  })

  it("Handles case-insensitive file extensions", {
    csv_file <- withr::local_tempfile(fileext = ".CSV")
    test_data <- data.frame(x = 1:3)

    # Should work
    pm::pm_write_file(csv_file, test_data)
    expect_true(file.exists(csv_file))
  })

  it("Passes additional arguments to underlying write functions", {
    csv_file <- withr::local_tempfile(fileext = ".csv")
    test_data <- data.frame(
      x = 1:3,
      y = c("a", "b", "c")
    )

    # Write with quote = FALSE
    pm::pm_write_file(csv_file, test_data, quote = FALSE)

    # Read it back and verify quotes are not present
    content <- readLines(csv_file)
    expect_false(any(grepl('"', content)))
  })

  it("Handles data.frames with row names", {
    csv_file <- withr::local_tempfile(fileext = ".csv")
    test_data <- data.frame(x = 1:3, y = letters[1:3])
    rownames(test_data) <- c("row1", "row2", "row3")

    # Write (row names should not be written by default)
    pm::pm_write_file(csv_file, test_data)

    # Read it back
    read_back <- pm::pm_read_file(csv_file)
    expect_equal(nrow(read_back), 3)
    expect_equal(read_back$x, 1:3)
  })

  it("Handles data.frames with factors", {
    csv_file <- withr::local_tempfile(fileext = ".csv")
    test_data <- data.frame(
      x = 1:3,
      y = factor(c("low", "medium", "high"), levels = c("low", "medium", "high"))
    )

    # Write
    pm::pm_write_file(csv_file, test_data)

    # Read it back
    read_back <- pm::pm_read_file(csv_file)
    expect_equal(read_back$x, 1:3)
    # Factors are typically converted to character when writing to CSV
    expect_type(read_back$y, "character")
  })

  it("Handles data.frames with dates", {
    csv_file <- withr::local_tempfile(fileext = ".csv")
    test_data <- data.frame(
      x = 1:3,
      date = as.Date(c("2023-01-01", "2023-01-02", "2023-01-03"))
    )

    # Write
    pm::pm_write_file(csv_file, test_data)

    # Read it back
    read_back <- pm::pm_read_file(csv_file)
    expect_equal(read_back$x, 1:3)
    # Dates are typically converted to character when writing to CSV
    expect_type(read_back$date, "character")
  })

  it("Returns file path invisibly", {
    csv_file <- withr::local_tempfile(fileext = ".csv")
    test_data <- data.frame(x = 1:3)

    # Write and capture output
    result <- pm::pm_write_file(csv_file, test_data)

    # Should return the file path
    expect_equal(result, csv_file)
    expect_true(file.exists(result))
  })

  it("Handles RData files with mixed named and unnamed objects", {
    rdata_file <- withr::local_tempfile(fileext = ".RData")
    obj1 <- data.frame(x = 1:2)
    obj2 <- c("a", "b")
    obj3 <- 42
    obj4 <- list(nested = TRUE)

    # Write with mix of named and unnamed
    pm::pm_write_file(rdata_file, obj1, obj2, obj3 = obj3, obj4)

    # Read it back
    env <- new.env()
    load(rdata_file, envir = env)

    # Verify all objects are present
    expect_true("obj1" %in% ls(env))
    expect_true("obj2" %in% ls(env))
    expect_true("obj3" %in% ls(env))
    expect_true("obj4" %in% ls(env))
    expect_equal(env$obj1, obj1)
    expect_equal(env$obj2, obj2)
    expect_equal(env$obj3, obj3)
    expect_equal(env$obj4, obj4)
  })
})

describe("PMData write method works correctly", {
  it("Can write files using PMData$write() method", {
    csv_file <- withr::local_tempfile(fileext = ".csv")
    test_data <- data.frame(
      id = 1:5,
      value = letters[1:5]
    )

    # Create PMData object
    data_obj <- pm::PMData$new(id = "test_data", path = csv_file)

    # Write using the method
    result <- data_obj$write(test_data)

    # Check that file was created
    expect_true(file.exists(csv_file))
    expect_equal(result, csv_file)

    # Read it back and verify
    read_back <- read.csv(csv_file)
    expect_equal(nrow(read_back), 5)
    expect_equal(read_back$id, 1:5)
  })

  it("Can write RDS files using PMData$write()", {
    rds_file <- withr::local_tempfile(fileext = ".rds")
    test_object <- list(a = 1, b = 2, c = 3)

    # Create PMData object
    data_obj <- pm::PMData$new(id = "test_rds", path = rds_file)

    # Write using the method
    data_obj$write(test_object)

    # Read it back and verify
    read_back <- readRDS(rds_file)
    expect_type(read_back, "list")
    expect_equal(read_back$a, 1)
    expect_equal(read_back$b, 2)
    expect_equal(read_back$c, 3)
  })

  it("Can write RData files using PMData$write() with multiple objects", {
    rdata_file <- withr::local_tempfile(fileext = ".RData")
    test_obj1 <- data.frame(x = 1:3)
    test_obj2 <- "hello"
    test_obj3 <- 42

    # Create PMData object
    data_obj <- pm::PMData$new(id = "test_rdata", path = rdata_file)

    # Write using the method with multiple objects
    data_obj$write(test_obj1, test_obj2, obj3 = test_obj3)

    # Read it back using PMData$read() and verify
    read_env <- data_obj$read()
    expect_type(read_env, "environment")
    expect_true("test_obj1" %in% ls(read_env))
    expect_true("test_obj2" %in% ls(read_env))
    expect_true("obj3" %in% ls(read_env))
    expect_equal(read_env$test_obj1, test_obj1)
    expect_equal(read_env$test_obj2, test_obj2)
    expect_equal(read_env$obj3, test_obj3)
  })

  it("Errors when object is not a data.frame for tabular formats", {
    csv_file <- withr::local_tempfile(fileext = ".csv")
    data_obj <- pm::PMData$new(id = "test", path = csv_file)

    # Should error
    expect_error(
      data_obj$write(list(a = 1)),
      regexp = "For CSV files, 'x' must be a data.frame"
    )
  })

  it("Returns file path invisibly from PMData$write()", {
    csv_file <- withr::local_tempfile(fileext = ".csv")
    test_data <- data.frame(x = 1:3)
    data_obj <- pm::PMData$new(id = "test", path = csv_file)

    # Write and capture output
    result <- data_obj$write(test_data)

    # Should return the file path
    expect_equal(result, csv_file)
    expect_true(file.exists(result))
  })

  it("Can write and read through PMData with additional arguments", {
    csv_file <- withr::local_tempfile(fileext = ".csv")
    test_data <- data.frame(
      x = 1:3,
      y = c("a", "b", "c")
    )
    data_obj <- pm::PMData$new(id = "test", path = csv_file)

    # Write with additional arguments
    data_obj$write(test_data, quote = FALSE)

    # Read it back
    read_data <- data_obj$read()
    expect_equal(read_data$x, test_data$x)
    expect_equal(read_data$y, test_data$y)
  })

  it("Errors for unsupported extensions in PMData$write()", {
    unsupported_file <- withr::local_tempfile(fileext = ".xyz")
    test_data <- data.frame(x = 1:3)
    data_obj <- pm::PMData$new(id = "test", path = unsupported_file)

    # Should error
    expect_error(
      data_obj$write(test_data),
      regexp = "Unsupported file extension"
    )
  })
})

describe("Round-trip integration tests through PMData", {
  it("Can write and read CSV files through PMData (write -> read)", {
    csv_file <- withr::local_tempfile(fileext = ".csv")
    original_data <- data.frame(
      x = 1:5,
      y = letters[1:5],
      z = c(TRUE, FALSE, TRUE, FALSE, TRUE)
    )

    # Create PMData object and write
    data_obj <- pm::PMData$new(id = "test", path = csv_file)
    data_obj$write(original_data)

    # Read it back
    read_data <- data_obj$read()

    # Verify round-trip
    expect_equal(nrow(read_data), nrow(original_data))
    expect_equal(ncol(read_data), ncol(original_data))
    expect_equal(read_data$x, original_data$x)
    expect_equal(read_data$y, original_data$y)
    expect_equal(read_data$z, original_data$z)
  })

  it("Can write and read TSV files through PMData (write -> read)", {
    tsv_file <- withr::local_tempfile(fileext = ".tsv")
    original_data <- data.frame(
      a = 1:3,
      b = c("one", "two", "three"),
      c = rnorm(3)
    )

    # Create PMData object and write
    data_obj <- pm::PMData$new(id = "test", path = tsv_file)
    data_obj$write(original_data)

    # Read it back
    read_data <- data_obj$read()

    # Verify round-trip
    expect_equal(nrow(read_data), nrow(original_data))
    expect_equal(ncol(read_data), ncol(original_data))
    expect_equal(read_data$a, original_data$a)
    expect_equal(read_data$b, original_data$b)
    expect_equal(read_data$c, original_data$c, tolerance = 1e-10)
  })

  it("Can write and read RDS files through PMData (write -> read)", {
    rds_file <- withr::local_tempfile(fileext = ".rds")
    original_object <- list(
      data = 1:10,
      name = "test",
      nested = list(a = 1, b = 2, c = list(x = "deep"))
    )

    # Create PMData object and write
    data_obj <- pm::PMData$new(id = "test", path = rds_file)
    data_obj$write(original_object)

    # Read it back
    read_object <- data_obj$read()

    # Verify round-trip
    expect_equal(read_object$data, original_object$data)
    expect_equal(read_object$name, original_object$name)
    expect_equal(read_object$nested, original_object$nested)
  })

  it("Can write and read RData files through PMData (write -> read)", {
    rdata_file <- withr::local_tempfile(fileext = ".RData")
    obj1 <- data.frame(x = 1:3, y = 4:6)
    obj2 <- c("a", "b", "c")
    obj3 <- 42

    # Create PMData object and write
    data_obj <- pm::PMData$new(id = "test", path = rdata_file)
    data_obj$write(obj1, obj2, obj3 = obj3)

    # Read it back
    read_env <- data_obj$read()

    # Verify round-trip
    expect_type(read_env, "environment")
    expect_true("obj1" %in% ls(read_env))
    expect_true("obj2" %in% ls(read_env))
    expect_true("obj3" %in% ls(read_env))
    expect_equal(read_env$obj1, obj1)
    expect_equal(read_env$obj2, obj2)
    expect_equal(read_env$obj3, obj3)
  })

  it("Can write and read Parquet files through PMData when arrow is available", {
    if (!requireNamespace("arrow", quietly = TRUE)) {
      skip("arrow package not available")
    }

    parquet_file <- withr::local_tempfile(fileext = ".parquet")
    original_data <- data.frame(
      id = 1:5,
      value = rnorm(5),
      category = letters[1:5]
    )

    # Create PMData object and write
    data_obj <- pm::PMData$new(id = "test", path = parquet_file)
    data_obj$write(original_data)

    # Read it back
    read_data <- data_obj$read()

    # Verify round-trip
    expect_equal(nrow(read_data), nrow(original_data))
    expect_equal(ncol(read_data), ncol(original_data))
    expect_equal(read_data$id, original_data$id)
    expect_equal(read_data$category, original_data$category)
    expect_equal(read_data$value, original_data$value, tolerance = 1e-10)
  })

  it("Can read existing file, modify, and write back through PMData (read -> write)", {
    # First create a file using base R
    csv_file <- withr::local_tempfile(fileext = ".csv")
    original_data <- data.frame(
      x = 1:5,
      y = letters[1:5]
    )
    write.csv(original_data, csv_file, row.names = FALSE)

    # Read it through PMData
    data_obj <- pm::PMData$new(id = "test", path = csv_file)
    read_data <- data_obj$read()

    # Modify the data
    read_data$z <- read_data$x * 2

    # Write it back
    data_obj$write(read_data)

    # Read it again and verify
    final_data <- data_obj$read()
    expect_equal(ncol(final_data), 3)
    expect_equal(final_data$z, read_data$z)
    expect_equal(final_data$x, original_data$x)
    expect_equal(final_data$y, original_data$y)
  })

  it("Can do multiple round-trips through PMData", {
    rds_file <- withr::local_tempfile(fileext = ".rds")
    original_object <- list(iteration = 0, data = 1:5)

    # Create PMData object
    data_obj <- pm::PMData$new(id = "test", path = rds_file)

    # Do multiple write-read cycles
    for (i in 1:3) {
      original_object$iteration <- i
      data_obj$write(original_object)
      read_object <- data_obj$read()
      expect_equal(read_object$iteration, i)
      expect_equal(read_object$data, original_object$data)
    }
  })

  it("Can write and read .pqt files through PMData", {
    if (!requireNamespace("arrow", quietly = TRUE)) {
      skip("arrow package not available")
    }

    pqt_file <- withr::local_tempfile(fileext = ".pqt")
    original_data <- data.frame(
      x = letters[1:3],
      y = 1:3
    )

    # Create PMData object and write
    data_obj <- pm::PMData$new(id = "test", path = pqt_file)
    data_obj$write(original_data)

    # Read it back
    read_data <- data_obj$read()

    # Verify round-trip
    expect_equal(read_data$x, original_data$x)
    expect_equal(read_data$y, original_data$y)
  })

  it("Can write and read .rda files through PMData", {
    rda_file <- withr::local_tempfile(fileext = ".rda")
    obj1 <- data.frame(x = 1:2)
    obj2 <- list(a = 1, b = 2)

    # Create PMData object and write
    data_obj <- pm::PMData$new(id = "test", path = rda_file)
    data_obj$write(obj1, obj2)

    # Read it back
    read_env <- data_obj$read()

    # Verify round-trip
    expect_type(read_env, "environment")
    expect_true("obj1" %in% ls(read_env))
    expect_true("obj2" %in% ls(read_env))
    expect_equal(read_env$obj1, obj1)
    expect_equal(read_env$obj2, obj2)
  })

  it("Handles empty data.frames in round-trip", {
    csv_file <- withr::local_tempfile(fileext = ".csv")
    # Create empty data.frame with at least one column to avoid read issues
    empty_df <- data.frame(x = character(0))

    # Create PMData object and write
    data_obj <- pm::PMData$new(id = "test", path = csv_file)
    data_obj$write(empty_df)

    # Read it back
    read_data <- data_obj$read()

    # Verify round-trip
    expect_s3_class(read_data, "data.frame")
    expect_equal(nrow(read_data), 0)
    expect_equal(ncol(read_data), 1)
  })

  it("Handles data.frames with special characters in round-trip", {
    csv_file <- withr::local_tempfile(fileext = ".csv")
    special_data <- data.frame(
      text = c("quotes\"here", "new\nline", "tab\there", "comma,here"),
      numbers = 1:4
    )

    # Create PMData object and write
    data_obj <- pm::PMData$new(id = "test", path = csv_file)
    data_obj$write(special_data)

    # Read it back
    read_data <- data_obj$read()

    # Verify round-trip
    expect_equal(nrow(read_data), 4)
    expect_equal(read_data$text, special_data$text)
    expect_equal(read_data$numbers, special_data$numbers)
  })

  it("Handles data.frames with NA values in round-trip", {
    csv_file <- withr::local_tempfile(fileext = ".csv")
    na_data <- data.frame(
      x = c(1, NA, 3, NA, 5),
      y = c("a", "b", NA, "d", NA),
      z = c(TRUE, FALSE, NA, TRUE, FALSE)
    )

    # Create PMData object and write
    data_obj <- pm::PMData$new(id = "test", path = csv_file)
    data_obj$write(na_data)

    # Read it back
    read_data <- data_obj$read()

    # Verify round-trip (NA values should be preserved)
    expect_equal(nrow(read_data), 5)
    expect_equal(is.na(read_data$x), is.na(na_data$x))
    expect_equal(is.na(read_data$y), is.na(na_data$y))
    expect_equal(is.na(read_data$z), is.na(na_data$z))
    expect_equal(read_data$x[!is.na(read_data$x)], na_data$x[!is.na(na_data$x)])
  })

  it("Handles complex nested structures in RDS round-trip", {
    rds_file <- withr::local_tempfile(fileext = ".rds")
    complex_obj <- list(
      df = data.frame(a = 1:3, b = letters[1:3]),
      nested = list(
        level1 = list(level2 = list(level3 = "deep")),
        matrix = matrix(1:9, nrow = 3),
        vector = 1:10
      ),
      attributes = structure(1:5, names = letters[1:5], class = "custom")
    )

    # Create PMData object and write
    data_obj <- pm::PMData$new(id = "test", path = rds_file)
    data_obj$write(complex_obj)

    # Read it back
    read_obj <- data_obj$read()

    # Verify round-trip
    expect_equal(read_obj$df, complex_obj$df)
    expect_equal(read_obj$nested$level1, complex_obj$nested$level1)
    expect_equal(read_obj$nested$matrix, complex_obj$nested$matrix)
    expect_equal(read_obj$nested$vector, complex_obj$nested$vector)
    expect_equal(attributes(read_obj$attributes), attributes(complex_obj$attributes))
  })

  it("Handles case-insensitive extensions in round-trip", {
    csv_file <- withr::local_tempfile(fileext = ".CSV")
    original_data <- data.frame(x = 1:3, y = letters[1:3])

    # Create PMData object and write
    data_obj <- pm::PMData$new(id = "test", path = csv_file)
    data_obj$write(original_data)

    # Read it back
    read_data <- data_obj$read()

    # Verify round-trip
    expect_equal(read_data, original_data)
  })

  it("Can write, read, modify, and write again (full cycle)", {
    tsv_file <- withr::local_tempfile(fileext = ".tsv")
    original_data <- data.frame(
      id = 1:5,
      value = rnorm(5),
      status = c("A", "B", "A", "C", "B")
    )

    # Create PMData object
    data_obj <- pm::PMData$new(id = "test", path = tsv_file)

    # Write initial data
    data_obj$write(original_data)

    # Read and modify
    data1 <- data_obj$read()
    data1$new_col <- data1$id * 10
    data1$value <- data1$value * 2

    # Write modified data
    data_obj$write(data1)

    # Read again and verify
    data2 <- data_obj$read()
    expect_equal(ncol(data2), 4)  # original 3 + new_col
    expect_equal(data2$new_col, data1$new_col)
    expect_equal(data2$value, data1$value, tolerance = 1e-10)
    expect_equal(data2$status, original_data$status)
  })
})

