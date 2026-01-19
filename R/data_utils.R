#' @title Validate project.yaml schema
#'
#' @description
#' Validates that the project.yaml structure follows the expected schema.
#'
#' @param inputs_def List. Parsed YAML content from project.yaml
#'
#' @details
#' Expected schema:
#' - Must have a top-level "inputs" key
#' - "inputs" must be a YAML list (array) or object (named list)
#' - If array: each element can be a string (ID only) or an object with ID as key
#' - If object: maps input IDs to their definitions
#' - Each input definition can be an empty object ({}) or null (just an ID)
#' - Optional fields: "expected" (for validation), fingerprint fields (md5, size, etc.)
#'
#' @keywords internal
.validate_inputs_schema <- function(inputs_def) {
  if (!is.list(inputs_def)) {
    stop("project.yaml must be a YAML object (key-value pairs)")
  }

  if (!"inputs" %in% names(inputs_def)) {
    stop("project.yaml must have a top-level 'inputs' key")
  }

  inputs_list <- inputs_def$inputs

  # Handle YAML simplification: arrays are parsed as character vectors
  # Convert character vectors to lists for consistent processing
  if (is.character(inputs_list)) {
    if (length(inputs_list) == 1) {
      # Single-item: only convert if it looks like a valid identifier
      if (grepl("^[a-zA-Z_][a-zA-Z0-9_]*$", inputs_list)) {
        inputs_list <- list(inputs_list)
      }
    } else {
      # Multiple items: convert to list
      inputs_list <- as.list(inputs_list)
    }
  }

  if (!is.list(inputs_list)) {
    stop("'inputs' in project.yaml must be a YAML list or object")
  }

  if (length(inputs_list) == 0) {
    stop("'inputs' in project.yaml must contain at least one input definition")
  }

  # Check if it's an array (unnamed list) or object (named list)
  is_array <- is.null(names(inputs_list)) || all(names(inputs_list) == "")

  if (is_array) {
    # Array format: iterate through elements
    for (i in seq_along(inputs_list)) {
      element <- inputs_list[[i]]

      # Element can be:
      # 1. A string (just the ID)
      # 2. A list where one key maps to NULL and other keys are fields
      #    (This happens when YAML has "id:\n  field: value" structure)
      if (is.character(element) && length(element) == 1) {
        # Just an ID string - valid, no validation needed
        next
      } else if (is.list(element)) {
        element_names <- names(element)
        
        # Try standard object format first: single key with definition as value
        # This handles: list(input_id = list(...)) or list(input_id = NULL)
        if (length(element) == 1 && !is.null(element_names)) {
          input_id <- element_names[1]
          input_def <- element[[1]]

          # Allow NULL or empty list (just an ID with no fields)
          if (is.null(input_def)) {
            next
          }

          if (!is.list(input_def)) {
            stop(sprintf("Input definition for '%s' must be a YAML object or null", input_id))
          }

          # Validate fingerprint fields if present
          .validate_input_fields(input_def, input_id)
          next
        }
        
        # Check if this is the special case: one key with NULL value + other keys
        # This handles: list(id: NULL, field1: value1, field2: value2)
        if (!is.null(element_names)) {
          # Find keys with NULL values (these are the IDs)
          null_keys <- element_names[!sapply(element, function(x) !is.null(x))]
          other_keys <- setdiff(element_names, null_keys)

          if (length(null_keys) == 1 && length(other_keys) > 0) {
            # This is the format: id: NULL, field1: value1, field2: value2
            # The ID is the null key, and the definition is the other keys
            input_id <- null_keys[1]
            input_def <- element[other_keys]
            .validate_input_fields(input_def, input_id)
            next
          } else if (length(null_keys) == 1 && length(other_keys) == 0) {
            # Just an ID with no fields: id: NULL
            next
          } else if (length(null_keys) == 0 && length(other_keys) > 0) {
            # This shouldn't happen in array format, but handle it
            stop(sprintf("Input element at position %d has fields but no ID key", i))
          }
        }

        stop(sprintf("Input element at position %d must be a string (ID) or an object with ID as key", i))
      } else {
        stop(sprintf("Input element at position %d must be a string (ID) or an object with ID as key", i))
      }
    }
  } else {
    # Object format: named list mapping IDs to definitions
    for (input_id in names(inputs_list)) {
      input_def <- inputs_list[[input_id]]

      # Allow NULL or empty list (just an ID with no fields)
      if (is.null(input_def)) {
        next
      }

      if (!is.list(input_def)) {
        stop(sprintf("Input definition for '%s' must be a YAML object or null", input_id))
      }

      # Validate fingerprint fields if present
      .validate_input_fields(input_def, input_id)
    }
  }

  invisible(TRUE)
}

#' @title Validate input fields
#'
#' @description
#' Helper function to validate fingerprint fields in an input definition.
#'
#' @param input_def List. Input definition
#' @param input_id Character. Input ID for error messages
#'
#' @keywords internal
.validate_input_fields <- function(input_def, input_id) {
  fingerprint_fields <- c("md5", "size", "description")
  for (field in fingerprint_fields) {
    if (field %in% names(input_def)) {
      if (field %in% c("md5", "description")) {
        chk::chk_scalar(input_def[[field]])
        chk::chk_character(input_def[[field]], x_name = sprintf("%s for input '%s'", field, input_id))
      } else if (field == "size") {
        chk::chk_number(input_def[[field]], x_name = sprintf("Size for input '%s'", input_id))
      }
    }
  }
}

#' @title Validate inputs.local.yaml schema
#'
#' @description
#' Validates that the inputs.local.yaml structure follows the expected schema.
#'
#' @param local_inputs List. Parsed YAML content from inputs.local.yaml
#'
#' @details
#' Expected schema:
#' - Must have a top-level "paths" key
#' - "paths" must be a mapping from input IDs to file paths (strings)
#'
#' @keywords internal
.validate_local_inputs_schema <- function(local_inputs) {
  if (!is.list(local_inputs)) {
    stop("inputs.local.yaml must be a YAML object (key-value pairs)")
  }

  if (!"paths" %in% names(local_inputs)) {
    stop("inputs.local.yaml must have a top-level 'paths' key")
  }

  if (!is.list(local_inputs$paths)) {
    stop("'paths' in inputs.local.yaml must be a YAML object (key-value pairs)")
  }

  # Validate that all paths are strings
  for (id in names(local_inputs$paths)) {
    chk::chk_scalar(local_inputs$paths[[id]])
    chk::chk_character(local_inputs$paths[[id]], x_name = sprintf("Path for input '%s'", id))
  }

  invisible(TRUE)
}

#' @title Extract input IDs from project.yaml
#'
#' @description
#' Extracts the canonical input IDs from the project.yaml structure.
#' Supports both array format (list of strings/objects) and object format (named list).
#'
#' @param inputs_def List. Parsed YAML content from project.yaml
#'
#' @return Character vector of input IDs
#'
#' @keywords internal
.extract_input_ids <- function(inputs_def) {
  if (!"inputs" %in% names(inputs_def)) {
    return(character(0))
  }
  inputs_list <- inputs_def$inputs

  # Handle YAML simplification: arrays are parsed as character vectors
  # Convert character vectors to lists for consistent processing
  if (is.character(inputs_list)) {
    if (length(inputs_list) == 1) {
      # Single-item: only convert if it looks like a valid identifier
      if (grepl("^[a-zA-Z_][a-zA-Z0-9_]*$", inputs_list)) {
        inputs_list <- list(inputs_list)
      }
    } else {
      # Multiple items: convert to list
      inputs_list <- as.list(inputs_list)
    }
  }

  if (!is.list(inputs_list)) {
    return(character(0))
  }

  # Check if it's an array (unnamed list) or object (named list)
  is_array <- is.null(names(inputs_list)) || all(names(inputs_list) == "")

  if (is_array) {
    # Array format: extract IDs from each element
    input_ids <- character(0)
    for (i in seq_along(inputs_list)) {
      element <- inputs_list[[i]]

      if (is.character(element) && length(element) == 1) {
        # Just a string ID
        input_ids <- c(input_ids, element)
      } else if (is.list(element)) {
        element_names <- names(element)
        if (!is.null(element_names)) {
          # Check for the special case: one key with NULL value + other keys
          null_keys <- element_names[!sapply(element, function(x) !is.null(x))]
          other_keys <- setdiff(element_names, null_keys)

          if (length(null_keys) == 1) {
            # ID is the null key
            input_ids <- c(input_ids, null_keys[1])
          } else if (length(element) == 1) {
            # Standard object format: single key
            input_ids <- c(input_ids, element_names[1])
          }
        }
      }
    }
    input_ids
  } else {
    # Object format: use names as IDs
    names(inputs_list)
  }
}

#' @title Check for missing entries in inputs.local.yaml
#'
#' @description
#' Checks which input IDs from project.yaml are missing from inputs.local.yaml paths.
#'
#' @param input_ids Character vector. Input IDs from project.yaml
#' @param local_paths Named list. Paths from inputs.local.yaml
#'
#' @return Character vector of missing input IDs
#'
#' @keywords internal
.check_missing_entries <- function(input_ids, local_paths) {
  missing_entries <- character(0)
  for (id in input_ids) {
    if (!id %in% names(local_paths)) {
      missing_entries <- c(missing_entries, id)
    }
  }
  missing_entries
}

#' @title Check for missing files
#'
#' @description
#' Checks which files referenced in inputs.local.yaml do not exist.
#'
#' @param local_paths Named list. Paths from inputs.local.yaml (input ID -> path)
#' @param project_path Character. Path to the project directory
#'
#' @return List with components:
#'   - ids: Character vector of input IDs with missing files
#'   - abs_paths: Character vector of resolved absolute paths
#'   - original_paths: Character vector of original paths from YAML
#'
#' @keywords internal
.check_missing_files <- function(local_paths, project_path) {
  missing_ids <- character(0)
  missing_abs_paths <- character(0)
  missing_original_paths <- character(0)

  for (id in names(local_paths)) {
    path <- local_paths[[id]]
    original_path <- path
    
    # Convert to absolute path if relative
    if (!fs::is_absolute_path(path)) {
      abs_path <- normalizePath(file.path(project_path, path), mustWork = FALSE)
    } else {
      abs_path <- normalizePath(path, mustWork = FALSE)
    }

    if (!file.exists(abs_path)) {
      missing_ids <- c(missing_ids, id)
      missing_abs_paths <- c(missing_abs_paths, abs_path)
      missing_original_paths <- c(missing_original_paths, original_path)
    }
  }

  list(
    ids = missing_ids,
    abs_paths = missing_abs_paths,
    original_paths = missing_original_paths
  )
}

#' @title Format validation error message
#'
#' @description
#' Formats a helpful error message for input validation failures.
#'
#' @param missing_entries Character vector. Input IDs missing from inputs.local.yaml
#' @param missing_files_details List. Output from .check_missing_files()
#'
#' @return Character. Formatted error message
#'
#' @keywords internal
.format_validation_error <- function(missing_entries, missing_files_details) {
  error_parts <- character(0)

  if (length(missing_entries) > 0) {
    entry_plural <- if (length(missing_entries) == 1) "ID" else "IDs"
    error_parts <- c(
      error_parts,
      sprintf(
        "Input validation failed: %d input %s defined in project.yaml but missing from inputs.local.yaml:\n  %s",
        length(missing_entries),
        entry_plural,
        paste(sprintf("'%s'", missing_entries), collapse = ", ")
      ),
      "",
      "To fix this, add the following entries to inputs.local.yaml under the 'paths' key:",
      paste(sprintf("  %s: <path_to_your_file>", missing_entries), collapse = "\n"),
      "",
      sprintf(
        "Example:\n  paths:\n    %s: data/your_file.csv",
        missing_entries[1]
      )
    )
  }

  if (length(missing_files_details$ids) > 0) {
    file_details <- character(0)
    for (i in seq_along(missing_files_details$ids)) {
      # Show both original path (from YAML) and resolved absolute path
      if (missing_files_details$original_paths[i] != missing_files_details$abs_paths[i]) {
        file_details <- c(
          file_details,
          sprintf(
            "  - Input ID '%s':\n    Path in inputs.local.yaml: '%s'\n    Resolved to: '%s'\n    Status: File not found",
            missing_files_details$ids[i],
            missing_files_details$original_paths[i],
            missing_files_details$abs_paths[i]
          )
        )
      } else {
        file_details <- c(
          file_details,
          sprintf(
            "  - Input ID '%s':\n    Path: '%s'\n    Status: File not found",
            missing_files_details$ids[i],
            missing_files_details$abs_paths[i]
          )
        )
      }
    }
    
    file_plural <- if (length(missing_files_details$ids) == 1) "file" else "files"
    error_parts <- c(
      error_parts,
      sprintf(
        "Input validation failed: %d input %s referenced in inputs.local.yaml do not exist:",
        length(missing_files_details$ids),
        file_plural
      ),
      "",
      paste(file_details, collapse = "\n\n"),
      "",
      "To fix this, you can either:",
      "  1. Update the path in inputs.local.yaml to point to the correct file location",
      "  2. Create the file at the specified path",
      "",
      "Note: If using relative paths, they are resolved relative to the project directory."
    )
  }

  paste(error_parts, collapse = "\n")
}

#' @title Validate input files exist
#'
#' @description
#' Validates that all input files referenced in inputs.local.yaml actually exist.
#' Also checks that all inputs defined in project.yaml have corresponding entries
#' in inputs.local.yaml.
#'
#' @param project_path Character. Path to the project directory
#' @param configuration_file Character. Path to project.yaml file
#' @param local_inputs_file Character. Path to inputs.local.yaml file
#'
#' @details
#' This function:
#' - Reads and validates the schema of both YAML files
#' - Checks that all input IDs from project.yaml have entries in inputs.local.yaml
#' - Checks that all files referenced in inputs.local.yaml exist
#' - Provides helpful error messages with guidance on how to fix issues
#'
#' @return Invisibly returns TRUE if validation passes
#'
#' @keywords internal
.validate_input_files <- function(project_path, configuration_file, local_inputs_file) {
  # Read project.yaml
  inputs_def <- tryCatch(
    yaml::read_yaml(configuration_file),
    error = function(e) {
      stop("Failed to read and parse project.yaml: ", conditionMessage(e))
    }
  )

  # If project.yaml is empty or NULL, skip validation
  if (is.null(inputs_def) || length(inputs_def) == 0) {
    return(invisible(TRUE))
  }

  # Validate project.yaml schema
  .validate_inputs_schema(inputs_def)

  # Read inputs.local.yaml
  local_inputs <- tryCatch(
    yaml::read_yaml(local_inputs_file),
    error = function(e) {
      stop("Failed to read and parse inputs.local.yaml: ", conditionMessage(e))
    }
  )

  # Extract input IDs from project.yaml
  input_ids <- .extract_input_ids(inputs_def)
  
  # If no inputs are defined, skip file validation
  if (length(input_ids) == 0) {
    return(invisible(TRUE))
  }

  # Handle inputs.local.yaml - validate schema if present
  if (is.null(local_inputs) || length(local_inputs) == 0 || !is.list(local_inputs)) {
    local_inputs <- list(paths = list())
  } else {
    .validate_local_inputs_schema(local_inputs)
  }

  # Ensure paths key exists
  if (!"paths" %in% names(local_inputs) || is.null(local_inputs$paths)) {
    local_inputs$paths <- list()
  }

  # Check for missing entries and files
  missing_entries <- .check_missing_entries(input_ids, local_inputs$paths)
  missing_files_details <- .check_missing_files(local_inputs$paths, project_path)

  # If there are any errors, format and throw error
  if (length(missing_entries) > 0 || length(missing_files_details$ids) > 0) {
    error_message <- .format_validation_error(missing_entries, missing_files_details)
    stop(error_message)
  }

  invisible(TRUE)
}

#' @title Read a file based on its extension
#'
#' @description
#' Reads a file based on its file extension. Supports multiple file formats
#' including tabular data (CSV, TSV), serialized R objects (RDS), and
#' R data files (RData/RData).
#'
#' @param file Character. Path to the file to read
#' @param ... Additional arguments passed to the underlying read function
#'   (e.g., `header`, `sep`, `stringsAsFactors` for CSV/TSV files)
#'
#' @return The contents of the file. For RData files, returns a new environment
#'   containing all objects from the file.
#'
#' @details
#' Supported file formats:
#' - **CSV**: Comma-separated values files (`.csv`) - uses `read.csv()`
#' - **TSV**: Tab-separated values files (`.tsv`) - uses `read.table()` with `sep = "\t"`
#' - **Parquet**: Apache Parquet files (`.parquet`, `.pqt`) - requires the `arrow` package
#' - **RDS**: R serialized data files (`.rds`) - uses `readRDS()`
#' - **RData/RData/Rda**: R data files (`.rdata`, `.RData`, `.rda`, `.Rda`) - uses `load()` into a new environment
#'
#' For RData files, all objects are loaded into a new environment which is returned.
#' You can access objects using `env$object_name` or `as.list(env)`.
#'
#' @examples
#' \dontrun{
#' # Read a CSV file
#' data <- pm_read_file("data.csv")
#'
#' # Read a TSV file
#' data <- pm_read_file("data.tsv")
#'
#' # Read an RDS file
#' obj <- pm_read_file("data.rds")
#'
#' # Read an RData file (returns environment)
#' env <- pm_read_file("data.RData")
#' my_object <- env$my_object
#' }
#'
#' @importFrom utils read.csv read.table
#' @export
pm_read_file <- function(file, ...) {
  chk::chk_scalar(file)
  chk::chk_character(file)
  chk::check_files(file, x_name = "File")

  # Get file extension (case-insensitive)
  ext <- tolower(tools::file_ext(file))

  # Read based on extension
  switch(ext,
    csv = {
      read.csv(file, ...)
    },
    tsv = {
      read.table(file, sep = "\t", header = TRUE, ...)
    },
    parquet =,
    pqt = {
      if (!requireNamespace("arrow", quietly = TRUE)) {
        stop("Reading Parquet files requires the 'arrow' package. ",
             "Install it with: install.packages('arrow')")
      }
      arrow::read_parquet(file, ...)
    },
    rds = {
      readRDS(file, ...)
    },
    rdata =,
    rda = {
      # Load into a new environment
      env <- new.env(parent = emptyenv())
      load(file, envir = env, ...)
      env
    },
    stop(sprintf("Unsupported file extension: .%s. Supported formats: csv, tsv, parquet, pqt, rds, rdata, rda", ext))
  )
}

#' @title Write an object to a file based on its extension
#'
#' @description
#' Writes an object to a file based on the file extension. Supports multiple file formats
#' including tabular data (CSV, TSV), serialized R objects (RDS), and
#' R data files (RData/RData).
#'
#' @param file Character. Path to the file to write
#' @param x The object to write. For tabular formats (CSV, TSV, Parquet), must be
#'   a data.frame or similar tabular object. For RData files, can accept multiple
#'   objects via `...`.
#' @param ... Additional arguments passed to the underlying write function.
#'   For RData files, additional objects can be provided here.
#' @param object_names Character vector. Optional. **Internal parameter.** For RData files, explicitly
#'   specify the names for objects. If provided, must match the number of objects
#'   (x + objects in ...). If not provided, names are extracted from the call.
#'   This parameter is intended for use when forwarding a `...` call from a parent frame
#'   where object names need to be preserved. See `PMData$write()` for example usage.
#'
#' @return Invisibly returns the file path.
#'
#' @details
#' Supported file formats:
#' - **CSV**: Comma-separated values files (`.csv`) - uses `write.csv()`. Requires a data.frame.
#' - **TSV**: Tab-separated values files (`.tsv`) - uses `write.table()` with `sep = "\t"`. Requires a data.frame.
#' - **Parquet**: Apache Parquet files (`.parquet`, `.pqt`) - requires the `arrow` package. Requires a data.frame.
#' - **RDS**: R serialized data files (`.rds`) - uses `saveRDS()`. Accepts any R object.
#' - **RData/RData/Rda**: R data files (`.rdata`, `.RData`, `.rda`, `.Rda`) - uses `save()`.
#'   Can accept multiple objects: `pm_write_file("file.RData", obj1, obj2, obj3)`.
#'
#' For tabular formats (CSV, TSV, Parquet), the function validates that the object
#' is a data.frame or similar tabular structure.
#'
#' The `object_names` parameter is an internal parameter used when forwarding `...` calls
#' from a parent frame. When a wrapper function (like `PMData$write()`) needs to preserve
#' object names from the original call, it can capture the names and pass them via
#' `object_names`. This ensures that RData files contain objects with the correct names
#' even when called through wrapper functions. See the implementation of `PMData$write()`
#' for an example of how to use this parameter.
#'
#' @examples
#' # Example usage:
#' withr::with_tempdir({
#'
#' # Write a data.frame to CSV
#' df <- data.frame(x = 1:5, y = letters[1:5])
#' pm_write_file("data.csv", df)
#'
#' # Write a data.frame to TSV
#' pm_write_file("data.tsv", df)
#'
#' # Write any object to RDS
#' my_list <- list(a = 1, b = 2)
#' pm_write_file("data.rds", my_list)
#'
#' # Write multiple objects to RData
#' obj1 <- data.frame(x = 1:3)
#' obj2 <- c("a", "b", "c")
#' pm_write_file("data.RData", obj1, obj2, obj3 = 42)
#' 
#' })
#'
#' @importFrom utils write.csv write.table
#' @export
pm_write_file <- function(file, x, ..., object_names = NULL) {
  chk::chk_scalar(file)
  chk::chk_character(file)

  # Get file extension (case-insensitive)
  ext <- tolower(tools::file_ext(file))

  # Write based on extension
  switch(ext,
    csv = {
      if (!is.data.frame(x)) {
        stop("For CSV files, 'x' must be a data.frame")
      }
      write.csv(x, file, row.names = FALSE, ...)
    },
    tsv = {
      if (!is.data.frame(x)) {
        stop("For TSV files, 'x' must be a data.frame")
      }
      write.table(x, file, sep = "\t", row.names = FALSE, quote = FALSE, ...)
    },
    parquet =,
    pqt = {
      if (!is.data.frame(x)) {
        stop("For Parquet files, 'x' must be a data.frame")
      }
      if (!requireNamespace("arrow", quietly = TRUE)) {
        stop("Writing Parquet files requires the 'arrow' package. ",
             "Install it with: install.packages('arrow')")
      }
      arrow::write_parquet(x, file, ...)
    },
    rds = {
      saveRDS(x, file, ...)
    },
    rdata =,
    rda = {
      # For RData files, collect all objects from x and ...
      dots_values <- list(...)

      # Use provided object_names if available, otherwise extract from call
      if (!is.null(object_names)) {
        # Use provided names
        if (length(object_names) != 1 + length(dots_values)) {
          stop("object_names must have length equal to number of objects (x + objects in ...)")
        }
        obj_names <- object_names
        obj_values <- c(list(x), dots_values)
      } else {
        # Extract names from call
        call_obj <- match.call(expand.dots = FALSE)
        first_name <- deparse(call_obj$x)
        dots_call <- call_obj$...
        
        # Collect all object names and values
        obj_names <- character(0)
        obj_values <- list()

        # Add the first object
        obj_names <- c(obj_names, first_name)
        obj_values <- c(obj_values, list(x))

        # Handle additional objects from ...
        if (length(dots_values) > 0) {
          dots_names <- names(dots_values)

          for (i in seq_along(dots_values)) {
            if (is.null(dots_names) || dots_names[i] == "") {
              # Unnamed argument, extract name from the call
              if (is.pairlist(dots_call)) {
                obj_name <- deparse(dots_call[[i]])
              } else if (is.call(dots_call) && dots_call[[1]] == as.name("list")) {
                # If ... was expanded, it might be a list call
                obj_name <- deparse(dots_call[[i + 1]])
              } else if (!is.null(dots_call)) {
                # Fallback: try to get name from the call
                obj_name <- deparse(dots_call[[i]])
              } else {
                # Last resort: use a default name
                obj_name <- paste0("obj", i)
              }
            } else {
              # Named argument, use the name
              obj_name <- dots_names[i]
            }
            obj_names <- c(obj_names, obj_name)
            obj_values <- c(obj_values, list(dots_values[[i]]))
          }
        }
      }

      # Create a temporary environment to store objects
      env <- new.env(parent = emptyenv())
      for (i in seq_along(obj_names)) {
        assign(obj_names[i], obj_values[[i]], envir = env)
      }

      # Save all objects
      # Note: ... might contain arguments for save(), but we'll ignore them
      # since we're using do.call with explicit arguments
      save(list = obj_names, file = file, envir = env)
    },
    stop(sprintf("Unsupported file extension: .%s. Supported formats: csv, tsv, parquet, pqt, rds, rdata, rda", ext))
  )

  invisible(file)
}

