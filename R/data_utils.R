#' @title Validate inputs.yaml schema
#'
#' @description
#' Validates that the inputs.yaml structure follows the expected schema.
#'
#' @param inputs_def List. Parsed YAML content from inputs.yaml
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
    stop("inputs.yaml must be a YAML object (key-value pairs)")
  }

  if (!"inputs" %in% names(inputs_def)) {
    stop("inputs.yaml must have a top-level 'inputs' key")
  }

  inputs_list <- inputs_def$inputs

  # Handle case where single-item list is parsed as the value itself
  # Only convert if it looks like a valid identifier (alphanumeric + underscore, no spaces)
  if (is.character(inputs_list) && length(inputs_list) == 1) {
    # Check if it's a valid identifier (simple ID, not a descriptive string)
    if (grepl("^[a-zA-Z_][a-zA-Z0-9_]*$", inputs_list)) {
      # Single string ID - convert to list format
      inputs_list <- list(inputs_list)
    } else {
      # Not a valid ID format, treat as error
      stop("'inputs' in inputs.yaml must be a YAML list or object")
    }
  }

  if (!is.list(inputs_list)) {
    stop("'inputs' in inputs.yaml must be a YAML list or object")
  }

  if (length(inputs_list) == 0) {
    stop("'inputs' in inputs.yaml must contain at least one input definition")
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
        # Check if this is the special case: one key with NULL value + other keys
        element_names <- names(element)
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

        # Try standard object format: single key with definition as value
        if (length(element) == 1 && !is.null(names(element))) {
          input_id <- names(element)[1]
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

#' @title Extract input IDs from inputs.yaml
#'
#' @description
#' Extracts the canonical input IDs from the inputs.yaml structure.
#' Supports both array format (list of strings/objects) and object format (named list).
#'
#' @param inputs_def List. Parsed YAML content from inputs.yaml
#'
#' @return Character vector of input IDs
#'
#' @keywords internal
.extract_input_ids <- function(inputs_def) {
  if (!"inputs" %in% names(inputs_def)) {
    return(character(0))
  }
  inputs_list <- inputs_def$inputs

  # Handle case where single-item list is parsed as the value itself
  if (is.character(inputs_list) && length(inputs_list) == 1) {
    # Single string - convert to list format
    inputs_list <- list(inputs_list)
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

