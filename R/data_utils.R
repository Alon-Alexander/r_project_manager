#' @title Validate inputs.yaml schema
#'
#' @description
#' Validates that the inputs.yaml structure follows the expected schema.
#'
#' @param inputs_def List. Parsed YAML content from inputs.yaml
#'
#' @details
#' Expected schema:
#' - Top-level keys should start with "inputs." prefix
#' - Each input definition should have a "type" field
#' - Optional fields: "expected" (for validation), fingerprint fields (md5, size, etc.)
#'
#' @keywords internal
.validate_inputs_schema <- function(inputs_def) {
  if (!is.list(inputs_def)) {
    stop("inputs.yaml must be a YAML object (key-value pairs)")
  }

  # Check that we have at least one input definition
  input_keys <- names(inputs_def)[grepl("^inputs\\.", names(inputs_def))]
  if (length(input_keys) == 0) {
    stop("inputs.yaml must contain at least one key starting with 'inputs.'")
  }

  # Validate each input definition
  for (key in input_keys) {
    input_id <- sub("^inputs\\.", "", key)
    input_def <- inputs_def[[key]]

    if (!is.list(input_def)) {
      stop(sprintf("Input definition for '%s' must be a YAML object", key))
    }

    # Validate fingerprint fields if present (md5, size, etc.)
    fingerprint_fields <- c("md5", "size", "description")
    for (field in fingerprint_fields) {
      if (field %in% names(input_def)) {
        if (field %in% c("md5", "description")) {
          chk::chk_scalar(input_def[[field]])
          chk::chk_character(input_def[[field]], x_name = sprintf("%s for input '%s'", field, key))
        } else if (field == "size") {
          chk::chk_number(input_def[[field]], x_name = sprintf("Size for input '%s'", key))
        }
      }
    }
  }

  invisible(TRUE)
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
#' Inputs are defined with keys like "inputs.feature_table", and this function
#' extracts "feature_table" as the ID.
#'
#' @param inputs_def List. Parsed YAML content from inputs.yaml
#'
#' @return Character vector of input IDs
#'
#' @keywords internal
.extract_input_ids <- function(inputs_def) {
  input_keys <- names(inputs_def)[grepl("^inputs\\.", names(inputs_def))]
  input_ids <- sub("^inputs\\.", "", input_keys)
  input_ids
}

