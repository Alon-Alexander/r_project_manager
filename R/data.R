#' @title Data input class
#'
#' @description
#' Represents a single input data file with its ID and path.
#' This is a minimal representation that will be extended later
#' with reading/writing capabilities.
#'
#' @field id Character. The canonical identifier for this input (e.g., "feature_table")
#' @field path Character. The absolute path to the input file
#'
#' @examples
#' data <- PMData$new(id = "feature_table", path = "/path/to/file.biom")
#' data$id
#' data$path
#'
#' @importFrom R6 R6Class
#' @export
PMData <- R6Class("PMData",
  public = list(
    id = NULL,
    path = NULL,

    #' @description
    #' Create a PMData object
    #'
    #' @param id Character. The canonical identifier for this input
    #' @param path Character. The absolute path to the input file
    initialize = function(id, path) {
      chk::chk_scalar(id)
      chk::chk_character(id)
      chk::chk_scalar(path)
      chk::chk_character(path)
      self$id <- id
      self$path <- path
    },

    #' @description
    #' Print method for PMData
    print = function() {
      cat("PMData:\n")
      cat("  ID: ", self$id, "\n", sep = "")
      cat("  Path: ", self$path, "\n", sep = "")
      invisible(self)
    },

    #' @description
    #' Read the data file
    #'
    #' @param ... Additional arguments passed to \code{\link{pm_read_file}}
    #'
    #' @return The contents of the file. For RData files, returns a new environment
    #'   containing all objects from the file.
    #'
    #' @examples
    #' \dontrun{
    #' data <- PMData$new(id = "feature_table", path = "/path/to/file.csv")
    #' contents <- data$read()
    #' }
    read = function(...) {
      pm_read_file(self$path, ...)
    },

    #' @description
    #' Write data to the file
    #'
    #' @param x The object to write. For tabular formats (CSV, TSV, Parquet), must be
    #'   a data.frame. For RData files, can accept multiple objects via `...`.
    #' @param ... Additional arguments passed to \code{\link{pm_write_file}}.
    #'   For RData files, additional objects can be provided here.
    #'
    #' @return Invisibly returns the file path.
    #'
    #' @examples
    #' withr::with_tempdir({
    #'   data <- PMData$new(id = "feature_table", path = "file.csv")
    #'   df <- data.frame(x = 1:5, y = letters[1:5])
    #'   data$write(df)
    #'
    #'   # For RData files, can write multiple objects
    #'   data_rdata <- PMData$new(id = "results", path = "results.RData")
    #'   obj1 <- data.frame(x = 1:3)
    #'   obj2 <- c("a", "b", "c")
    #'   data_rdata$write(obj1, obj2, obj3 = 42)
    #' })
    write = function(x, ...) {
      # For RData files, we need to preserve object names from the original call
      ext <- tolower(tools::file_ext(self$path))
      if (ext %in% c("rdata", "rda")) {
        # Capture the call to extract object names
        call_obj <- match.call(expand.dots = FALSE)
        first_name <- deparse(call_obj$x)
        
        # Get ... arguments and their names
        dots_values <- list(...)
        dots_call <- call_obj$...
        dots_names <- names(dots_values)
        
        # Build object names vector
        obj_names <- character(0)
        obj_names <- c(obj_names, first_name)
        
        if (length(dots_values) > 0) {
          for (i in seq_along(dots_values)) {
            if (is.null(dots_names) || dots_names[i] == "") {
              # Unnamed argument, extract name from call
              obj_name <- deparse(dots_call[[i]])
            } else {
              # Named argument, use the name
              obj_name <- dots_names[i]
            }
            obj_names <- c(obj_names, obj_name)
          }
        }
        
        # Call pm_write_file with explicit object names
        pm_write_file(self$path, x, ..., object_names = obj_names)
      } else {
        # For other formats, just pass through
        pm_write_file(self$path, x, ...)
      }
    }
  )
)


