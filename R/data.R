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
    }
  )
)


