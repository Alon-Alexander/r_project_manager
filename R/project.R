#' @importFrom R6 R6Class
#' @export
PMProject <- R6Class("PMProject",
  public = list(
    path = NULL,
    initialize = function(path) {
      path <- normalizePath(path)
      chk::check_dirs(path)
      self$path <- path

      self$validate()
    },
    validate = function() {
      chk::check_files(
        private$at(constants$INPUTS_FILENAME),
        x_name = "Inputs definition file"
      )
      chk::check_files(
        private$at(constants$LOCAL_INPUTS_FILENAME),
        x_name = "Local inputs mapping file"
      )
      chk::check_files(
        private$at(constants$README_FILENAME),
        x_name = "Readme file"
      )
      chk::check_dirs(
        private$at(constants$ANALYSES_DIR),
        x_name = "Analyses folder"
      )
    }
  ),
  private = list(
    at = function(...) {
      file.path(self$path, ...)
    }
  )
)
