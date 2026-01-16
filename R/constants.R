constants <- list(
  INPUTS_FILENAME = "inputs.yaml",
  LOCAL_INPUTS_FILENAME = "inputs.local.yaml",
  ANALYSES_DIR = "analyses",
  README_FILENAME = "README.md",
  TEMPLATE_PROJECT_DIR = "template_project",
  TEMPLATE_ANALYSIS_DIR = "template_analysis",
  TEMPLATE_GITIGNORE_FILENAME = "template_gitignore",
  ANALYSIS_INTERMEDIATE_DIR = "intermediate",
  ANALYSIS_OUTPUT_DIR = "outputs",
  TYPE_MAPPINGS = list(
    defaults = list(
      table = "parquet",
      object = "rdata",
      image = "png",
      figure = "png"
    ),
    allowed = list(
      parquet = c("parquet", "pqt"),
      pqt = c("parquet", "pqt"),
      table = c("parquet", "pqt", "tsv", "csv", "rds", "rdata", "rda"),
      object = c("rdata", "rda", "rds"),
      image = c("png", "jpeg", "jpg", "svg", "gif", "tiff", "bmp"),
      figure = c("png", "jpeg", "jpg", "svg", "gif", "tiff", "bmp")
    )
  )
)

# Null coalescing operator
`%||%` <- function(x, y) if (is.null(x)) y else x