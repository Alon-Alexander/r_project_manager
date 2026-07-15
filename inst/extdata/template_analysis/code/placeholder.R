suppressPackageStartupMessages({
  library(pm)
})

ana <- pm_infer_analysis()

# Read project input
project_inputs <- ana$project$parse_inputs()
input_data <- project_inputs$my_input$read()

# Read artifact from another analysis
data <- ana$get_artifact("previous_results", analysis_name = NULL)

# Save output
ana$get_output_path("processed_data")$write(data)