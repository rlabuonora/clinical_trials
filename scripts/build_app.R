concatenate_scripts <- function(input_files, output_file) {
  # Check if input_files is a character vector
  if (!is.character(input_files)) {
    stop("input_files must be a character vector of file paths.")
  }
  
  # Check if files exist
  missing_files <- input_files[!file.exists(input_files)]
  if (length(missing_files) > 0) {
    stop("The following files do not exist: ", paste(missing_files, collapse = ", "))
  }
  
  # Open the output file for writing
  con <- file(output_file, open = "w")
  
  # Iterate over each input file
  for (file in input_files) {
    # Read the contents of the file
    script <- readLines(file, warn = FALSE)
    
    # Add a header to indicate the source file
    header <- paste0("# ---- Start of ", basename(file), " ----")
    footer <- paste0("# ---- End of ", basename(file), " ----")
    
    # Write the header, script, and footer to the output file
    writeLines(c(header, script, footer, ""), con)
  }
  
  # Close the output file connection
  close(con)
  
  message("Scripts successfully concatenated into ", output_file)
}

# List of R scripts to concatenate
input_files <- c("global.R", "R/get_studies.R", "app.R")

# Output file
output_file <- "../app.R"

# Concatenate the scripts
concatenate_scripts(input_files, output_file)
