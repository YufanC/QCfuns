#' Batch Run QC Scripts
#'
#' Execute R qc scripts end with \code{\link{qc_compare2xlsx}}
#' @param files one or more filenames 
#' @param path the path where batchrun result compare_results.html stores. Default is qc
#'
#' @return A summary table of comparison results will display in Viewer
#' @export
#'
#' @examplesIf exists("rptdrv")
#' ### select all r scripts start with 'qct' on SPACE
#' qc_files <- list.files(path = read_path(rptdrv, "qc"), pattern = "qct.*\\.r", 
#'                        full.names = TRUE)
#' 
#' qc_batchrun(qc_files)
qc_batchrun <- function(files, path = qc){
  
  assertthat::assert_that(all(file.exists(files)))
  assertthat::is.dir(envsetup::write_path(path))
  
  ### source all r scripts in files
  source_batch <- function(files) {
    op <- options(); on.exit(options(op)) # to reset after each 
    result_combine = ""
    for (i in files) {
      # Create a message for debugging
      message(paste("Running", i))
      source(i)
      # Combine result_temp
      result_combine <- c(result_combine, result_temp)
      options(op)
    }
    return(result_combine)
  }
  
  temp_result <- source_batch(files)
  
  if (interactive()){
    ### Output comparison result to viewer
    # Create temp directory and files
    tempDir <- tempfile(); dir.create(tempDir)
    temp_file <- file.path(tempDir, "temp.html")
    
    # diverts output to a temp file
    sink(temp_file, append = TRUE)
    cat("<h2>Comparison Results</h2>")
    cat("<table border='1'>")
    cat("<tr>")
    cat("<th>Table id</th>")
    cat("<th>Results match</th>")
    cat("</tr>")
    cat(temp_result)
    cat("</table>")
    
    # vie file in Viewer window
    viewer <- getOption("viewer")
    rstudioapi::viewer(temp_file)
    
    # diverts output back to console
    sink()
    
  } else {
    
    ### Delete html_output.txt if it exists
    if (file.exists(file.path(envsetup::write_path(path), "html_output.txt"))) {
      unlink(envsetup::read_path(path, "html_output.txt"))
    }
    
    ### Create compare_results.html when run not interactively
    cat("<h2>Comparison Results</h2>", "<table border='1'>", "<tr>", 
        "<th>Table id</th>", "<th>Results match</th>", "</tr>", 
        temp_result, "</table>",
        file = file.path(write_path(path), "html_output.txt"), sep = "")
    
    html_output <- file.path(envsetup::read_path(path, "html_output.txt"))
    
    knitr::knit(html_output, output = "compare_results.html")
    
    unlink(html_output)
  }
  
}


