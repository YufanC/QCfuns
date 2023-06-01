#' Batch Run QC Scripts
#'
#' Execute R qc scripts end with \code{\link{qc_compare2xlsx}}
#' @param files a character vector of full/relative path names 
#' @param path the folder path of html output file, in which a 'result' subfolder will be created to hold all compare results. The default corresponds to the working directory, getwd().
#'
#' @return A summary table of comparison results will display in Viewer
#' @export
#' @examples
#' \dontrun{
#' ### select all r scripts start with 'qct' on SPACE
#' qc_files <- list.files(path = qc[["PDEV"]], pattern = "qct.*\\.r", 
#'                        full.names = TRUE)
#' 
#' qc_batchrun(files = qc_files, path = qc[["PDEV"]])
#' }
#' @importFrom rstudioapi viewer
#' @importFrom knitr knit
qc_batchrun <- function(files, path = "."){
  
  assertthat::assert_that(all(file.exists(files)))
  assertthat::is.dir(path)
  
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
    # Save compare result html to results folder
    result_path <- file.path(path, "results")
    # Create 'results' subfolder if it does not exist
    if (is.na(file.info(result_path)$isdir)|file.info(result_path)$isdir == FALSE) dir.create(result_path)
    
    filename <- paste0("compare_results_", as.character(Sys.Date()), ".html")
    file_path <- file.path(result_path, filename)
    # Remove output html if it has the same name
    if (file.exists(file_path)) {
      unlink(file_path)
    }
    temp_file <- file_path
    
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
    if (file.exists(file.path(path, "html_output.txt"))) {
      unlink(file.path(path, "html_output.txt"))
    }
    
    ### Create compare_results.html when run not interactively
    cat("<h2>Comparison Results</h2>", "<table border='1'>", "<tr>", 
        "<th>Table id</th>", "<th>Results match</th>", "</tr>", 
        temp_result, "</table>",
        file = file.path(path, "html_output.txt"), sep = "")
    
    html_output <- file.path(path, "html_output.txt")
    
    filename <- paste0("compare_results_", as.character(Sys.Date()), ".html")
    file_path <- file.path(path, "results", filename)
    knitr::knit(html_output, output = file_path)
    
    unlink(html_output)
  }
  
}


