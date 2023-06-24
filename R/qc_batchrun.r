#' Batch Run QC Scripts
#'
#' Execute R qc scripts by batch with log
#' @param files a character vector of full/relative path names 
#' @param path the folder path of HTML output file. By default the path is the working directory, getwd().
#'
#' @return A summary table of comparison results will display in html
#' @export
#' @examples
#' \dontrun{
#' ### select all r scripts start with 'qct' on SPACE
#' qc_files <- list.files(path = qc[["PDEV"]], pattern = "qct.*\\.r", 
#'                        full.names = TRUE)
#' 
#' qc_batchrun(files = qc_files, path = qc[["PDEV"]])
#' }
#' @importFrom logrx axecute
#' @importFrom rstudioapi viewer
#' @importFrom utils browseURL
qc_batchrun <- function(files, path = "."){
  
  assertthat::assert_that(all(file.exists(files)))
  assertthat::is.dir(path)
  
  ### source all r scripts in files
  source_batch <- function(files) {
    op <- options(log.rx = NULL); on.exit(options(op)) # to reset after each
    axecute(
      files,
      log_name = str_replace(basename(i), ".r|.R", ".log"),
      log_path = path,
      remove_log_object = TRUE,
      quit_on_error = FALSE,
      to_report = c("messages", "output", "results")
    )
  }
  
  temp_result = ""
  for (i in files) {
    # Create a message for debugging
    message(paste("Running", i))
    
    # Source for each
    source_batch(i)
    
    # Combine result_temp
    temp_result <- c(temp_result, result_temp)
  }
  
  ### Output comparison result to viewer
  # Save compare result html to path
  filename <- paste0("compare_results_", as.character(Sys.Date()), ".html")
  file_path <- file.path(path, filename)
  
  if (interactive()){
    
    ### Display result in viewer
    # create temp file
    # temp_dir = tempdir()
    # temp_file <- paste0(temp_dir, "/temp.html")
    
    # diverts output to a temp file
    sink(file_path, append = TRUE)
    
    cat("<h2>Comparison Results</h2>", "<table border='1'>", "<tr>", 
        "<th>Table id</th>", "<th>Results match</th>", "</tr>", 
        temp_result, "</table>", file = file_path)
    
    # Use RStudio viewer if available, otherwise open in a web browser
    if (!is.null(getOption("viewer"))) {
      rstudioapi::viewer(file_path)
    } else {
      browseURL(file_path)
    }
    
    # diverts output back to console
    sink()
    
  } else {
    
    ### Create compare_results.html
    cat("<h2>Comparison Results</h2>", "<table border='1'>", "<tr>", 
        "<th>Table id</th>", "<th>Results match</th>", "</tr>", 
        temp_result, "</table>", file = file_path)
  }
}


