#' Batch Run QC Scripts
#'
#' Execute R qc scripts end with \code{\link{qc_compare2xlsx}}
#' @param files one or more filenames 
#' @param path the path where temp1.txt is stored. Default is qc
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
  
  ### Delete temp1.txt if it exists
  if (file.exists(file.path(envsetup::write_path(path), "temp1.txt"))) {
    unlink(envsetup::read_path(qc, "temp1.txt"))
  }
  
  sapply(files, source)
  
  ### Output comparison result to viewer
  # Create temp directory and files
  tempDir <- tempfile(); dir.create(tempDir)
  temp_file <- file.path(tempDir, "temp.html")
  temp_result <- file.path(envsetup::write_path(qc), "temp1.txt")
  
  # diverts output to a temp file
  sink(temp_file, append = TRUE)
  cat("<h2>Comparison Results</h2>")
  cat("<table border='1'>")
  cat("<tr>")
  cat("<th>Table id</th>")
  cat("<th>Results match</th>")
  cat("</tr>")
  cat(readLines(temp_result, warn = F))
  cat("</table>")
  
  # vie file in Viewer window
  viewer <- getOption("viewer")
  rstudioapi::viewer(temp_file)
  # diverts output back to console
  sink()
  
  unlink(temp_result)
  
}

