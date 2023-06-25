#' Compare QC and RTF Read-in Data Frames
#'
#' Tool used to compare QC data frame with RTF read-in data frame, and output comparison results to HTML file   
#' @param qc the name of data frame you create in QC
#' @param rtf the name of data frame read in from RTF
#' @param path the folder path of HTML output file. By default the path is the working directory, getwd().
#' @param filename output HTML file name, which is typically tableid
#' @param by A character vector of a string of character vector showing the columns by which to group_by. Defaults to row sequence
#' @param exclude The columns which should be excluded from the comparison
#' @param tolerance The amount in fraction to which changes are ignored while showing the visual representation. By default, the value is 0 and any change in the value of variables is shown off. Doesn't apply to categorical variables.
#' @param tolerance_type Defaults to 'ratio'. The type of comparison for numeric values, can be 'ratio' or 'difference'
#' @param stop_on_error Whether to stop on acceptable errors on not. Defaults to FALSE
#' @param keep_unchanged_rows whether to preserve unchanged values or not. Defaults to FALSE
#' @param keep_unchanged_cols whether to preserve unchanged values or not. Defaults to TRUE
#' @param round_output_to Number of digits to round the output to. Defaults to 3.
#' @return the output of comparison in HTML
#' @examples
#' \dontrun{
#' qc_comparedf(qc = tab_qc, rtf = tab_rtf, path = qc[["PDEV"]], filename = "tsidem01")
#' }
#' @export
#' @importFrom compareDF compare_df create_output_table
qc_comparedf <- function(qc, rtf, path = ".", filename = NULL, by = "row_seq", exclude = NULL, tolerance = 0, tolerance_type = "ratio", 
                         stop_on_error = FALSE, keep_unchanged_rows = FALSE, keep_unchanged_cols = TRUE, round_output_to = 3) {
  
  assertthat::is.dir(path)
  assertthat::assert_that(length(qc) == length(rtf))
  
  # Get rid of the leading and trailing whitespace from the character string
  rtfdf <- as.data.frame(sapply(rtf, trimws))
  qcdf <- as.data.frame(sapply(qc, trimws))
  colnames(qcdf) <- colnames(rtfdf)
  
  row.names(rtfdf) <- NULL
  row.names(qcdf) <- NULL
  
  # Add row sequence
  rtfdf1 <- rtfdf %>%
    mutate(row_seq = row_number())
  qcdf1 <- qcdf %>%
    mutate(row_seq = row_number())
  
  result <- suppressWarnings(compareDF::compare_df(df_new = rtfdf1, 
                                                   df_old = qcdf1, 
                                                   group_col = by, 
                                                   exclude = exclude, 
                                                   tolerance = tolerance,
                                                   tolerance_type = tolerance_type,
                                                   stop_on_error = stop_on_error,
                                                   keep_unchanged_rows = keep_unchanged_rows, 
                                                   keep_unchanged_cols = keep_unchanged_cols,
                                                   change_markers = c("PROD", "QC"),
                                                   round_output_to = round_output_to))
  
  ### if there is a difference detected
  if (result$change_summary["changes"] != 0){
    
    # display result in viewer
    temp_dir = tempdir()
    temp_file <- file.path(temp_dir, paste0("qc", filename, ".html"))
    
    # diverts output to a temp file
    sink(temp_file, append = TRUE)
    
    cat(sprintf("<h3>Comparison Results for %s</h3>", filename),
        compareDF::create_output_table(result), file = temp_file)
    
    # Use RStudio viewer if available, otherwise open in a web browser
    if (!is.null(getOption("viewer"))) {
      rstudioapi::viewer(temp_file)
    } else {
      browseURL(temp_file)
    }
    
    # diverts output back to console
    sink()
    
    # Save comparison results in html
    file_path <- file.path(path, paste0("qc", filename, ".html"))
    cat(sprintf("<h3>Comparison Results for %s</h3>", filename),
        compareDF::create_output_table(result), file = file_path)

  } else {
    message("The two data frames are the same after accounting for tolerance!")
  }
  
  # Store comparison result to result_temp for batchrun
  check_final <- ifelse(result$change_summary["changes"] == 0, "Yes", "No")
  
  if (check_final == "Yes"){
    result_temp <<- paste0("<tr>\n", sprintf("<td>%s</td>", filename), "\n",
                           sprintf("<td>%s</td>", check_final), "\n</tr>")
  } else {
    result_temp <<- paste0("<tr>\n", 
                           sprintf(paste0("<td><a href='", paste0("qc", filename, ".html"), "' target='_blank'>%s</a></td>"), filename), 
                           "\n",
                           sprintf("<td>%s</td>", check_final), 
                           "\n</tr>")
  }
}

