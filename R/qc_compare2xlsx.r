#' Compare QC and RTF Read-in Data Frames
#'
#' Tool used to compare QC data frame with RTF read-in data frame, and output comparison results to XLSX file   
#' @param qc the name of data frame you create in QC
#' @param rtf the name of data frame read in from RTF
#' @param path the path to save XLSX output file folder
#' @param filename output XLSX file name, which is typically tableid
#' @param version whether to add time stamp in file name. Default is TRUE to add the current date in file name
#' @param max_diff maximum number of differences to report. NA will print all differences overall with the constraint of max_diff_per_var
#' @param max_diff_per_var maximum number of differences per variable to report. NA will print all differences for each variable with the constraint of max_diff
#' @return the output of comparison in XLSX
#' @examples
#' \dontrun{
#' qc_compare2xlsx(qc = tab_qc, rtf = tab_rtf, path = qc, filename = "tableid")
#' }
#' @export
#' @import openxlsx
#' @importFrom arsenal comparedf
qc_compare2xlsx <- function(qc, rtf, path = NULL, filename = NULL, max_diff = 50, max_diff_per_var = 10, version = TRUE) {
  
  assertthat::is.dir(path)
  
  # Check whether the number of columns matches
  if (length(qc) != length(rtf)){
    stop("qc and rtf dataframes have different columns")
  } else {
    # Get rid of the leading and trailing whitespace from the character string
    rtfdf <- as.data.frame(sapply(rtf, trimws))
    qcdf <- as.data.frame(sapply(qc, trimws))
    colnames(qcdf) <- colnames(rtfdf)
    
    row.names(rtfdf) <- NULL
    row.names(qcdf) <- NULL
    
    # Assign the two datasets to global environment
    assign("rtfdf", rtfdf, envir = .GlobalEnv)
    assign("qcdf", qcdf, envir = .GlobalEnv)
    
    result <- summary(comparedf(rtfdf, qcdf))[c(1, 7)]
    result$diffs.table$values.x <- unlist(result$diffs.table$values.x)
    result$diffs.table$values.y <- unlist(result$diffs.table$values.y)
    
    ### Store comparison result to result_temp for batchrun 
    check1 <- result$frame.summary.table$ncol[1] == result$frame.summary.table$ncol[2]
    check2 <- result$frame.summary.table$nrow[1] == result$frame.summary.table$nrow[2]
    check3 <- nrow(result$diffs.table) == 0
    check_final <- ifelse(check1 & check2 & check3, "Yes", "No")
    
    result_temp <<- paste0("<tr>\n", sprintf("<td>%s</td>", filename), "\n", 
                           sprintf("<td>%s</td>", check_final), "\n</tr>")
    
    # Create workbook
    wb <- createWorkbook()
    
    # Iterate the same way as PavoDive, slightly different (creating an anonymous function inside Map())
    Map(function(data, nameofsheet){     
      
      addWorksheet(wb, nameofsheet)
      writeData(wb, nameofsheet, data)
      
    }, result, names(result))
    
    if (version == FALSE) {
      filename1 <- paste0(filename, ".xlsx")
    } else {
      filename1 <- paste0("qc", filename, "_", as.character(Sys.Date()), ".xlsx")
    }
    
    # Save workbook to excel file 
    result_path <- file.path(path, "results")
    if (is.na(file.info(result_path)$isdir)|file.info(result_path)$isdir == FALSE) dir.create(result_path)
    saveWorkbook(wb, file = file.path(result_path, filename1), overwrite = TRUE)
    
    # Output the summary of compare result in the log
    print(summary(comparedf(rtfdf, qcdf, max.print.diffs = max_diff, max.print.diffs.per.var = max_diff_per_var)))
  }
  
}

