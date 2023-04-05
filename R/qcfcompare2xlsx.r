#' Compare dataframes
#'
#' compare two dataframe and output the result to xlsx
#' @param qc the dataframe you create in QC
#' @param rtf the dataframe read in from rtf
#' @param path the path to put xlsx format compare result
#' @param filename tableid
#' @param version add version control by current date. Default = TURE
#' @param max_diff maximum number of differences to report
#' @param max_diff_per_var maximum number of differences per variable to report
#' @return compare result in xlsx
#' @examplesIf exists("qc") 
#' qc_compare2xlsx(qc = tab_qc, rtf = tab_rtf, path = qc, filename = "tableid")
#' @export
#' @import openxlsx
#' @importFrom arsenal comparedf
qc_compare2xlsx <- function(qc, rtf, path = qc, filename = "TSIDEM01", max_diff = 50, max_diff_per_var = 10, version = TRUE) {
  
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
    
    ### Output comparison result to temp1.txt
    check1 <- result$frame.summary.table$ncol[1] == result0$frame.summary.table$ncol[2]
    check2 <- result$frame.summary.table$nrow[1] == result0$frame.summary.table$nrow[2]
    check3 <- nrow(result$diffs.table) == 0
    check_final <- ifelse(check1 & check2 & check3, "Yes", "No")
    ### Append all compare results and store them in temp1.txt
    cat("<tr>\n", sprintf("<td>%s</td>", filename), "\n", 
        sprintf("<td>%s</td>", check_final), "\n</tr>", file = file.path(write_path(path), "temp1.txt"), sep = "", append = T)
    
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
    result_path <- file.path(envsetup::write_path(path), "results")
    if (!exists(result_path)) dir.create(result_path)
    saveWorkbook(wb, file = file.path(result_path, filename1), overwrite = TRUE)
    
    # Output the summary of compare result in the log
    print(summary(comparedf(rtfdf, qcdf, max.print.diffs = max_diff, max.print.diffs.per.var = max_diff_per_var)))
  }
  
}

