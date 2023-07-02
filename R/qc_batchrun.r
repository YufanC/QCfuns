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
  
  ### Execute all scripts in sequence
  sapply(files, source_batch)
  
  ### Create summary html report
  num_files <- length(files)
  
  output_result <- data.frame(
    file_name = character(),
    all_match = character(),
    error_text = character(),
    error_exist = integer(),
    warning_text = character(),
    stringsAsFactors = FALSE
  )
  
  for (i in 1:num_files) {
    file_temp <- file.path(path, str_replace(basename(files[i]), ".r|.R", ".log"))
    extracted <- extract_errors_and_warnings(file_temp)
    
    all_match <- ifelse(any(extracted$result == "Yes", na.rm = T), "Yes", "No")
    error_text <- paste(extracted$errors, collapse = "\n")
    error_exist <- ifelse(any(nchar(extracted$errors) > 0), 1, 2)
    warning_text <- paste(extracted$warnings, collapse = "\n")
    
    row <- data.frame(
      file_name = str_extract(basename(file_temp), "(?<=qc).*?(?=\\.log)"),
      all_match = all_match,
      error_text = error_text,
      error_exist = error_exist,
      warning_text = warning_text,
      stringsAsFactors = FALSE
    )
    
    output_result <- rbind(output_result, row)
  }
  
  ### Create summary html report
  temp_result1 <- output_result %>% 
    arrange(error_exist, all_match) %>% 
    mutate(html_col = ifelse(all_match == "Yes" | error_exist == 1,
                             paste0("<tr>\n", 
                                    sprintf("<td>%s</td>", file_name), 
                                    "\n",
                                    sprintf("<td>%s</td>", all_match), 
                                    "\n",
                                    sprintf("<td>%s</td>", error_text), 
                                    "\n",
                                    sprintf("<td>%s</td>", warning_text), 
                                    "\n</tr>"),
                             paste0("<tr>\n", 
                                    sprintf(paste0("<td><a href='", paste0("qc", file_name, ".html"), "' target='_blank'>%s</a></td>"), file_name), 
                                    "\n",
                                    sprintf("<td>%s</td>", all_match), 
                                    "\n",
                                    sprintf("<td>%s</td>", error_text), 
                                    "\n",
                                    sprintf("<td>%s</td>", warning_text), 
                                    "\n</tr>")))
  
  temp_result <- paste(temp_result1$html_col, collapse = "")
  
  ### Output comparison result to viewer
  # Save compare result html to path
  filename <- paste0("compare_results_", as.character(Sys.Date()), ".html")
  file_path <- file.path(path, filename)
  
  if (interactive()){
    
    ### Display result in viewer
    # create temp file
    temp_dir = tempdir()
    temp_file <- paste0(temp_dir, "/temp.html")
    
    # diverts output to a temp file
    sink(temp_file, append = TRUE)
    
    cat("<h2>Comparison Results</h2>", 
        "<table border='1'>", 
        "<tr>", 
        "<th>Output ID</th>", 
        "<th>All Matched</th>", 
        "<th>Error</th>", 
        "<th>Warning</th>", 
        "</tr>", 
        temp_result, 
        "</table>", 
        file = temp_file)
    
    # Use RStudio viewer if available, otherwise open in a web browser
    if (!is.null(getOption("viewer"))) {
      rstudioapi::viewer(temp_file)
    } else {
      browseURL(temp_file)
    }
    
    # diverts output back to console
    sink()
  } 
  
  ### save comparison result in html
  cat("<h2>Comparison Results</h2>", 
      "<table border='1'>", 
      "<tr>", 
      "<th>Output ID</th>", 
      "<th>All Matched</th>", 
      "<th>Error</th>", 
      "<th>Warning</th>", 
      "</tr>", 
      temp_result, 
      "</table>", 
      file = file_path)
}

source_batch <- function(script) {
  op <- options(log.rx = NULL); on.exit(options(op)) # to reset after each
  
  # Create a message for tracking
  message(paste("Running", script))
  
  axecute(
    script,
    log_name = str_replace(basename(script), ".r|.R", ".log"),
    log_path = path,
    remove_log_object = TRUE,
    quit_on_error = FALSE,
    to_report = c("messages", "output", "results")
  )
}

extract_errors_and_warnings <- function(file_path) {
  errors <- vector("character")
  warnings <- vector("character")
  result <- vector("character")
  
  lines <- readLines(file_path)
  num_lines <- length(lines)
  i <- 1
  
  while (i <= num_lines) {
    if (grepl("^Errors:", lines[i])) {
      i <- i + 1
      while (i <= num_lines && !grepl("^Warnings:", lines[i])) {
        errors <- c(errors, trimws(lines[i]))
        i <- i + 1
      }
    } else if (grepl("^Warnings:", lines[i])) {
      i <- i + 1
      while (i <= num_lines && !grepl("^-+$", lines[i])) {
        warnings <- c(warnings, trimws(lines[i]))
        i <- i + 1
      }
    } else if (grepl("The two data frames are different!", lines[i])) {
      i <- i + 1
      result <- "No"
    } else if (grepl("The two data frames are the same after accounting for tolerance!", lines[i])) {
      i <- i + 1
      result <- "Yes"
    } else {
      i <- i + 1
    }
  }
  return(list(errors = errors, warnings = warnings, result = result))
}

