#' Batch Run QC Scripts
#'
#' Execute R qc scripts by batch with log
#' @param files a character vector of full/relative path names 
#' @param path the folder path of HTML output file. By default the path is the working directory, getwd().
#' @param parallel whether to batch run in parallel, default is FALSE
#'
#' @return A summary table of comparison results will display in html
#' @export
#' @examples
#' \dontrun{
#' ### select all r scripts start with 'qct' on SPACE
#' qc_files <- list.files(path = qc[["PDEV"]], pattern = "qct.*\\.r", 
#'                        full.names = TRUE)
#' 
#' qc_batchrun(files = qc_files, path = qc[["PDEV"]], parallel = TRUE)
#' }
#' @importFrom logrx axecute
#' @importFrom rstudioapi viewer
#' @importFrom utils browseURL
qc_batchrun <- function(files, path = ".", parallel = FALSE){
  
  assertthat::assert_that(all(file.exists(files)))
  assertthat::is.dir(path)
  
  if (parallel){
    # Determine the number of core used by current system
    system_info <- Sys.info()
    if (system_info["sysname"] == "Windows"){
      parallel::mclapply(files, source_batch_par, path = path, mc.cores = 1)
    } else {
      # Define the number of cores to use for parallel execution
      num_cores <- parallel::detectCores()
      
      # Parallelize the execution of the script
      parallel::mclapply(files, source_batch_par, path = path, mc.cores = num_cores)
    }
  } else {
    ### Execute all scripts in sequence
    sapply(files, source_batch, path = path)
  }
  
  ### Create summary html report
  num_files <- length(files)
  
  output_result <- data.frame(
    file_name = character(),
    all_match = character(),
    link_text = character(),
    error_text = character(),
    error_exist = integer(),
    warning_text = character(),
    stringsAsFactors = FALSE
  )
  
  for (i in 1:num_files) {
    file_temp <- file.path(path, str_replace(basename(files[i]), ".r|.R", ".log"))
    extracted <- extract_errors_and_warnings(file_temp)
    
    all_match <- ifelse(any(str_detect(extracted$result, "QC and production are the same for")), "Yes", "No")
    message_text <- paste(extracted$result, collapse = "")
    error_text <- paste(extracted$errors, collapse = "\n")
    error_exist <- ifelse(any(nchar(extracted$errors) > 0), 1, 2)
    warning_text <- paste(extracted$warnings, collapse = "\n")
    
    row <- data.frame(
      # Get filename from message in log
      file_name = ifelse(message_text == "", 
                         str_extract(basename(file_temp), ".*?(?=\\.log)"),
                         word(message_text, -1)),
      all_match = all_match,
      # Get names of the link for scripts with unmatched results
      link_text = ifelse(str_detect(message_text, "QC and production are not matched for"),
                         word(message_text, -1),
                         NA),
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
    mutate(html_col = ifelse(is.na(link_text),
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
                                    sprintf(paste0("<td><a href='", paste0("qc", link_text, ".html"), "' target='_blank'>%s</a></td>"), file_name), 
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

source_batch <- function(script, path = NULL) {
  op <- options(log.rx = NULL); on.exit(options(op)) # to reset after each
  
  # Create a message for tracking
  message(paste("Running", script))
  
  axecute(
    script,
    log_name = str_replace(basename(script), ".r|.R", ".log"),
    log_path = path,
    quit_on_error = FALSE
  )
}

source_batch_par <- function(script, path = NULL) {
  
  axecute(
    script,
    log_name = str_replace(basename(script), ".r|.R", ".log"),
    log_path = path,
    quit_on_error = FALSE
  )
}

extract_errors_and_warnings <- function(file_path) {
  errors <- vector("character")
  warnings <- vector("character")
  result <- vector("character")
  
  lines <- readLines(file_path, warn = FALSE)
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
    } else if (grepl("QC and production are not matched for", lines[i])) {
      result <- lines[i]
      i <- i + 1
    } else if (grepl("QC and production are the same for", lines[i])) {
      result <- lines[i]
      i <- i + 1
    } else {
      i <- i + 1
    }
  }
  return(list(errors = errors, warnings = warnings, result = result))
}

