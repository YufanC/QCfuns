test_that("Check if the output HTML file exists", {
  
  # Create sample data
  qc <- data.frame(A = c(1, 2, 3, 4, 5), B = c("a", "b", "c", "d", "e"))
  rtf <- data.frame(A = c(1, 2, 4, 3, 2), B = c("a", "b", "d", "e", "e"))
  
  # Store outputs in temporary diretory
  dir_temp <- tempdir()
  qc_comparedf(qc, rtf, path = dir_temp, filename = "test_output")
  
  file_name <- paste0("qctest_output", ".html")
  file_path <- file.path(dir_temp, file_name)
  
  ### Check if the output html file exists
  expect_true(file.exists(file_path))
})

test_that("Check if the message works for two identical dataframs", {
  
  # Create sample data
  qc <- data.frame(A = c(1, 2, 3, 4, 5), B = c("a", "b", "c", "d", "e"))
  rtf <- data.frame(A = c(1, 2, 3, 4, 5), B = c("a", "b", "c", "d", "e"))
  
  # Store outputs in temporary diretory
  dir_temp <- tempdir()
  output <- capture.output(qc_comparedf(qc, rtf, path = dir_temp, filename = "test_output"), 
                           type = "message")[2]
  
  ### Check if the message is correct
  expect_equal(output, "QC and production are the same for test_output")
})

test_that("Check if the message works for difference detected", {
  
  # Create sample data
  qc <- qc_rtf2df("tsfae-st03", system.file(package = "QCfuns")) %>% 
    mutate(Combined = ifelse(X == "Avg exposure (days)", '97.2', Combined))
  rtf <- qc_rtf2df("tsfae-st03", system.file(package = "QCfuns"))
  
  # Store outputs in temporary diretory
  dir_temp <- tempdir()
  qc_comparedf(qc, rtf, path = dir_temp, filename = "tsfae-st03")
  
  file_name <- paste0("qctsfae-st03", ".html")
  file_path <- file.path(dir_temp, file_name)
  
  ### Check if the output html file is correct
  results <- readLines(file_path, warn = F)
  results_line <- results[str_detect(results, "97.2")]
  color <- str_extract(results_line, " color:(.*?);")
  expect_equal(color, " color: #FC4E07;")
})



