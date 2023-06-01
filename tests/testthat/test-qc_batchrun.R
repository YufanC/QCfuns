test_that("Check if the output html exists and is correct", {
  
  # Store outputs in temporary diretory
  dir_temp <- tempdir()
  
  compare_script1 <- "dir_temp <- tempdir() \n
  qc <- data.frame(x = c(1, 2), y = c('a', 'b')) \n
  rtf <- data.frame(x = c(1, 2), y = c('a', 'b')) \n
  qc_compare2xlsx(qc, rtf, path = dir_temp, filename = 'test_output1')"
  
  compare_script2 <- "dir_temp <- tempdir() \n
  qc <- data.frame(x = c(1, 1), y = c('a', 'b')) \n
  rtf <- data.frame(x = c(1, 2), y = c('a', 'b')) \n
  qc_compare2xlsx(qc, rtf, path = dir_temp, filename = 'test_output2')"
  
  writeLines(compare_script1, con = file.path(dir_temp, "qctable1.r"))
  writeLines(compare_script2, con = file.path(dir_temp, "qctable2.r"))
  
  file_list <- c(file.path(dir_temp, "qctable1.r"), file.path(dir_temp, "qctable2.r"))
  qc_batchrun(file_list, dir_temp)
  
  file_name <- paste0("compare_results_", Sys.Date(), ".html")
  file_path <- file.path(dir_temp, "results", file_name)
  ### Check if the output html file exists
  expect_true(file.exists(file_path))
  
  ### Check if the output html file is correct
  results <- readLines(file_path, warn = F)
  expect_equal(results[3], "<td>Yes</td>")
  expect_equal(results[6], "<td>No</td>")
})
