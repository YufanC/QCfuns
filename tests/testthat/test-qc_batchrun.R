test_that("Check if the output html exists and is correct", {
  
  # Store outputs in temporary diretory
  dir_temp <- tempdir()
  
  compare_script1 <- "dir_temp <- tempdir() \n
  qc <- data.frame(Row_text = c('N', 'N'), Treatment = c('5', '7'), Placebo = c('10', '6')) \n
  rtf <- data.frame(Row_text = c('N', 'N'), Treatment = c('5', '7'), Placebo = c('10', '6')) \n
  qc_comparedf(qc, rtf, path = dir_temp, filename = 'test_output1')"
  
  compare_script2 <- "dir_temp <- tempdir() \n
  qc <- data.frame(Row_text = c('N', 'N'), Treatment = c('5', '7'), Placebo = c('10', '6')) \n
  rtf <- data.frame(Row_text = c('N', 'N'), Treatment = c('6', '7'), Placebo = c('10', '6')) \n
  qc_comparedf(qc, rtf, path = dir_temp, filename = 'test_output2')"
  
  writeLines(compare_script1, con = file.path(dir_temp, "qctable1.r"))
  writeLines(compare_script2, con = file.path(dir_temp, "qctable2.r"))
  
  file_list <- c(file.path(dir_temp, "qctable1.r"), file.path(dir_temp, "qctable2.r"))
  qc_batchrun(file_list, dir_temp, parallel = T)
  
  file_name <- paste0("compare_results_", Sys.Date(), ".html")
  file_path <- file.path(dir_temp, file_name)
  ### Check if the output html file exists
  expect_true(file.exists(file_path))
  
  ### Check if the output html file is correct
  results <- readLines(file_path, warn = F)
  expect_equal(results[3], "<td>No</td>")
  expect_equal(results[9], "<td>Yes</td>")
})

# test_that("Check if the output html is correct using real scripts", {
#   
#   # Store outputs in temporary diretory
#   dir_temp <- tempdir()
#   
#   file_list <- file.path(system.file(package = "QCfuns"), c("qctsidem-st02.R", "qctsfae-st03.R"))
#   # file_list <- list.files(path = system.file(package = "QCfuns"), pattern = "qct.*\\.R", 
#   #                         full.names = TRUE)
#   qc_batchrun(file_list, dir_temp)
# 
#   file_name <- paste0("compare_results_", Sys.Date(), ".html")
#   file_path <- file.path(dir_temp, file_name)
# 
#   ### Check if the output html file is correct
#   results <- readLines(file_path, warn = F)
#   expect_equal(results[9], "<td>Yes</td>")
#   expect_equal(results[3], "<td>No</td>")
# })
