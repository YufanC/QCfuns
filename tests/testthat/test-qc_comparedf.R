test_that("Check if the output HTML file exists", {
  
  # Create sample data
  qc <- data.frame(A = c(1, 2, 3, 4, 5), B = c("a", "b", "c", "d", "e"))
  rtf <- data.frame(A = c(1, 2, 4, 3, 2), B = c("a", "b", "d", "e", "e"))
  
  # Store outputs in temporary diretory
  dir_temp <- tempdir()
  qc_comparedf(qc, rtf, path = dir_temp, filename = "test_output")
  
  file_name <- paste0("qctest_output", ".html")
  file_path <- file.path(dir_temp, file_name)
  
  ### Check if the output XLSX file exists
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
  
  ### Check if the output XLSX file exists
  expect_equal(output, "QC and production are the same for test_output")
})

