test_that("Check if the output XLSX file exists and has the correct sheets", {
  
  # Create sample data
  qc <- data.frame(A = c(1, 2, 3, 4, 5), B = c("a", "b", "c", "d", "e"))
  rtf <- data.frame(A = c(1, 2, 4, 3, 2), B = c("a", "b", "d", "e", "e"))
  
  # Store outputs in temporary diretory
  dir_temp <- tempdir()
  qc_compare2xlsx(qc, rtf, path = dir_temp, filename = "test_output")
  
  file_name <- paste0("qctest_output_", Sys.Date(), ".xlsx")
  file_path <- file.path(dir_temp, "results", file_name)
  
  ### Check if the output XLSX file exists
  expect_true(file.exists(file_path))
  
  ### Check if the sheet names in the XLSX file are correct
  expect_equal(getSheetNames(file_path), c("frame.summary.table", "diffs.table"))
  
  ### Check if the version argument works
  file_path1 <- file.path(dir_temp, "results", "qctest_output.xlsx")
  expect_true(file.exists(file_path1))
})

test_that("Check if max_diff and max_diff_per_var arguments work", {
  
  # Create sample data
  qc <- data.frame(A = c(1, 2, 3, 4, 5), B = c("a", "b", "c", "d", "e"))
  rtf <- data.frame(A = c(1, 2, 4, 3, 2), B = c("a", "b", "d", "e", "e"))
  
  dir_temp <- tempdir()
  
  ### Check if the number of difference displayed is correct
  compare_result <- capture.output({
    qc_compare2xlsx(qc, rtf, path = dir_temp, filename = "test_output",
                                    max_diff = NA, max_diff_per_var = NA)
  })
  
  start_line <- which(str_detect(compare_result, "Table: Differences detected"))[2] + 4
  
  end_line <- which(compare_result == "Table: Non-identical attributes") - 3
  
  n_dff <- end_line - start_line
  
  expect_equal(n_diff, 5)
  
  ### Check if the number of difference displayed is correct
  compare_result <- capture.output({
    qc_compare2xlsx(qc, rtf, path = dir_temp, filename = "test_output",
                    max_diff = 3, max_diff_per_var = NA)
  })
  
  start_line <- which(str_detect(compare_result, "Table: Differences detected"))[2] + 4
  
  end_line <- which(compare_result == "Table: Non-identical attributes") - 3
  
  n_diff <- end_line - start_line
  
  expect_equal(n_diff, 3)
  
  ### Check if the number of difference displayed is correct
  compare_result <- capture.output({
    qc_compare2xlsx(qc, rtf, path = dir_temp, filename = "test_output",
                    max_diff = NA, max_diff_per_var = 2)
  })
  
  start_line <- which(str_detect(compare_result, "Table: Differences detected"))[2] + 4
  
  end_line <- which(compare_result == "Table: Non-identical attributes") - 3
  
  n_diff <- end_line - start_line
  
  expect_equal(n_diff, 4)
})

