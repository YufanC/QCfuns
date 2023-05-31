### Read in sample rtf file
test_file <- system.file("tsfae-st03.rtf", package = "QCfuns")

test_that("Check read-in rtf dimention", {
  
  result_df <- qc_rtf2df("tsfae-st03", dirname(test_file))
  
  expect_identical(dim(result_df), c(33L, 6L))
})

test_that("Check read-in rtf first row value", {
  
  expected_df <- data.frame(
    X             = "Analysis set: Safety",
    Placebo       = "86",
    'Low Dose'    = "84",
    'High Dose'   = "84",
    'Combined'    = "168",
    'Total'       = "254",
    stringsAsFactors = FALSE
  )
  
  result_df <- qc_rtf2df("tsfae-st03", dirname(test_file))
  row1 <- result_df[1, ]
  
  expect_identical(dim(result_df), c(33L, 6L))
})
