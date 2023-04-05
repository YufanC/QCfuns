test_that("test subset", {
  adae <- data.frame(
    USUBJID = 1:10,
    TRT01P = rep(c("A", "B"), 5),
    SEX = c(rep("M", 5), rep("F", 5)))
  
  expect_equal(qc_cntrow1(adae, "TRT01P", subset = "SEX == 'M'")$row1$A, "3")
})
