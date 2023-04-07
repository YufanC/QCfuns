test_that("check row order", {
  adlb <- data.frame(
    USUBJID = 1:9,
    TRT01P = factor(rep(c("A", "B", "C"), 3), levels = c("B", "C", "A")))
  expect_equal(pull(qc_cntrow1_shift(adlb, "TRT01P", row_text = "total"), row_text),
               c("total", "B", "C", "A"))
})
