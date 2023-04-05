test_that("error message works for qc_chgfb", {
  adsl <- data.frame(
    USUBJID = 1:10,
    TRT01P = sample(c("A", "B", "C"), 10, replace = TRUE))
  
  param <- data.frame(PARAM = c("Test1", "Test2"))
  visit <- data.frame(AVISIT = c("Baseline", "Visit1", "Visit2"))
  
  adlb0 <- merge(adsl, param)
  adlb0$BASE <- sample(1:100, 20)
  
  adlb <- merge(adlb0, visit)
  adlb$AVAL <- sample(1:100, 60)
  adlb$CHG <- ifelse(adlb$AVISIT == "Baseline", NA, adlb$AVAL - adlb$BASE)
  adlb$digit <- ifelse(adlb$PARAM == "Test1", 0, 2)
  
  ### Create analysis row first
  first_row <- qc_cntrow1(input = adsl, colvar = "TRT01P", 
                          row_text = "Analysis set: Safety")
  
  expect_error(qc_chgfb(adlb, "AVAL", "CHG", rowvar = c("PARAM", "TRT01P", "AVISIT"), 
                        stats_list = c("N", "Med"), max_digit = 0, keep = FALSE))
})
