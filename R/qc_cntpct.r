#' Create Count and Percentage Row
#'
#' compute count and percentage by column variable
#' @param input input dataframe 
#' @param colvar column variable 
#' @param row_text row text 
#' @param N_row dataframe with N 
#' @param subset subset criteria. Default = NULL
#' @return a dataframe containing count and percentage by colvar
#' @examples 
#' aedecod <- sample(paste0("PT", 1:3), 10, replace = TRUE)
#' 
#' adae <- data.frame(
#'   USUBJID = 1:10,
#'   TRT01P = factor(sample(c("A", "B", "C"), 10, replace = TRUE)),
#'   SEX = factor(sample(c("Female", "Male"), 10, replace = TRUE)),
#'   AEBODSYS = ifelse(aedecod == "PT1", "SOC1", "SOC2"),
#'   AEDECOD = aedecod)
#' 
#' ### Create analysis row first
#' first_row <- qc_cntrow1(input = adae, colvar = "TRT01P", row_text = "Analysis set: Safety")
#' 
#' tab1 <- qc_cntpct(input = adae, colvar = "TRT01P", row_text = "Subjects with 1 or more AEs", 
#'                   N_row = first_row$N_row)
#' tab1
#' @export
qc_cntpct <- function(input, colvar = "TRT01P", row_text = "Subjects with 1 or more AEs", N_row, subset = NULL){
  
  assertthat::assert_that(not_empty(input))
  assertthat::assert_that(assertthat::has_name(input, colvar))
  assertthat::assert_that(is.factor(input[[colvar]]))
  
  ### first N row
  row1 <- input %>%
    group_by(.data[[colvar]], .drop = F) %>% 
    summarise(n = ifelse(is.null(subset), n_distinct(USUBJID, na.rm = T), n_distinct(USUBJID[eval(parse(text = subset))], na.rm = T)), .groups = "drop") %>% 
    left_join(., N_row, by = colvar) %>% 
    mutate(pct = (round_sas(n * 100 / N_trt, 1)),
           col = ifelse(pct == 0, "0", paste0(n, ' (', formatC(pct, format = "f", digits = 1), '%)')))
  
  row2 <- row1 %>% 
    select(all_of(colvar), col) %>% 
    pivot_wider(names_from = all_of(colvar),
                values_from = col)
  
  row3 <- cbind(row_text, row2)
  
  colnames(row3) <- c("row_text", levels(as.factor(pull(N_row, colvar))))
  
  return(row3)
  
}