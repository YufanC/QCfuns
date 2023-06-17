#' Create Analysis Set Row for Shift Table
#'
#' Function to create analysis set row for shift table
#' @inheritParams qc_cntrow1
#' @return Analysis set row for shift table
#' @examples 
#' adlb <- data.frame(
#'   USUBJID = 1:10,
#'   TRT01P = factor(sample(c("A", "B", "C"), 10, replace = TRUE)))
#'   
#' first_row <- qc_cntrow1_shift(input = adlb, colvar = "TRT01P", row_text = "Analysis set: Full")
#' first_row
#' @export
### Create the analysis set row
qc_cntrow1_shift <- function(input, colvar = "TRT01P", row_text = "Analysis set: Full", subset = NULL){
  
  assertthat::assert_that(not_empty(input))
  assertthat::assert_that(assertthat::has_name(input, colvar))
  assertthat::assert_that(is.factor(input[[colvar]]))
  
  tab1 <- input %>% 
    mutate("{colvar}" := "Total")
  
  first_row <- bind_rows(input, tab1) %>%
    mutate("{colvar}" := factor(.data[[colvar]], levels = c("Total", levels(pull(input, colvar))))) %>% 
    group_by(.data[[colvar]]) %>%
    summarise(n = ifelse(is.null(subset), n_distinct(USUBJID, na.rm = T), n_distinct(USUBJID[eval(parse(text = subset))], na.rm = T)), .groups = "drop") %>% 
    mutate(N = n,
           row_text = if_else(.data[[colvar]] == "Total", row_text, as.character(.data[[colvar]]))) %>% 
    select(row_text, N) %>% 
    mutate(across(where(is.numeric), as.character))
  
  return(first_row)
}
