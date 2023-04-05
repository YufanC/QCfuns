#' Create Analysis Set Row for Shift Table
#'
#' Function to create analysis set row for shift table
#' @inheritParams qc_cntrow1
#' @return Analysis set row for shift table
#' @examples 
#' adlb <- data.frame(
#'   USUBJID = 1:10,
#'   TRT01P = sample(c("A", "B", "C"), 10, replace = TRUE))
#'   
#' first_row <- qc_cntrow1_shift(input = adlb, colvar = "TRT01P", row_text = "Analysis set: Full")
#' first_row
#' @export
### Create the analysis set row
qc_cntrow1_shift <- function(input, colvar = "TRT01P", row_text = "Analysis set: Full", subset = "TRUE"){
  tab1 <- input %>% 
    mutate("{colvar}" := "Total")
  
  first_row <- bind_rows(input, tab1) %>%
    filter(eval(parse(text = subset))) %>% 
    group_by(.data[[colvar]]) %>%
    summarise(n = n_distinct(USUBJID), .groups = "drop") %>%
    mutate(N = n,
           row_text = if_else(.data[[colvar]] == "Total", row_text, .data[[colvar]])) %>% 
    select(-n, -all_of(colvar))
  
  return(first_row)
}
