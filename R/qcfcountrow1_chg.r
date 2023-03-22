#' Create Analysis Set Row for Change From Baseline Table
#'
#' Function to create analysis set row for change from baseline table
#' @inheritParams qc_cntrow1
#' @return Analysis set row for change from baseline table
#' @examples 
#' adlb <- data.frame(
#'   USUBJID = 1:10,
#'   TRT01P = sample(c("A", "B", "C"), 10, replace = T))
#'   
#' first_row <- qc_cntrow1_chg(input = adlb, colvar = "TRT01P", row_text = "Analysis set: Full")
#' first_row
#' @export
qc_cntrow1_chg <- function(input, colvar = "TRT01P", row_text = "Analysis set: Full", subset = "TRUE"){
  first_row <- input %>%
    filter(eval(parse(text = subset))) %>% 
    group_by(.data[[colvar]]) %>%
    summarise(n = n_distinct(USUBJID), .groups = "drop") %>%
    mutate(N.x = n,
           N.y = n,
           row_text = .data[[colvar]]) %>% 
    select(-c(n, .data[[colvar]])) %>% 
    bind_rows(c(row_text = row_text), .)
  
  return(first_row)
}
