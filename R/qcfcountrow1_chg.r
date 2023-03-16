#' Create Analysis Set Row for Change From Baseline Table
#'
#' Function to create analysis set row for change from baseline table
#' @param input input dataframe 
#' @param colvar column variable 
#' @param row_text row text 
#' @param subset subset criteria. Default = "TRUE" means no subsetting
#' @return Analysis set row
#' @examples 
#' cntrow1_chg(input = adlb, colvar = "TRT01P", row_text = "Analysis set: Full")
#' @export
cntrow1_chg <- function(input, colvar, row_text = NULL, subset = "TRUE"){
  first_row <- input %>%
    filter(eval(parse(text = subset))) %>% 
    group_by(.data[[colvar]]) %>%
    summarise(n = n_distinct(USUBJID)) %>%
    mutate(N.x = n,
           N.y = n,
           row_text = .data[[colvar]]) %>% 
    ungroup() %>% 
    select(-c(n, .data[[colvar]])) %>% 
    bind_rows(c(row_text = row_text), .)
  
  return(first_row)
}
