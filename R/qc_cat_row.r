#' Create Rows for Categorical Variables
#'
#' Function to create rows for categorical variables in demographic table
#' @inheritParams qc_cntrow1
#' @param rowvar row variable
#' @param keep if = TRUE, keep all factor levels
#' @return dataframe with demographic rows 
#' @examples 
#' adsl <- data.frame(
#'   USUBJID = 1:10,
#'   TRT01P = factor(sample(c("A", "B", "C"), 10, replace = TRUE)),
#'   SEX = factor(sample(c("Female", "Male"), 10, replace = TRUE)))
#'   
#' tab1 <- qc_cat_row(adsl, "TRT01P", "SEX")
#' tab1
#' @export
### Categorical variable rows
qc_cat_row <- function(input, colvar = "TRT01P", rowvar = "SEX", row_text = "Sex", keep = TRUE){
  
  assertthat::assert_that(not_empty(input))
  assertthat::assert_that(assertthat::has_name(input, c(colvar, rowvar)))
  assertthat::assert_that(is.factor(input[[colvar]]))
  
  N_row <- input %>% 
    filter(!is.na(.data[[rowvar]])) %>% 
    group_by(.data[[colvar]], .drop = F) %>% 
    summarise(n = n_distinct(USUBJID), .groups = "drop") %>% 
    mutate(N_trt = n) %>% 
    select(-n)
  
  N_row1 <- N_row %>% 
    distinct(.data[[colvar]], N_trt) %>% 
    pivot_wider(names_from = all_of(colvar),
                values_from = N_trt)  %>% 
    mutate(across(where(is.numeric), as.character))
  
  N_row2 <- cbind("N", N_row1)
  
  # Calculate count and percentage
  tab1 <- input %>% 
    filter(!is.na(.data[[rowvar]])) %>% 
    group_by(.data[[colvar]], .data[[rowvar]], .drop = !keep) %>% 
    summarise(n = n_distinct(USUBJID), .groups = "drop") %>%
    left_join(N_row, by = colvar) %>% 
    mutate(pct = (round_sas(n * 100 / N_trt, 1)),
           col = ifelse(pct == 0, "0", paste0(n, ' (', formatC(pct, format = "f", digits = 1), '%)')))
  
  # Transpose data
  tab2 <- tab1 %>%
    select(all_of(c(rowvar, colvar)), col) %>%
    pivot_wider(names_from = all_of(colvar), 
                values_from = col)
  
  colnames(tab2) <- c("row_text", levels(input[[colvar]]))
  colnames(N_row2) <- c("row_text", levels(input[[colvar]]))
  
  tab5 <- bind_rows(data.frame(row_text = row_text), N_row2, tab2)
  
  return(tab5)
}