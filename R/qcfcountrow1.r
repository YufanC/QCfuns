#' Create Analysis Set Row 
#'
#' Function to create analysis set row
#' @param input input dataframe 
#' @param colvar column variable 
#' @param row_text row text 
#' @param subset subset criteria. Default = "TRUE" means no subsetting
#' @return Analysis set row list containing two elements.
#' $N_row is a dataframe with N that can be passed to following functions
#' $row1 is the analysis set row that can be combined with other following rows
#' @examples 
#' adae <- data.frame(
#'   USUBJID = 1:10,
#'   TRT01P = sample(c("A", "B", "C"), 10, replace = TRUE))
#'   
#' first_row <- qc_cntrow1(input = adae, colvar = "TRT01P", row_text = "Analysis set: Safety")
#' 
#' first_row$N_row
#' first_row$row1
#' @export
#' @import tidyr
qc_cntrow1 <- function(input, colvar = "TRT01P", row_text = "Analysis set: Safety", subset = "TRUE"){
  
  assertthat::assert_that(not_empty(input))
  assertthat::assert_that(assertthat::has_name(input, colvar))
  
  first_row <- input %>% 
    filter(eval(parse(text = subset))) %>% 
    group_by(.data[[colvar]], .drop = F) %>% 
    summarise(n = n_distinct(USUBJID), .groups = "drop") %>%
    mutate(N_trt = n) %>% 
    select(-n)
  
  first_row1 <- first_row %>% 
    distinct(.data[[colvar]], N_trt) %>% 
    pivot_wider(names_from = all_of(colvar),
                values_from = N_trt)  %>% 
    mutate(across(where(is.numeric), as.character))
  
  first_row2 <- cbind(row_text, first_row1)
  
  colnames(first_row2) <- c("row_text", levels(as.factor(pull(first_row, colvar))))
  
  return(list(N_row = first_row,
              row1 = first_row2))
}
