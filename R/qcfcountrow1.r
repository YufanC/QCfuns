#' Create Analysis Set Row 
#'
#' Function to create analysis set row
#' @param input input dataframe 
#' @param colvar column variable 
#' @param row_text row text 
#' @param subset subset criteria. Default = "TRUE" means no subsetting
#' @return Analysis set row
#' @examples 
#' cntrow1(input = adae, colvar = "TRT01P", row_text = "Analysis set: Safety")
#' @export
countrow1 <- function(input, colvar, row_text = NULL, subset = "TRUE"){
  first_row <- input %>% 
    filter(eval(parse(text = subset))) %>% 
    group_by(.data[[colvar]], .drop = F) %>% 
    count() %>% 
    mutate(N_trt = n) %>% 
    select(-n)
  
  first_row1 <- first_row %>% 
    distinct(.data[[colvar]], N_trt) %>% 
    pivot_wider(names_from = .data[[colvar]],
                values_from = N_trt)
  
  first_row2 <- cbind(row_text, first_row1)
  
  colnames(first_row2) <- c("row_text", levels(as.factor(pull(first_row, colvar))))
  
  return(list(first_row, first_row2))
}
