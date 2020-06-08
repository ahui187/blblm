#' Bag of Little Bootstraps using List of File Names
#'
#' Instead of splitting the data within the blblm function, the data is already split up into multiple files.
#' This function will read in a list of those file names as an extra parameter and carry out the blblm function.
#'
#' @param formula for linear regression
#' @param data dataset
#' @param B integer bootstrap sample number
#' @param file_names list of file names the data is split into
#'
#' @return list of linear regression coefficients and standard deviation for each bootstrap sample
#' @export
#'

lst_blblm <- function(formula, data, B = 5000, file_names) {
  lt = file_names %>% map(~{
    df <- readr::read_csv(., col_types = cols())
  })
  estimates <- map(
    lt,
    ~ lm_each_subsample(formula = formula, data = ., n = nrow(data), B = B))
  res <- list(estimates = estimates, formula = formula)
  class(res) <- "blblm"
  invisible(res)
}