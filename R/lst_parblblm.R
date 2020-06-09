#' Bag of Little Bootstraps using List of File Names with Parallelization
#'
#' Data has to already have been split into multiple files. The data is then read in
#' through a list of file names containing the data as a parameter to be specified. The blblm function
#' with parallelization is then used to return linear regression coefficients and standard deviation.
#' The amount of workers to be used can also be specified as a parameter.
#'
#' @param formula for linear regression
#' @param data dataset
#' @param B integer bootstrap sample number
#' @param file_names list of file names the data is split into
#' @param work integer number of workers for parallelization
#'
#' @return list of linear regression coefficients and standard deviation for each bootstrap sample
#' @export
#'

lst_parblblm <- function(formula, data, B = 5000, file_names, work = 4) {
  suppressWarnings(future::plan(future::multiprocess, workers = work))
  lt = file_names %>% furrr::future_map(~{
    df <- readr::read_csv(., col_types = readr::cols())
  })
  estimates <- furrr::future_map(
    lt,
    ~ lm_each_subsample(formula = formula, data = ., n = nrow(data), B = B))
  res <- list(estimates = estimates, formula = formula)
  class(res) <- "blblm"
  invisible(res)
}