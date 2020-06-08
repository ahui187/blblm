#' Bag of Little Bootstraps using Parallelization
#'
#' Bag of little bootstraps for linear regression using parallelization. The user can prespecify the amount
#' of workers used
#'
#' @param formula for linear regression
#' @param data dataset
#' @param m integer number of subsamples data is split into
#' @param B integer bootstrap number
#' @param work integer number of workers
#'
#' @return list of linear regression coefficients and standard deviation for each bootstrap sample
#' @export
#'

parblblm <- function(formula, data, m = 10, B = 5000, work = 4) {
  suppressWarnings(future::plan(future::multiprocess, workers = work))
  data_list <- split_data(data, m)
  estimates <- furrr::future_map(
    data_list,
    ~ lm_each_subsample(formula = formula, data = ., n = nrow(data), B = B))
  res <- list(estimates = estimates, formula = formula)
  class(res) <- "blblm"
  invisible(res)
}