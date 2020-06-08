#' User choice for blblm with or without parallelization
#'
#' Input is read from user whether parallelization will be used to implement bag of little
#' bootstraps(blblm)
#'
#' @param formula for linear regression
#' @param data from dataset
#' @param m integer number of subsamples data is separated into
#' @param B integer number of bootstrap samples
#' @param work integer number of workers to be used if parallization is used
#'
#' @return list of linear regression coefficients and standard deviation for each bootstrap sample
#' @export
#'
#' @examples
#' choice_blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100, work = 4)
choice_blblm = function(formula, data, m, B, work = 4) {
  yn = readline(prompt = "Would you like to use parallel processing? Type yes or no: ")
  if (tolower(yn) == "yes") {
    fit <- parblblm(formula, data, m, B, work)
    return(fit)
  }
  else if (tolower(yn) == "no") {
    fit <- blblm(formula, data, m, B)
    return(fit)
  }
  else
    print("Input is not recognized")
}