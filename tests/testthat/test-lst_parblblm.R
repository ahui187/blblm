test_that("lst_parblblm works", {
  file_names <- Sys.glob("C:/Users/angel/Documents/blblm/mtcars/*")
  fit <- lst_parblblm(mpg ~ wt * hp, data = mtcars, B = 100, file_names = file_names, work = 2)
  expect_s3_class(fit, "blblm")
})
