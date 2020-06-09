test_that("lst_blblm works", {
  file_names <- Sys.glob("C:/Users/angel/Documents/blblm/mtcars/*")
  fit <- lst_blblm(mpg ~ wt * hp, data = mtcars, B = 100, file_names = file_names)
  expect_s3_class(fit, "blblm")
})
