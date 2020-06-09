test_that("parblblm works", {
  fit <- parblblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100, work = 2)
  expect_s3_class(fit, "blblm")
})
