test_that("geometric mean and CV are correct for positive values", {
  x <- c(1, 10, 100)
  expect_equal(MVPapp:::gm_mean(x), 10)
  expect_equal(MVPapp:::gm_mean_cv(c(5, 5, 5)), 0)
  expect_equal(MVPapp:::gm_mean_cv(x), sqrt(exp(stats::sd(log(x))^2) - 1) * 100)
})

test_that("log breaks cover the requested decades", {
  b <- MVPapp:::generate_log_breaks(c(1, 3), -1, 1)
  expect_equal(b, c(0.1, 0.3, 1, 3, 10, 30))
})

test_that("quantize assigns values to the nearest level", {
  expect_equal(MVPapp:::quantize(c(0.4, 1.6, 9), levels = c(0, 2, 10)), c(0, 2, 10))
})

test_that("sanitize_numeric_input handles bad and zero inputs", {
  expect_equal(MVPapp:::sanitize_numeric_input("5"), 5)
  expect_equal(suppressWarnings(MVPapp:::sanitize_numeric_input("abc")), 0)
  expect_equal(suppressWarnings(MVPapp:::sanitize_numeric_input("abc", allow_zero = FALSE)), 1)
  expect_equal(MVPapp:::sanitize_numeric_input(0, allow_zero = FALSE, return_value = 2), 2)
})

test_that("code typed in the app is evaluated without attaching packages", {
  s0 <- search()
  env <- MVPapp:::mvp_user_env()
  env$nmdata_global <- data.frame(ID = 1:3, DV = c(1, 5, 9))
  res <- MVPapp:::safely_eval(parse(text = "nmdata_global %>% filter(DV > 2) %>% mutate(X = DV * 2)"),
                              envir = env)
  expect_null(res$error)
  expect_equal(res$result$X, c(10, 18))
  expect_identical(search(), s0)
})
