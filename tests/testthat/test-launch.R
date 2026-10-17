test_that("mvp_app() builds the app without touching the global environment or search path", {
  s0 <- search()
  g0 <- ls(globalenv(), all.names = TRUE)
  o0 <- options("scipen", "shiny.maxRequestSize")
  app <- suppressWarnings(mvp_app(insert_watermark = FALSE))
  expect_s3_class(app, "shiny.appobj")
  expect_identical(search(), s0)
  expect_identical(ls(globalenv(), all.names = TRUE), g0)
  expect_identical(options("scipen", "shiny.maxRequestSize"), o0)
})

test_that("run_mvp() has the same arguments as mvp_app()", {
  expect_identical(formals(run_mvp), formals(mvp_app))
})
