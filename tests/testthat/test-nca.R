test_that("NCA PDF page helpers work without unexported ncar functions (#26)", {
  f <- tempfile(fileext = ".pdf")
  on.exit(unlink(f), add = TRUE)
  old_width <- getOption("width")
  MVPapp:::nca_prep_pdf(f)
  MVPapp:::nca_add_page(Header1 = "Header")
  MVPapp:::nca_text1(1, 1, "Title")
  MVPapp:::nca_text_m(c("line 1", "line 2"), StartRow = 2)
  MVPapp:::nca_close_pdf()
  expect_identical(getOption("width"), old_width) # page helpers do not change options
  expect_true(file.exists(f))
  expect_gt(file.size(f), 0)
})

test_that("the NCA PDF report has no default path and does not change the working directory (CRAN)", {
  expect_true(identical(formals(MVPapp:::pdfNCA_wm)$fileName, quote(expr = )))
  expect_false(any(grepl("setwd", deparse(MVPapp:::pdfNCA_wm), fixed = TRUE)))
})

test_that("the NCA PDF report restores options, graphical parameters and devices (CRAN)", {
  skip_if_not_installed("ncar")
  skip_if_not_installed("NonCompart")
  conc <- data.frame(Subject = rep(1:2, each = 6),
                     Time    = rep(c(0, 0.5, 1, 2, 4, 8), 2),
                     conc    = c(0, 5, 8, 6, 3, 1, 0, 4, 7, 5, 2.5, 0.8))
  f <- tempfile(fileext = ".pdf")
  on.exit(unlink(f), add = TRUE)
  grDevices::pdf(NULL)                       # a device the user already has open
  on.exit(grDevices::dev.off(), add = TRUE)
  user_dev   <- grDevices::dev.cur()
  graphics::par(mar = c(2, 2, 2, 2))
  old_par    <- graphics::par(no.readonly = TRUE)
  old_width  <- getOption("width")
  old_wd     <- getwd()
  old_files  <- list.files(old_wd, all.files = TRUE)
  suppressWarnings(MVPapp:::pdfNCA_wm(f, conc, dose = 100, show_progress = FALSE))
  expect_true(file.exists(f))
  expect_identical(getwd(), old_wd)
  expect_identical(getOption("width"), old_width)
  expect_identical(grDevices::dev.cur(), user_dev)
  expect_identical(graphics::par(no.readonly = TRUE), old_par)
  expect_identical(list.files(old_wd, all.files = TRUE), old_files)
})
