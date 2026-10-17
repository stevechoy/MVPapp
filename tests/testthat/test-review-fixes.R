# Regression tests for bugs found in the pre-CRAN code review

with_mock_session <- function(expr) {
  shiny::withReactiveDomain(shiny::MockShinySession$new(), expr)
}

test_that("gm_mean() ignores NA, zero and negative values", {
  expect_equal(MVPapp:::gm_mean(c(1, 100, NA)), 10)
  expect_equal(MVPapp:::gm_mean(c(1, 100, 0)), 10)
  expect_equal(MVPapp:::gm_mean(c(2, 8)), 4)
  expect_true(is.na(MVPapp:::gm_mean(c(NA, NA))))
})

test_that("value boxes pick the exact metric column (CFBPCT vs MEANCFBPCT)", {
  df <- data.frame(DV_CFBPCT_ranged = 10, DV_MEANCFBPCT_ranged = 36)
  box_value <- function(metric) {
    html <- as.character(MVPapp:::create_value_box(df, metric, "x", color = "olive"))
    as.numeric(sub(".*<p[^>]*>([-0-9.]+)</p>.*", "\\1", gsub("\n", "", html)))
  }
  expect_equal(box_value("CFBPCT_ranged"), 10)
  expect_equal(box_value("MEANCFBPCT_ranged"), 36)
})

test_that("sample_age_wt() errors clearly instead of duplicating subjects", {
  expect_error(
    MVPapp:::sample_age_wt("NHANES", nsubj = 50, lower.agemo = 240, upper.agemo = 241,
                           lower.wt = 150, upper.wt = 200),
    "match the selected"
  )
  out <- MVPapp:::sample_age_wt("NHANES", nsubj = 40, lower.agemo = 240, upper.agemo = 480)
  expect_equal(nrow(out), 40)
  expect_equal(anyDuplicated(out[, c("SEX", "AGEMO", "WT", "HT")]), 0)
})

test_that("outlier flags use observation rows only (dose rows with DV = NA)", {
  obs <- data.frame(ID = rep(1:3, each = 3), TIME = rep(1:3, 3), EVID = 0,
                    DV = c(rep(10, 3), rep(10, 3), rep(100, 3)), facet_label = "x", GRP = 1)
  dose <- data.frame(ID = 1:3, TIME = 0, EVID = 1, DV = NA, facet_label = "x", GRP = 1)
  out <- MVPapp:::categorize_outliers(rbind(dose, obs), highlight_range = "50%",
                                      y_axis = "DV", strat_by = "GRP")
  status <- unique(out[, c("ID", "outlier_status")])
  expect_equal(status$outlier_status[status$ID == 3], "Above")
  expect_equal(status$outlier_status[status$ID == 1], "Below")
})

test_that("spider plot ranks parameters by how much they change the metric", {
  df <- data.frame(.paramname = rep(c("A", "B"), each = 3), .ratio = rep(c(0.5, 1, 2), 2),
                   Cmax = c(99, 100, 101, 60, 100, 140))
  p <- MVPapp:::spider_plot(df, reference_value = 100, filter_rows = 1)
  expect_equal(as.character(unique(p$data$Parameter)), "B")
})

test_that("ID/TIME detection keeps a match on the last candidate name", {
  df <- data.frame(PTNO = 1:2, ATSD = c(0, 1), DV = 1:2)
  with_mock_session({
    expect_true("ID" %in% names(MVPapp:::search_id_col(df)))
    expect_true("TIME" %in% names(MVPapp:::search_time_col(df)))
  })
})

test_that("handle_blanks() copes with NA in character columns", {
  df <- data.frame(SITE = c("A", NA, ""), stringsAsFactors = FALSE)
  out <- with_mock_session(MVPapp:::handle_blanks(df, "SITE"))
  expect_equal(out$SITE, c("A", NA, ".(blank)"))
  df2 <- data.frame(SITE = c("A", NA), stringsAsFactors = FALSE) # NA but no blanks
  expect_equal(with_mock_session(MVPapp:::handle_blanks(df2, "SITE"))$SITE, c("A", NA))
})

test_that("NCA subsetting handles quotes in key values without evaluating them", {
  d <- data.frame(ID = c("O'Neil", "O'Neil", "B"), TIME = c(0, 1, 0), DV = 1:3)
  ids <- unique(d[, "ID", drop = FALSE])
  expect_equal(nrow(MVPapp:::nca_subset(d, "ID", ids[1, , drop = FALSE])), 2)
  evil <- data.frame(ID = "1' & {stop('injected')} & 'a", TIME = 0, DV = 1)
  expect_equal(nrow(MVPapp:::nca_subset(evil, "ID", evil[1, "ID", drop = FALSE])), 1)
})

test_that("translated code survives being placed in the editor string", {
  code <- "$MAIN\ndouble x = 1; // it's \"quoted\"\nRcout << \"v\\n\";"
  cleaned <- MVPapp:::clean_llm_response(paste0("```cpp\n", code, "\n```"))
  expect_identical(trimws(eval(parse(text = paste0("\"", cleaned, "\"")))), code)
})

test_that("safe_seed() pauses on an empty seed instead of erroring", {
  expect_equal(MVPapp:::safe_seed(42), 42)
  expect_error(MVPapp:::safe_seed(NA_real_), class = "shiny.silent.error")
  expect_error(MVPapp:::safe_seed(NULL), class = "shiny.silent.error")
})
