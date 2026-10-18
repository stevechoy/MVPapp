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
  # AUC is stored without a prefix ("AUC_ranged")
  df <- data.frame(AUC_ranged = 250, DV_CMAX_ranged = 12)
  expect_equal(box_value("AUC_ranged"), 250)
  expect_equal(box_value("CMAX_ranged"), 12)
  expect_true(grepl("NA", as.character(MVPapp:::create_value_box(df, "TMIN_ranged", "x", color = "olive"))))
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

test_that("create_facet_label() builds sorted labels and handles empty data", {
  d <- data.frame(ID = c(2, 2, 1, 1, 3), SEX = c("F", "F", "M", "M", "F"), WT = c(60, 60, 80, 80, 55.5))
  out <- MVPapp:::create_facet_label(d, c("SEX", "WT"))
  expect_equal(unique(out$facet_label),
               c("ID: 3, SEX: F, WT: 55.5", "ID: 2, SEX: F, WT: 60", "ID: 1, SEX: M, WT: 80"))
  expect_equal(nrow(MVPapp:::create_facet_label(d[0, ], c("SEX", "WT"))), 0)
  expect_equal(unique(MVPapp:::create_facet_label(d, "ID")$facet_label), c("ID: 2", "ID: 1", "ID: 3"))
})

test_that("nonmem2rx uploads: control stream or listing plus supporting files", {
  skip_if_not(utils::packageVersion("testthat") >= "3.1.7") # local_mocked_bindings()
  local_mocked_bindings(nonmem2rx_available = function() TRUE, .package = "MVPapp")
  src <- tempfile("uploads_"); dir.create(src)
  on.exit(unlink(src, recursive = TRUE), add = TRUE)
  upload <- function(names) {
    paths <- file.path(src, paste0(seq_along(names), "_tmp"))
    for (k in seq_along(names)) writeLines(names[k], paths[k])   # content = original name
    data.frame(name = names, datapath = paths, stringsAsFactors = FALSE)
  }
  prep <- function(files) with_mock_session(MVPapp:::prepare_uploaded_files(
    files, llm_service = "Claude", model_lang = "rxode2", use_nonmem2mrgsolve = FALSE,
    use_nonmem2rx = TRUE, single_file_types = c(".pdf", ".txt"), multi_file_types = c(".pdf", ".txt")))

  one <- prep(upload("run1.lst"))
  expect_equal(basename(one$primary_path), "run1.lst")

  many <- prep(upload(c("run1.ext", "run1.ctl", "run1.phi", "run1.lst")))
  expect_equal(basename(many$primary_path), "run1.ctl")
  expect_setequal(list.files(many$dir), c("run1.ext", "run1.ctl", "run1.phi", "run1.lst"))
  expect_equal(readLines(file.path(many$dir, "run1.phi")), "run1.phi")  # copied under original name

  expect_null(prep(upload(c("a.ctl", "b.ctl"))))      # two control streams: rejected
  expect_null(prep(upload(c("run1.ext", "run1.phi"))))   # no control stream: not nonmem2rx
})

test_that("window metrics use pre-dose values at the window end and post-dose at its start", {
  m <- mrgsolve::update(mrgsolve::param(mrgsolve::house(), CL = 1, VC = 20), digits = 8)
  e <- as.data.frame(mrgsolve::ev(amt = 100, ii = 24, addl = 2, cmt = 2))
  out <- with_mock_session(MVPapp:::run_single_sim(m, ev_df = e, sampling_times = mrgsolve::tgrid(0, 72, 1)))
  # every dose time has the pre-dose observation followed by the post-dose record
  at24 <- out[out$TIME == 24, ]
  expect_equal(at24$evid, c(0, 1))
  expect_lt(at24$CP[1], at24$CP[2])

  trough <- 5 * exp(-24 / 20)
  pk <- MVPapp:::pknca_table(out, "CP", 0, 24)
  expect_equal(pk$CP_CMAX_ranged[1], 5)
  expect_equal(pk$CP_TMAX_ranged[1], 0)
  expect_equal(pk$CP_CMIN_ranged[1], trough, tolerance = 1e-6)
  expect_equal(pk$AUC_ranged[1], 100 * (1 - exp(-24 / 20)), tolerance = 1e-3)  # exact 69.88
  expect_equal(pk$CP_CAVG_ranged[1], pk$AUC_ranged[1] / 24)

  ex <- MVPapp:::exposures_table(out, "CP", 24, 48)
  expect_equal(ex$Cmax, 5 + trough, tolerance = 1e-6)          # post-dose at the start
  expect_equal(ex$Clast, (5 + trough) * exp(-24 / 20), tolerance = 1e-6) # pre-dose at the end
  expect_equal(ex$Tmax, 24)
  expect_equal(MVPapp:::exposures_table(out, "CP", 24, 24)$Cmax, trough, tolerance = 1e-6)
})

test_that("ADDL doses are written out as separate records", {
  e <- data.frame(ID = 1, time = 0, amt = 100, cmt = 2, evid = 1, ii = 24, addl = 2)
  r <- MVPapp:::realize_dose_records(e)
  expect_equal(r$time, c(0, 24, 48))
  expect_true(all(r$addl == 0))
  expect_identical(MVPapp:::realize_dose_records(transform(e, addl = 0)), transform(e, addl = 0))
})

test_that("variability helpers cope with a model that has no OMEGA", {
  skip_on_cran() # compiles a model
  noiiv <- mrgsolve::mcode("noiiv", "$PARAM CL = 1, V = 10\n$CMT CENT\n$ODE dxdt_CENT = -CL/V*CENT;",
                           quiet = TRUE)
  om <- MVPapp:::extract_matrix(mrgsolve::house(), "omega")
  expect_equal(MVPapp:::reconstruct_matrices(noiiv, om, name_of_matrix = "omega"), list())
  expect_true(mrgsolve::is.mrgmod(MVPapp:::update_variability(noiiv, list(), name_of_matrix = "omega")))
})

test_that("individual plots show one regression label per panel", {
  d <- data.frame(ID = rep(1:4, each = 5), TIME = rep(1:5, 4), DV = rnorm(20) + rep(1:4, each = 5),
                  EVID = 0, CMT = 1)
  p <- with_mock_session(MVPapp:::do_data_page_ind_plot(
    nmd = d, rownums = 2, colnums = 2, pagenum = 1, filter_id = "", filter_cmt = "NULL", sort_by = "ID",
    strat_by = "", highlight_range = "None", x_axis = "TIME", y_axis = "DV", color_by = "",
    med_line = FALSE, med_line_by = "", boxplot = FALSE, dolm = TRUE, smoother = FALSE,
    logy = FALSE, logx = FALSE, plot_title = "", label_size = 3, highlight_var = "",
    highlight_var_values = "", plot_dosing = FALSE, dose_col = "", dose_units = "", lloq_name = ""))
  labels <- Filter(function(l) inherits(l$geom, "GeomText"), p$layers)[[1]]$data
  expect_true("facet_label" %in% names(labels))
  expect_equal(nrow(labels), 4)
})

test_that("OpenAI-Compatible requests use OPENAI_COMPATIBLE_API_KEY", {
  body <- paste(deparse(MVPapp:::run_ellmer_chat), collapse = "\n")
  expect_match(body, 'credentials = function() Sys.getenv("OPENAI_COMPATIBLE_API_KEY")', fixed = TRUE)
})

# Medium-severity findings from the final pre-CRAN review -----------------------

test_that("check_matrix() always checks eigenvalues, with a tolerance (M7, L36)", {
  mod <- list(code = "$OMEGA @correlation @block\n0.1 0.5 0.2") # @correlation no longer skips the check
  bad <- list(matrix(c(1, 2, 2, 1), 2))                          # not positive semi-definite
  expect_false(MVPapp:::check_matrix(mod, bad, display_error = FALSE))
  v <- c(0.3, 0.7, 1.1)
  rank1 <- list(tcrossprod(v))                                   # valid, smallest eigenvalue ~ -1e-17
  expect_true(MVPapp:::check_matrix(mod, rank1, display_error = FALSE))
  expect_false(MVPapp:::check_matrix(mod, list(matrix(NA_real_, 1)), display_error = FALSE))
  expect_true(MVPapp:::check_matrix(mod, list(), display_error = FALSE))
})

test_that("update_variability() keeps the model when omat() refuses the matrix (M7)", {
  wrong_size <- list(diag(0.1, 7)) # house() has a 4 x 4 OMEGA
  out <- suppressWarnings(MVPapp:::update_variability(mrgsolve::house(), wrong_size, check_validity = FALSE))
  expect_true(mrgsolve::is.mrgmod(out))
  expect_equal(mrgsolve::omat(out, make = TRUE), mrgsolve::omat(mrgsolve::house(), make = TRUE))
})

test_that("get_bin_times() copes with a single unique x value (M8)", {
  expect_equal(MVPapp:::get_bin_times(rep(24, 10)), 24)
  expect_equal(MVPapp:::get_bin_times(c(NA, 24, 24)), 24)
  expect_length(MVPapp:::get_bin_times(c(NA_real_, NA_real_)), 0)
  expect_equal(MVPapp:::quantize(c(24, 24), levels = MVPapp:::get_bin_times(c(24, 24))), c(24, 24))
  expect_equal(range(MVPapp:::get_bin_times(c(0, 0, 10, 10))), c(0, 10))
})

test_that("tornado plot works when no parameter changes the metric (M17)", {
  expect_equal(MVPapp:::calculate_tick_size(0), 1)
  expect_equal(MVPapp:::calculate_tick_size(NA_real_), 1)
  expect_equal(MVPapp:::calculate_tick_size(37), 5)
  df <- data.frame(.paramname = c("KA", "CL"), Lower = c(100, 100), Upper = c(100, 100))
  for (as in c("Percentage", "Ratio")) {
    p <- suppressWarnings(MVPapp:::tornado_plot(df, reference_value = 100, display_as = as)) # plotly-only "text" aesthetic
    expect_s3_class(p, "ggplot")
    expect_no_error(suppressWarnings(ggplot2::ggplot_build(p)))
  }
})

test_that("lm_eqn() handles non-syntactic column names (M20)", {
  d <- data.frame(`Conc (ng/mL)` = c(1, 2, 3, 5), `Time (h)` = 1:4, check.names = FALSE)
  out <- MVPapp:::lm_eqn(d, facet_name = NULL, x = "Time (h)", y = "Conc (ng/mL)")
  expect_match(out$label, "R²")
  expect_equal(unname(out$slope), unname(coef(lm(d[[1]] ~ d[[2]]))[2]))
})

test_that("covariates the model also outputs are not duplicated as .x/.y (M21)", {
  skip_on_cran() # compiles an mrgsolve model
  code <- '
$PARAM CL = 1, V = 10, WT = 70
$CMT CENT
$ODE dxdt_CENT = -CL * pow(WT / 70, 0.75) / V * CENT;
$TABLE double CP = CENT / V;
$CAPTURE CP WT
'
  mod <- mrgsolve::mcode("mvp_capture_wt", code, quiet = TRUE)
  db  <- suppressMessages(MVPapp:::sample_age_wt(df_name = "NHANES", nsubj = 5))
  out <- suppressWarnings(MVPapp:::run_single_sim(
    mod, ev_df = mrgsolve::ev(amt = 100), sampling_times = mrgsolve::tgrid(0, 24, 4),
    nsubj = 5, ext_db = db))
  expect_true("WT" %in% names(out))
  expect_false(any(c("WT.x", "WT.y") %in% names(out)))
  expect_true(all(c("AGEMO", "SEX") %in% names(out)) || all(setdiff(names(db), "ID") %in% names(out)))
})

test_that("mean % change from baseline is time-weighted (M16)", {
  skip_on_cran() # compiles an mrgsolve model
  code <- '
$PARAM CL = 1, V = 10, BL = 10
$CMT CENT
$ODE dxdt_CENT = -CL / V * CENT;
$TABLE double Y = BL + CENT / V;
$CAPTURE Y
'
  mod <- mrgsolve::update(mrgsolve::mcode("mvp_baseline", code, quiet = TRUE), digits = 8)
  st  <- c(0, 0.5, 1, 2, 4, 8, 24, 48) # non-uniform sampling
  out <- with_mock_session(MVPapp:::run_single_sim(mod, ev_df = as.data.frame(mrgsolve::ev(amt = 100, time = 1)),
                                                   sampling_times = st))
  pk  <- MVPapp:::pknca_table(out, "Y", 0, 48)
  # % change is linear in Y, so its time-weighted mean follows from Cavg
  expect_equal(pk$Y_MEANCFBPCT_ranged[1], (pk$Y_CAVG_ranged[1] - 10) / 10 * 100, tolerance = 1e-6)
  expect_equal(pk$Y_MEANCFBPCT_tlast[1],  (pk$Y_CAVG_tlast[1]  - 10) / 10 * 100, tolerance = 1e-6)
  obs <- out[out$evid == 0, ]
  expect_false(isTRUE(all.equal(pk$Y_MEANCFBPCT_ranged[1], mean((obs$Y - 10) / 10 * 100))))
})
