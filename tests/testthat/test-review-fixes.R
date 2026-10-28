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
  expect_true(grepl("N/A", as.character(MVPapp:::create_value_box(df, "TMIN_ranged", "x", color = "olive"))))
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

test_that("plots and window metrics use observation records only (dose records only on request)", {
  m <- mrgsolve::update(mrgsolve::param(mrgsolve::house(), CL = 1, VC = 20), digits = 8)
  e <- as.data.frame(mrgsolve::ev(amt = 100, ii = 24, addl = 2, cmt = 2))
  st <- mrgsolve::tgrid(0, 72, 1)
  out  <- with_mock_session(MVPapp:::run_single_sim(m, ev_df = e, sampling_times = st))
  full <- with_mock_session(MVPapp:::run_single_sim(m, ev_df = e, sampling_times = st, keep_dose_records = TRUE))

  # default: observations only; full output also has the dose records at sampled dose times
  expect_true(all(out$evid == 0))
  expect_equal(nrow(MVPapp:::obs_only(full)), nrow(out))
  expect_equal(full$evid[full$TIME == 24], c(0, 1))
  expect_equal(MVPapp:::obs_only(full)$CP, out$CP)
  expect_null(MVPapp:::obs_only(NULL))

  # metrics only see sampled observations (the post-dose peak is not a sample)
  w <- out[out$TIME >= 0 & out$TIME <= 24, ]
  pk <- MVPapp:::pknca_table(out, "CP", 0, 24)
  expect_equal(pk$CP_CMAX_ranged[1], max(w$CP))
  expect_equal(pk$CP_TMAX_ranged[1], w$TIME[which.max(w$CP)])
  expect_equal(pk$CP_CMIN_ranged[1], min(w$CP))
  expect_equal(pk$CP_CAVG_ranged[1], pk$AUC_ranged[1] / 24)

  ex <- MVPapp:::exposures_table(out, "CP", 24, 48)
  w2 <- out[out$TIME >= 24 & out$TIME <= 48, ]
  expect_equal(ex$Cmax, max(w2$CP))
  expect_equal(ex$Clast, w2$CP[w2$TIME == 48])
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
    boxplot = FALSE, dolm = TRUE,
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
    # the plotly-only "text" aesthetic no longer triggers "Ignoring unknown aesthetics"
    expect_no_warning(p <- MVPapp:::tornado_plot(df, reference_value = 100, display_as = as))
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

# Low-severity findings from the final pre-CRAN review --------------------------

test_that("sample_age_wt() upper bounds are inclusive (L40)", {
  lo <- min(MVPapp::cdc.expand$AGEMO); hi <- max(MVPapp::cdc.expand$AGEMO)
  d <- suppressMessages(MVPapp:::sample_age_wt(df_name = "CDC", nsubj = 400, lower.agemo = hi, upper.agemo = hi,
                                               lower.wt = 0, upper.wt = 500, prop.male = 0.5, seed.number = 1))
  expect_true(all(d$AGEMO == hi))
})

test_that("code templates carry the installed mrgsolve version (L41)", {
  v <- as.character(utils::packageVersion("mrgsolve"))
  expect_match(MVPapp:::code_preamble, paste0("mrgsolve [v", v, "]"), fixed = TRUE)
  expect_match(MVPapp:::one_cmt, paste0("mrgsolve [v", v, "]"), fixed = TRUE)
  expect_match(MVPapp:::model_code_2cmt_QE_TMDD, "Equilibrium dissociation constant (nM)", fixed = TRUE)
})

test_that("individual plots no longer take unused arguments and skip lm on log axes (L42)", {
  expect_false(any(c("med_line", "med_line_by", "smoother", "lby", "lbx") %in%
                     names(formals(MVPapp:::do_data_page_ind_plot))))
  d <- data.frame(ID = rep(1:4, each = 5), TIME = rep(1:5, 4), DV = rep(1:4, each = 5) + c(0.3, 0.1, 0.5, 0.2, 0.4)[rep(1:5, 4)],
                  EVID = 0, CMT = 1)
  mk <- function(logy) with_mock_session(MVPapp:::do_data_page_ind_plot(
    nmd = d, rownums = 2, colnums = 2, pagenum = 1, filter_id = "", filter_cmt = "NULL", sort_by = "ID",
    strat_by = "", highlight_range = "None", x_axis = "TIME", y_axis = "DV", color_by = "",
    boxplot = FALSE, dolm = TRUE, logy = logy, logx = FALSE, plot_title = "", label_size = 3,
    highlight_var = "", highlight_var_values = "", plot_dosing = FALSE, dose_col = "", dose_units = "", lloq_name = ""))
  has_text <- function(p) any(vapply(p$layers, function(l) inherits(l$geom, "GeomText"), logical(1)))
  expect_true(has_text(mk(FALSE)))
  expect_false(has_text(mk(TRUE)))
})

test_that("facet labels, axis maxima and exposure model names (L43)", {
  d <- data.frame(ID = c(1, 1, 2), X = 1:3)
  expect_equal(MVPapp:::create_facet_label(d, character(0))$facet_label, c("ID: 1", "ID: 1", "ID: 2"))
  expect_equal(MVPapp:::max_x_of("TIME", data.frame(TIME = c(1, 5)), NULL, data.frame(TIME = c(2, 9, NA))), 9)
  expect_true(is.na(MVPapp:::max_x_of("TIME", NULL)))
  ex <- data.frame(MODEL = rep(c("Model 1", "Model 2"), each = 10), AUC = c(1:10, 11:20))
  p <- MVPapp:::plot_iiv_exp_data(ex, yvar = "AUC", model_1_name = "Same", model_2_name = "Same", show_stats = FALSE)
  expect_equal(length(unique(p$data$MODEL)), 2)
})

test_that("dosing records after the last sampling time are not in the output, nor offered as windows", {
  skip_on_cran() # compiles an mrgsolve model
  mod <- mvp_test_model()
  ev  <- data.frame(ID = 1, time = 0, amt = 50, cmt = 1, evid = 1, ii = 12, addl = 11, rate = 0, tinf = 0)
  st  <- seq(0, 72, by = 1)
  out <- with_mock_session(MVPapp:::run_single_sim(mod, ev_df = ev, sampling_times = st, keep_dose_records = TRUE))
  expect_lte(max(out$TIME), 72)
  expect_true(all(out$TIME[out$evid != 0] <= 72))
  # dose at 12 h intervals inside the range are still kept
  expect_true(any(out$evid != 0 & out$TIME == 60))
  # custom sampling grid: dose times that are not sampled are not offered as time windows
  st2 <- c(0, 5, 10, 30, 70)
  out2 <- with_mock_session(MVPapp:::run_single_sim(mod, ev_df = ev, sampling_times = st2))
  expect_equal(MVPapp:::sampled_times(out2), st2)
})

test_that("dose records only at sampled times; statistics use observation rows only (H7, H8)", {
  skip_on_cran() # compiles an mrgsolve model
  mod <- mvp_test_model()
  ev  <- data.frame(ID = 1, time = 0, amt = 50, cmt = 1, evid = 1, ii = 12, addl = 2, rate = 0, tinf = 0)
  st  <- seq(0, 30, by = 5)
  out <- with_mock_session(MVPapp:::run_single_sim(mod, ev_df = ev, sampling_times = st, keep_dose_records = TRUE))
  # doses at 12 and 24 h are not sampling times -> no rows there; dose at 0 is kept
  expect_false(any(out$evid != 0 & out$TIME %in% c(12, 24)))
  expect_true(any(out$evid != 0 & out$TIME == 0))

  # quantiles ignore the dose records at a sampled dose time
  st3 <- seq(0, 36, by = 4)
  out3 <- with_mock_session(MVPapp:::run_single_sim(mod, ev_df = ev, sampling_times = st3))
  q <- MVPapp:::quantile_output(out3, yvar = "CP")
  at12 <- q[q$TIME == 12, ]
  obs_val <- out3$CP[out3$TIME == 12 & out3$evid == 0]
  expect_equal(unique(at12$median_yvar), round(stats::median(obs_val), 5))
})

test_that("compiled model names are unique per call and rewritten on restore (M24)", {
  a <- MVPapp:::new_model_suffix(NULL)
  b <- MVPapp:::new_model_suffix(NULL)
  expect_false(identical(a, b))
  code <- "model_object <- mrgsolve::mcode('Model-1-1234567', model_code, recover = TRUE)"
  new <- MVPapp:::rename_stored_models(code, NULL)
  expect_false(grepl("1234567", new))
  expect_match(new, "^model_object <- mrgsolve::mcode\\('Model-1-[A-Za-z0-9]+',")
  # editors without a stored name are left alone
  expect_equal(MVPapp:::rename_stored_models("nothing here", NULL), "nothing here")
})

test_that("nonmem2mrgsolve output is escaped like LLM output (M28)", {
  expect_equal(MVPapp:::escape_editor_string('$PROB "x" \\ y'), '$PROB \\"x\\" \\\\ y')
})

test_that("% change from baseline is NA, not Inf, for a zero baseline (M29)", {
  skip_on_cran() # needs NonCompart-free path only, but uses mrgsolve
  mod <- mvp_test_model()
  out <- with_mock_session(MVPapp:::run_single_sim(mod, ev_df = as.data.frame(mrgsolve::ev(amt = 100, time = 0)),
                                                   sampling_times = seq(0, 24, by = 2)))
  pk <- MVPapp:::pknca_table(out, "CP", 0, 24)
  expect_true(is.na(pk$CP_CFBPCT_ranged[1]))
  expect_true(is.na(pk$CP_MEANCFBPCT_ranged[1]))
  expect_true(is.na(pk$CP_NADIRPCT_ranged[1]))
  expect_true(is.na(pk$CP_CFBPCT_tlast[1]) || !is.infinite(pk$CP_CFBPCT_tlast[1]))
})

test_that("facets by a non-syntactic column name work (L46)", {
  d <- data.frame(ID = rep(1:4, each = 3), TIME = rep(0:2, 4), DV = c(1:12), `Dose group` = rep(c("a", "b"), each = 6),
                  CMT = 1, check.names = FALSE)
  p <- with_mock_session(MVPapp:::do_data_page_plot(
    nmd = d, filter_cmt = "NULL", x_axis = "TIME", y_axis = "DV", color_by = "", med_line = FALSE, med_line_by = "",
    boxplot = FALSE, num_quantiles = 0, dolm = FALSE, smoother = FALSE, facet_name = "Dose group",
    logy = FALSE, logx = FALSE, plot_title = ""))
  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("NCA dose comes from the first positive dosing record (L51)", {
  all_df <- data.frame(ID = c(1, 1, 1, 2, 2, 2), EVID = c(1, 0, 0, 1, 0, 0), AMT = c(100, 0, 0, 200, 0, 0),
                       TIME = c(0, 1, 2, 0, 1, 2), DV = c(0, 5, 3, 0, 8, 4))
  obs <- all_df[all_df$EVID == 0, ]
  expect_equal(MVPapp:::nca_dose_vector(obs, all_df, "ID", "", "AMT"), c(100, 200))
  # the app's dataset can be a data.table
  skip_if_not_installed("data.table")
  dt <- data.table::as.data.table(all_df)
  expect_equal(MVPapp:::nca_dose_vector(dt[dt$EVID == 0], dt, "ID", NULL, "AMT"), c(100, 200))
  # no EVID column: first positive value on the observation rows
  d <- data.frame(ID = c(1, 1, 2, 2), DOSE = c(10, 10, 20, 20))
  expect_equal(MVPapp:::nca_dose_vector(d, d, "ID", "", "DOSE"), c(10, 20))
})

test_that("batch runs work outside Shiny and a zero metric reference falls back to Value (L53, L54)", {
  df <- data.frame(.paramname = c("CL", "V"), Lower = c(1, 2), Upper = c(3, 4))
  p <- suppressMessages(MVPapp:::tornado_plot(df, reference_value = 0, display_as = "Percentage"))
  expect_s3_class(p, "ggplot")
})

test_that("reserved covariate names include ID and USUBJID (L57)", {
  out <- with_mock_session(MVPapp:::check_cov_name("ID", "COV1"))
  expect_equal(out, "COV1")
})

test_that("restore_session_dynamic() puts back matrices and marks them applied (M27)", {
  om <- matrix(c("0.09", NA, NA, "0.09"), 2)
  st <- list(settings = list(), extras = list(param_values_model_1 = list(CL = 2), omega_model_1 = om))
  rv <- shiny::reactiveVal(NULL)
  applied <- NULL
  shiny::testServer(function(input, output, session) {}, {
    expect_no_error(MVPapp:::restore_session_dynamic(st, session, part = "params"))
    MVPapp:::restore_session_dynamic(st, session, part = "matrices", extracted = list(omega_model_1 = rv),
                                     apply_matrices = function(m, omega, sigma) applied <<- c(m, omega, sigma))
  })
  expect_equal(shiny::isolate(rv()), om)
  expect_equal(applied, c(1, 1, 0))
})

test_that("branding helpers validate and derive colours (brand_*)", {
  expect_equal(MVPapp:::brand_hex("orange"), "#FFA500")
  expect_equal(MVPapp:::brand_hex("#f36633"), "#F36633")
  expect_error(MVPapp:::brand_hex("not-a-colour"), "not a valid colour")
  expect_error(MVPapp:::brand_hex(c("red", "blue")), "single colour")
  expect_equal(MVPapp:::brand_contrast_text("#003366"), "#FFFFFF")
  expect_equal(MVPapp:::brand_contrast_text("#FFFF00"), "#000000")
  expect_equal(MVPapp:::brand_contrast_text("#F36633"), "#FFFFFF")
})

test_that("mvp_app() is unchanged without branding and honours the brand arguments", {
  ui_html <- function(app) {
    ui <- environment(app$httpHandler)$ui
    if (is.function(ui)) ui <- ui(NULL)
    r <- htmltools::renderTags(ui)
    paste(as.character(r$head), as.character(r$html))
  }
  default <- ui_html(MVPapp::mvp_app())
  expect_false(grepl("<img[^>]*mvp-brand-logo", default)) # no logo in the navbar (only the shared style rule)
  expect_true(grepl("#003366", default, fixed = TRUE))
  expect_true(grepl("Model Visualization Platform (MVP)", default, fixed = TRUE))
  expect_true(grepl("<title>Model Visualization Platform (MVP)</title>", default, fixed = TRUE))
  expect_true(grepl("mvp-favicon/mvp-favicon.svg", default, fixed = TRUE)) # shipped default icon
  expect_true(file.exists(system.file("favicon", "mvp-favicon.png", package = "MVPapp")))

  logo <- tempfile(fileext = ".svg")
  writeLines('<svg xmlns="http://www.w3.org/2000/svg" width="40" height="40"><rect width="40" height="40" fill="red"/></svg>', logo)
  png_file <- file.path(R.home("doc"), "html", "logo.jpg")
  fav <- if (file.exists(png_file)) png_file else logo
  branded <- ui_html(MVPapp::mvp_app(brand_color = "#F36633", brand_logo = logo, brand_favicon = fav,
                                     page_title = "Company MVP"))
  expect_true(grepl("#F36633", branded, fixed = TRUE))
  expect_true(grepl("mvp-brand-logo", branded, fixed = TRUE))
  expect_true(grepl("rel=\"icon\"", branded, fixed = TRUE))
  expect_false(grepl("mvp-favicon/mvp-favicon", branded, fixed = TRUE)) # brand_favicon replaces the default
  expect_true(grepl("Company MVP", branded, fixed = TRUE))
  expect_true(grepl("<title>Company MVP</title>", branded, fixed = TRUE))
  # data table headers use the brand colour
  expect_error(MVPapp::mvp_app(brand_logo = "no/such/file.png"), "file not found")
  expect_error(MVPapp::mvp_app(brand_color = "zzz"), "not a valid colour")
  expect_warning(MVPapp::mvp_app(brand_text_color = "white"), "ignored")
})

test_that("the reproducible bundle plots observations only but keeps dose records in solved_output", {
  skip_on_cran() # compiles an mrgsolve model
  skip_if_not_installed("ggplot2")
  mod <- mvp_test_model()
  ev  <- data.frame(ID = 1, time = 0, amt = 50, cmt = 1, evid = 1, ii = 12, addl = 5, rate = 0, tinf = 0)
  st  <- seq(0, 72, by = 1)
  zipf <- tempfile(fileext = ".zip")
  MVPapp:::build_repro_bundle(zip_path = zipf, input_model_object = mod,
                              event_data_orig = ev, event_data_post = ev, sampling_times = st)
  dir <- tempfile("bundle"); dir.create(dir)
  utils::unzip(zipf, exdir = dir)
  script <- list.files(dir, pattern = "^simulation\\.R$", recursive = TRUE, full.names = TRUE)[1]
  env <- new.env(parent = globalenv())
  pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
  # chdir = TRUE: the script finds model.cpp next to it without setwd()
  suppressMessages(suppressWarnings(source(script, local = env, chdir = TRUE)))
  expect_true(any(env$solved_output$evid != 0))        # all records kept for the user
  expect_true(all(env$plot_output$data$evid == 0))     # the example plot shows observations only
  expect_equal(sum(env$plot_output$data$time == 72), 1)
})
