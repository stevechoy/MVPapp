# Tests for the helpers introduced when simplifying the code before CRAN (#77)

with_mock_session <- function(expr) {
  shiny::withReactiveDomain(shiny::MockShinySession$new(), expr)
}

test_that("trapezoid_auc() and conc_metrics() give the hand-calculated values", {
  time <- c(0, 1, 2, 4)
  y    <- c(10, 8, 6, 2)
  expect_equal(MVPapp:::trapezoid_auc(time, y), (10 + 8) / 2 + (8 + 6) / 2 + (6 + 2) / 2 * 2)
  expect_equal(MVPapp:::trapezoid_auc(time, c(10, NA, 6, 2)), 8) # intervals with a missing value count as 0

  m <- MVPapp:::conc_metrics(time, y, baseline = 10)
  expect_equal(c(m$CMIN, m$CMAX, m$TMIN, m$TMAX), c(2, 10, 4, 0))
  expect_equal(m$AUC, 24)
  expect_equal(m$CAVG, 24 / 4)
  expect_equal(m$CFBPCT, c(0, -20, -40, -80))
  expect_equal(m$NADIRPCT, -80)
  expect_equal(m$MEANCFBPCT, MVPapp:::trapezoid_auc(time, m$CFBPCT) / 4)
  expect_true(is.na(MVPapp:::conc_metrics(time, y, baseline = 0)$MEANCFBPCT)) # undefined for a zero baseline
})

test_that("pknca_table() adds the whole-simulation and window metrics", {
  df <- data.frame(USUBJID = "m1-1", ID = 1, TIME = c(0, 1, 2, 4), evid = 0, tad = 0, TIMEADJ = c(0, 1, 2, 4),
                   CP = c(10, 8, 6, 2))
  pk <- with_mock_session(MVPapp:::pknca_table(df, "CP", 1, 2))
  expect_equal(nrow(pk), 4)
  expect_false(any(c("ID", "tad", "TIMEADJ") %in% names(pk)))
  expect_equal(unique(pk$AUC_tlast), 24)
  expect_equal(unique(pk$AUC_ranged), 7)
  expect_equal(unique(pk$CP_CMAX_ranged), 8)
  expect_equal(unique(pk$CP_TMIN_ranged), 2)
  expect_equal(unique(pk$CP_CFBPCT_ranged), -40) # % change at the end of the window
  expect_equal(pk$CP_CFBPCT_tlast, c(0, -20, -40, -80))
  # dose records are left out
  pk2 <- with_mock_session(MVPapp:::pknca_table(rbind(df, transform(df[2, ], evid = 1, CP = 99)), "CP", 0, 4))
  expect_equal(unique(pk2$CP_CMAX_tlast), 10)
})

test_that("exposures_table() gives one row per subject with its window metrics", {
  df <- data.frame(USUBJID = rep(c("a", "b"), each = 3), ID = rep(1:2, each = 3), TIME = rep(c(0, 1, 2), 2),
                   CP = c(1, 3, 2, 5, 4, 0), WT = rep(c(60, 80), each = 3), evid = 0)
  ex <- MVPapp:::exposures_table(df, "CP", 0, 2, carry_out = c("ID", "WT"))
  expect_equal(ex$USUBJID, c("a", "b"))
  expect_equal(ex$Cmax, c(3, 5))
  expect_equal(ex$Tmax, c(1, 0))
  expect_equal(ex$Clast, c(2, 0))
  expect_equal(ex$AUC, c(4.5, 6.5))
  expect_equal(ex$WT, c(60, 80))
  expect_equal(names(ex), c("USUBJID", "ID", "Cmin", "Cmax", "Cavg", "Clast", "AUC", "Tmax", "Tmin", "WT"))
})

test_that("calculate_quantiles() and categorize_xvar() bin a continuous variable", {
  df <- data.frame(WT = c(40, 50, 60, 70, 80, NA))
  q  <- MVPapp:::calculate_quantiles(df, "WT", 2)
  expect_equal(names(q), c("Q1", "Q2"))
  expect_equal(unlist(q, use.names = FALSE), c(60, 80))
  out <- MVPapp:::categorize_xvar(df, q, "WT")
  expect_equal(levels(out$Quantile), c("NA", "Q1\n[40-60]", "Q2\n(60-80]"))
  expect_equal(as.character(out$Quantile), c(rep("Q1\n[40-60]", 3), rep("Q2\n(60-80]", 2), "NA"))
})

test_that("sanitize_numeric_input() replaces bad values", {
  s <- function(...) MVPapp:::sanitize_numeric_input(...)
  expect_equal(s(NA), 0)
  expect_equal(s("a", allow_zero = FALSE), 1)
  expect_equal(s(-2), 0)
  expect_equal(s(0, allow_zero = FALSE, return_value = 7), 7)
  expect_equal(s(2.7, as_integer = TRUE), 2L)
  expect_equal(s(100, legal_maximum = 50), 50)
  expect_equal(s(1, legal_minimum = 3), 3)
  expect_equal(s(0, legal_minimum = 3), 3) # 0 below the legal minimum
  expect_equal(s(5, legal_maximum = 50, legal_minimum = 3), 5)
  expect_equal(s(NULL), 0)
})

test_that("dosing regimens are read from the app inputs and combined", {
  input <- list()
  for (i in 1:5) for (m in 1:2) { # every regimen present, only the first of each model given
    sfx <- if (m == 2) "_2" else ""
    input[[paste0("amt", i, sfx)]]           <- if (i == 1) 100 * m else 0
    input[[paste0("delay_time", i, sfx)]]    <- 0
    input[[paste0("cmt", i, "_model_", m)]]  <- m
    input[[paste0("tinf", i, sfx)]]          <- 0
    input[[paste0("total", i, sfx)]]         <- if (i == 1) 3 else 0
    input[[paste0("ii", i, sfx)]]            <- 24
  }
  r1 <- MVPapp:::dosing_regimens_from_input(input, model = 1)
  expect_length(r1, 5)
  expect_equal(r1[[1]], list(amt = 100, delay_time = 0, cmt = 1, tinf = 0, total = 3, ii = 24))
  expect_equal(MVPapp:::dosing_regimens_from_input(input, model = 2)[[1]][c("amt", "cmt")], list(amt = 200, cmt = 2))

  ev <- as.data.frame(MVPapp:::generate_dosing_regimens(r1, mw_conversion = 2))
  expect_equal(nrow(ev), 1)
  expect_equal(c(ev$amt, ev$addl, ev$ii), c(200, 2, 24))
  no_dose <- lapply(r1, function(r) { r$amt <- 0; r })
  dummy <- as.data.frame(MVPapp:::generate_dosing_regimens(no_dose))
  expect_equal(dummy$amt, 0) # a dose of 0 when there is none
})

test_that("transform_ev_df() applies weight-based dosing and modelled rate / duration", {
  skip_on_cran() # compiles an mrgsolve model
  mod <- mvp_test_model()
  ev  <- mrgsolve::ev(amt = 10, tinf = 2)
  wt  <- as.data.frame(MVPapp:::transform_ev_df(mod, ev, FALSE, FALSE, FALSE, wt_based_dosing = TRUE))
  expect_equal(wt$amt, 700) # model WT = 70
  dur <- as.data.frame(MVPapp:::transform_ev_df(mod, ev, TRUE, FALSE, FALSE))
  expect_equal(dur$rate, -2)
  expect_false("tinf" %in% names(dur))
  expect_equal(as.data.frame(MVPapp:::transform_ev_df(mod, ev, FALSE, TRUE, FALSE))$rate, -1)
  pred <- as.data.frame(MVPapp:::transform_ev_df(mod, ev, FALSE, FALSE, TRUE))
  expect_equal(pred$cmt, 0)
})

test_that("expand_addl_ii() writes out additional doses", {
  d <- data.frame(ID = c(1, 2), TIME = c(0, 5), AMT = c(10, 20), EVID = 1, ADDL = c(2, NA), II = c(12, NA),
                  facet_label = c("ID: 1", "ID: 2"), NAMT = c(10, 20), SAMT = 1, min_yvar = 0)
  out <- MVPapp:::expand_addl_ii(d, "TIME", "AMT")
  expect_equal(out$DOSETIME, c(0, 12, 24, 5))
  expect_equal(out$ID, c(1, 1, 1, 2))
})

test_that("update_batch_run_table() updates the right bounds", {
  tab <- data.frame(Name = c("CL", "V"), Reference = c("1", "10"), Lower = c("0.1", "1"), Upper = c("9", "90"))
  all_rows <- with_mock_session(MVPapp:::update_batch_run_table(tab, 0.5, 2))
  expect_equal(all_rows$Lower, c("0.5", "5"))
  one_row <- with_mock_session(MVPapp:::update_batch_run_table(tab, 0.5, 2, last_change_ref_index = 2))
  expect_equal(one_row$Lower, c("0.1", "5"))
  kept <- with_mock_session(MVPapp:::update_batch_run_table(tab, 0.5, 2, length_lower_change = 1))
  expect_equal(kept$Lower, tab$Lower)
})

test_that("iterate_batch_runs() labels each run", {
  skip_on_cran() # compiles an mrgsolve model
  mod <- mvp_test_model()
  bdf <- data.frame(Name = "CL", Reference = "1", Lower = "0.5", Upper = "2")
  out <- MVPapp:::iterate_batch_runs(bdf, mod, ev_df = mrgsolve::ev(amt = 100), sampling_times = c(0, 1, 2),
                                     show_progress = FALSE, gradient = TRUE)
  runs <- unique(out[, c(".desc", ".cat", ".value", ".ratio", "ID")])
  expect_equal(runs$.desc, c("Reference", "lower CL", "lower CL (Q1)", "lower CL (Q2)", "lower CL (Q3)",
                             "upper CL (Q1)", "upper CL (Q2)", "upper CL (Q3)", "upper CL"))
  expect_equal(runs$.cat[2:3], c("Lower", "Lower (Q1)"))
  expect_equal(runs$.value, c(-99, 0.5, 0.625, 0.75, 0.875, 1.25, 1.5, 1.75, 2))
  expect_equal(runs$.ratio[9], 2)
  expect_equal(runs$ID, 1:9) # one ID per run
})

test_that("lm_eqn() fits once per group and labels the line", {
  d <- data.frame(g = rep(c("a", "b"), each = 3), x = rep(1:3, 2), y = c(2, 4, 6, 1, 1, 1))
  out <- suppressWarnings(MVPapp:::lm_eqn(d, "g", "x", "y"))
  expect_equal(out$slope, c(2, 0), tolerance = 1e-10)
  expect_equal(out$intercept, c(0, 1), tolerance = 1e-10)
  # Only the shape of the label: the number formatting (e.g. 2 vs 2.0e+00) depends on
  # floating-point noise in the fitted intercept, which differs between platforms
  expect_match(out$label[1], "^y = .* \\+ .*\u00B7x, R\u00B2 = +1$")
  expect_equal(MVPapp:::lm_eqn(d, "", "x", "x")$rsq, 1)
})

test_that("derive_column() creates ID / TIME from the first available column", {
  df <- with_mock_session(MVPapp:::search_id_col(data.frame(A = 1, SUBJID = 2, PTNO = 3)))
  expect_equal(names(df)[1], "ID")
  expect_equal(df$ID, 2)
  expect_equal(MVPapp:::search_time_col(data.frame(TIME = 1, TAFD = 2))$TIME, 1) # existing column kept
  expect_equal(names(MVPapp:::search_time_col(data.frame(X = 1))), "X")
})

test_that("tornado and spider plots share the metric scale", {
  s <- MVPapp:::sensitivity_scale("Percentage", 10, "AUC")
  expect_equal(s$transform(c(5, 15)), c(-0.5, 0.5))
  expect_equal(s$label, "AUC (% Change From Reference)")
  expect_equal(MVPapp:::sensitivity_scale("Ratio", 10, "AUC")$transform(20), 2)
  expect_equal(suppressMessages(MVPapp:::sensitivity_scale("Ratio", 0, "AUC"))$display_as, "Value") # undefined at 0

  p <- MVPapp:::add_sensitivity_y_axis(ggplot2::ggplot(), c(-0.5, 0.5), s, bioeq_lines = TRUE, max_ticks = 12, value_breaks = 10)
  expect_equal(p$scales$get_scales("y")$breaks, seq(-0.5, 0.5, 0.1))
  expect_equal(p$layers[[1]]$data$yintercept, c(-0.2, 0.25))
})

test_that("OMEGA matrices survive the extract / reconstruct round trip", {
  skip_on_cran() # compiles an mrgsolve model
  mod <- suppressMessages(mrgsolve::mcode("refactor_omega", "$PARAM CL = 1\n$CMT C\n$OMEGA @block\n0.1 0.02 0.2\n$OMEGA 0.05\n$ODE dxdt_C = -CL*C;", quiet = TRUE))
  tab <- MVPapp:::extract_matrix(mod, "omega")
  expect_equal(dim(tab), c(3, 3))
  expect_true(is.na(tab[1, 2])) # upper triangle hidden
  blocks <- MVPapp:::reconstruct_matrices(mod, tab, "omega")
  expect_equal(blocks[[1]], matrix(c(0.1, 0.02, 0.02, 0.2), 2))
  expect_equal(blocks[[2]], matrix(0.05))
})

test_that("LLM helpers pick models, prompts and keys", {
  models <- list(model_anthropic = "claude-x", model_apollo = "ap")
  expect_equal(MVPapp:::llm_model_name("Claude", models), "claude-x")
  expect_equal(MVPapp:::llm_model_name("Test Provider", models), "ap")
  expect_equal(MVPapp:::llm_model_name("PROD", models), "[PROD model]")

  env <- new.env()
  env$mrgsolve_refine_system_prompt    <- "S"
  env$mrgsolve_refine_long_user_prompt <- "L"
  expect_equal(MVPapp:::llm_prompts("mrgsolve_refine", env), list(system = "S", long = "L", short = NULL))

  old_key <- Sys.getenv("ANTHROPIC_API_KEY", unset = NA)
  Sys.setenv(ANTHROPIC_API_KEY = "abc")
  on.exit(if (is.na(old_key)) Sys.unsetenv("ANTHROPIC_API_KEY") else Sys.setenv(ANTHROPIC_API_KEY = old_key), add = TRUE)
  expect_equal(MVPapp:::get_api_key("Claude"), "abc")
  expect_equal(MVPapp:::get_api_key("AWS Bedrock"), "dummy")
})

test_that("Dify requests continue the conversation only when asked", {
  skip_if_not_installed("httr2")
  skip_if_not_installed("curl")
  sent <- list()
  mock <- function(req) {
    sent[[length(sent) + 1]] <<- req$body$data
    httr2::response_json(body = list(answer = "ok", conversation_id = "c1"))
  }
  httr2::with_mocked_responses(mock, {
    MVPapp:::run_dify_chat("Q", "K", api_chat = "https://x", user_id = "u", model_name = "m", debug = FALSE)
    MVPapp:::relay_llm_success(list(conversation_id = "c0"), "PROD", "K", "https://x", "u")
  })
  expect_null(sent[[1]]$conversation_id)
  expect_equal(sent[[2]]$conversation_id, "c0")
  expect_equal(sent[[2]]$query, "Model compiled successfully!")
  expect_equal(length(sent[[2]]$inputs), 0) # sent as an empty JSON object
})

test_that("individual plots draw one dose bar per dose, as wide as the infusion", {
  d <- data.frame(ID = 1, TIME = c(0, 1, 24, 30), DV = c(0, 5, 1, 4), EVID = c(1, 0, 1, 0), AMT = c(100, 0, 100, 0),
                  CMT = 1, ADDL = c(1, 0, 0, 0), II = c(24, 0, 0, 0))
  bars <- function(nmd) {
    p <- with_mock_session(MVPapp:::do_data_page_ind_plot(
      nmd, rownums = 1, colnums = 1, filter_id = "", filter_cmt = "NULL", sort_by = NULL, strat_by = "",
      highlight_range = "80%", x_axis = "TIME", y_axis = "DV", color_by = "", boxplot = FALSE, dolm = FALSE,
      logy = FALSE, logx = FALSE, plot_title = NULL, highlight_var = "", highlight_var_values = "",
      dose_col = "AMT", dose_units = ""))
    rect <- ggplot2::layer_data(p, which(vapply(p$layers, function(l) inherits(l$geom, "GeomRect"), logical(1))))
    rect[order(rect$xmin), c("xmin", "xmax")]
  }
  no_rate <- bars(d) # doses at 0, 24 (ADDL of the first record) and 24
  expect_equal(no_rate$xmin, c(0, 24, 24))
  expect_equal(no_rate$xmax, no_rate$xmin)
  infusion <- bars(transform(d, RATE = c(50, 0, 50, 0)))
  expect_equal(infusion$xmax - infusion$xmin, rep(2, nrow(infusion))) # 100 / 50
})

test_that("a short last PDF page keeps the panel size of a full page", {
  d <- data.frame(ID = rep(1:10, each = 3), TIME = rep(0:2, 10), DV = rep(c(5, 3, 1), 10), EVID = 0, AMT = 0)
  page <- function(i) with_mock_session(MVPapp:::do_data_page_ind_plot(
    d, rownums = 3, colnums = 3, pagenum = i, filter_id = "", filter_cmt = "NULL", sort_by = NULL, strat_by = "",
    highlight_range = "80%", x_axis = "TIME", y_axis = "DV", color_by = "", boxplot = FALSE, dolm = FALSE,
    logy = FALSE, logx = FALSE, plot_title = NULL, highlight_var = "", highlight_var_values = "",
    plot_dosing = FALSE, dose_col = "AMT", dose_units = ""))
  # onefile = FALSE writes one PDF file per page, so the files can be counted
  dir <- tempfile("pages")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))
  grDevices::pdf(file.path(dir, "page%03d.pdf"), width = 6, height = 8, onefile = FALSE)
  seen <- integer(0)
  MVPapp:::print_ind_pages(lapply(1:2, page), rownums = 3, on_page = function(i, n) seen <<- c(seen, i))
  grDevices::dev.off()
  expect_equal(seen, 1:2)
  # one PDF page per plot (no blank first page)
  expect_length(list.files(dir, pattern = "\\.pdf$"), 2)
})

test_that("the watermark is drawn with linear and log axes and left out of plotly", {
  d <- data.frame(x = c(1, 10, 100), y = c(1, 10, 100))
  base <- ggplot2::ggplot(d, ggplot2::aes(x, y)) + ggplot2::geom_point()
  has_watermark <- function(x) {
    if (inherits(x, "watermark")) return(TRUE)
    kids <- if (inherits(x, "gtable")) x$grobs else if (inherits(x, "gTree")) x$children else list()
    any(vapply(kids, has_watermark, logical(1)))
  }
  drawn <- function(p) has_watermark(ggplot2::ggplotGrob(p))
  expect_true(drawn(base + MVPapp:::add_watermark(TRUE)))
  expect_true(drawn(MVPapp:::add_log_axes(base, logx = TRUE, logy = TRUE) + MVPapp:::add_watermark(TRUE)))
  expect_false(drawn(MVPapp:::add_log_axes(base, logy = TRUE) + MVPapp:::add_watermark(FALSE)))
  skip_if_not_installed("plotly")
  # ggplotly does not know the watermark layer: it must be removed first, not warned about
  expect_no_warning(MVPapp:::convert_to_plotly_watermark(base + MVPapp:::add_watermark(TRUE)))
})

test_that("run_mvp() opens the browser unless launch.browser says otherwise", {
  seen <- NULL
  fake_run_app <- function() seen <<- as.list(match.call())[-1] # records the arguments it gets
  formals(fake_run_app) <- formals(shiny::runApp)
  testthat::with_mocked_bindings({
    run_mvp()
    expect_true(seen$launch.browser)
    run_mvp(launch.browser = FALSE)
    expect_false(seen$launch.browser)
  }, runApp = fake_run_app, .package = "shiny")
})
