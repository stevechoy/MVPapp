test_that("Model 2 simulation with a covariate database has no missing covariates (#28)", {
  skip_on_cran() # compiles an mrgsolve model
  mod <- mvp_test_model()
  db  <- suppressMessages(MVPapp:::sample_age_wt(df_name = "NHANES", nsubj = 8))
  out <- suppressWarnings(MVPapp:::run_single_sim(
    mod, ev_df = mrgsolve::ev(amt = 100, ii = 12, addl = 2),
    sampling_times = mrgsolve::tgrid(0, 36, 1), nsubj = 8, nsubj_offset = 20,
    append_id_text = "m2-", ext_db = db))
  expect_equal(sort(unique(out$ID)), 21:28)
  expect_false(anyNA(out$CP))
  expect_false(anyNA(out$WT))
})

test_that("each model keeps its own dosing events for the bundle download (#29)", {
  skip_on_cran() # compiles an mrgsolve model
  mod <- mvp_test_model()
  st  <- mrgsolve::tgrid(0, 24, 1)
  MVPapp:::run_single_sim(mod, ev_df = mrgsolve::ev(amt = 100), sampling_times = st,
                          save_events_as = "model_1")
  MVPapp:::run_single_sim(mod, ev_df = mrgsolve::ev(amt = 999), sampling_times = st,
                          append_id_text = "m2-", save_events_as = "model_2")
  MVPapp:::run_single_sim(mod, ev_df = mrgsolve::ev(amt = 5), sampling_times = st) # not saved
  saved <- MVPapp:::.mvp_state$last_ev_df
  expect_equal(unique(saved$model_1$amt), 100)
  expect_equal(unique(saved$model_2$amt), 999)
})
