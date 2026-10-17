test_that("covariate databases are found without attaching MVPapp", {
  for (db in c("cdc.expand", "who.expand", "nhanes.filtered")) {
    d <- MVPapp:::mvp_dataset(db)
    expect_s3_class(d, "data.frame")
    expect_gt(nrow(d), 0)
  }
})

test_that("sample_age_wt() samples subjects from each database", {
  for (db in c("CDC", "WHO", "NHANES")) {
    d <- suppressMessages(MVPapp:::sample_age_wt(df_name = db, nsubj = 5,
                                                 lower.agemo = 24, upper.agemo = 60))
    expect_equal(nrow(d), 5)
    expect_true(all(c("ID", "WT", "AGE", "SEX") %in% names(d)))
    expect_false(anyNA(d$WT))
  }
})
