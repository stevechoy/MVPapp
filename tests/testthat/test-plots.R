test_that("individual profiles are drawn one line per subject, in the model's colour (#27)", {
  d <- expand.grid(ID = 1:4, TIMEADJ = 0:10)
  d$yvar <- exp(-0.1 * d$ID * d$TIMEADJ)
  for (v in c("median_yvar", "mean_yvar", "lower_yvar", "upper_yvar")) d[[v]] <- d$yvar
  p <- MVPapp:::plot_iiv_data_with_nm(input_dataset1 = d, xvar = "TIMEADJ", yvar = "yvar",
                                      show_ind_profiles = TRUE, line_color_1 = "#F8766D",
                                      y_median = "median_yvar", y_min = "lower_yvar",
                                      y_max = "upper_yvar")
  built <- ggplot2::ggplot_build(p)$data[[1]]
  expect_equal(length(unique(built$group)), 4)
  expect_equal(unique(built$colour), "#F8766D")
})
