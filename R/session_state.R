# Saving and restoring app sessions

#' Get current MVP session state
#'
#' Captures all current UI inputs and reactive values into a serializable list
#' for saving via \code{saveRDS()}. Complementary to
#' \code{\link{restore_session_state}}.
#'
#' @param input The Shiny \code{input} object from the current server session.
#' @param uploaded_data The \code{uploaded_data} reactive. Wrapped in
#'   \code{tryCatch} so \code{NULL} is stored gracefully if no file is loaded.
#'
#' @return A named list containing \code{version}, \code{saved_at}, and
#'   sublists \code{model}, \code{data}, \code{dosing}, \code{sim},
#'   \code{psa}, and \code{variability}.
#'
#' @seealso \code{\link{restore_session_state}}
#'
#' @noRd
get_session_state <- function(input, uploaded_data) {
  list(
    version = mvp_version(),  # tag with app version for forward-compat checks
    saved_at = Sys.time(),
    
    # --- Model code & settings ---
    model = list(
      model_input     = input$model_input,
      model_input2    = input$model_input2,
      model_select    = input$model_select,
      model_select2   = input$model_select2
    ),
    
    # --- Data ---
    data = list(
      uploaded_data      = tryCatch(uploaded_data(), error = function(e) NULL),
      create_cmt_col     = input$create_cmt_col,
      create_id_col      = input$create_id_col,
      create_time_col    = input$create_time_col,
      BLQ_filter         = input$BLQ_filter,
      EVID_filter        = input$EVID_filter,
      distinct_by_ID     = input$distinct_by_ID,
      turn_all_numeric   = input$turn_all_numeric,
      column             = input$column,
      codes              = input$codes,
      enableAutocomplete = input$enableAutocomplete,
      ########################################
      transpose_data_info= input$transpose_data_info,
      subject_colname    = input$subject_colname,
      additional_keys    = input$additional_keys,
      time_colname       = input$time_colname,
      desc_time_unit     = input$desc_time_unit,
      conc_colname       = input$conc_colname,
      desc_conc_unit     = input$desc_conc_unit,
      dose_colname       = input$dose_colname,
      desc_dose_unit     = input$desc_dose_unit,
      adm_route          = input$adm_route,
      dur_inf            = input$dur_inf,
      down_method        = input$down_method,
      mw_value           = input$mw_value,
      nca_sigfigs        = input$nca_sigfigs,
      transpose_nca      = input$transpose_nca,
      ########################################
      y_axis             = input$y_axis,
      x_axis             = input$x_axis,
      color              = input$color,
      median_line_by     = input$median_line_by,
      facet_by           = input$facet_by,
      filter_cmt_data    = input$filter_cmt_data,
      log_y_axis_data    = input$log_y_axis_data,
      log_x_axis_data    = input$log_x_axis_data,
      insert_smoother    = input$insert_smoother,
      median_line_data   = input$median_line_data,
      insert_lm_eqn      = input$insert_lm_eqn,
      do_boxplot         = input$do_boxplot, # updatePrettySwitch
      plot_title_data    = input$plot_title_data,
      select_label_size  = input$select_label_size,
      quantize_x         = input$quantize_x,
      do_data_plotly     = input$do_data_plotly,
      ########################################
      filter_by_id       = input$filter_by_id,
      page_number        = input$page_number,
      number_of_rows     = input$number_of_rows,
      number_of_cols     = input$number_of_cols,
      highlight_var      = input$highlight_var,
      highlight_var_values=input$highlight_var_values,
      lloq_colname       = input$lloq_colname,
      ind_dose_colname   = input$ind_dose_colname,
      dose_units         = input$dose_units,
      insert_dosing      = input$insert_dosing,
      fixed_scale        = input$fixed_scale,
      do_data_ind_plotly = input$do_data_ind_plotly,
      plot_title_ind     = input$plot_title_ind,
      sort_by_ind        = input$sort_by_ind,
      strat_by_ind       = input$strat_by_ind,
      outlier_threshold  = input$outlier_threshold,
      ########################################
      var_corr           = input$var_corr,
      color_corr         = input$color_corr,
      ########################################
      var_hist           = input$var_hist,
      bin_size           = input$bin_size
    ),
    
    # --- Dosing ---
    dosing = list(
      cmt1_model_1       = input$cmt1_model_1,
      amt1               = input$amt1,
      delay_time1        = input$delay_time1,
      total1             = input$total1,
      ii1                = input$ii1,
      tinf1              = input$tinf1,
      ########################################
      cmt2_model_1       = input$cmt2_model_1,
      amt2               = input$amt2,
      delay_time2        = input$delay_time2,
      total2             = input$total2,
      ii2                = input$ii2,
      tinf2              = input$tinf2,
      ########################################
      cmt3_model_1       = input$cmt3_model_1,
      amt3               = input$amt3,
      delay_time3        = input$delay_time3,
      total3             = input$total3,
      ii3                = input$ii3,
      tinf3              = input$tinf3,   
      ########################################
      cmt4_model_1       = input$cmt4_model_1,
      amt4               = input$amt4,
      delay_time4        = input$delay_time4,
      total4             = input$total4,
      ii4                = input$ii4,
      tinf4              = input$tinf4,  
      ########################################
      cmt5_model_1       = input$cmt5_model_1,
      amt5               = input$amt5,
      delay_time5        = input$delay_time5,
      total5             = input$total5,
      ii5                = input$ii5,
      tinf5              = input$tinf5,
      ########################################
      mw_checkbox        = input$mw_checkbox,
      mw                 = input$mw,
      multi_factor       = input$multi_factor,
      wt_based_dosing_checkbox = input$wt_based_dosing_checkbox,
      wt_based_dosing_name     = input$wt_based_dosing_name,
      model_dur_checkbox       = input$model_dur_checkbox,
      model_rate_checkbox      = input$model_rate_checkbox,
      ########################################
      ########################################
      cmt1_model_2       = input$cmt1_model_2,
      amt1_2               = input$amt1_2,
      delay_time1_2        = input$delay_time1_2,
      total1_2             = input$total1_2,
      ii1_2                = input$ii1_2,
      tinf1_2              = input$tinf1_2,
      ########################################
      cmt2_model_2       = input$cmt2_model_2,
      amt2_2               = input$amt2_2,
      delay_time2_2        = input$delay_time2_2,
      total2_2             = input$total2_2,
      ii2_2                = input$ii2_2,
      tinf2_2              = input$tinf2_2,
      ########################################
      cmt3_model_2       = input$cmt3_model_2,
      amt3_2               = input$amt3_2,
      delay_time3_2        = input$delay_time3_2,
      total3_2             = input$total3_2,
      ii3_2                = input$ii3_2,
      tinf3_2              = input$tinf3_2,   
      ########################################
      cmt4_model_2       = input$cmt4_model_2,
      amt4_2               = input$amt4_2,
      delay_time4_2        = input$delay_time4_2,
      total4_2             = input$total4_2,
      ii4_2                = input$ii4_2,
      tinf4_2              = input$tinf4_2,  
      ########################################
      cmt5_model_2       = input$cmt5_model_2,
      amt5_2               = input$amt5_2,
      delay_time5_2        = input$delay_time5_2,
      total5_2             = input$total5_2,
      ii5_2                = input$ii5_2,
      tinf5_2              = input$tinf5_2,
      ########################################
      mw_checkbox_2        = input$mw_checkbox_2,
      mw_2                 = input$mw_2,
      multi_factor_2       = input$multi_factor_2,
      wt_based_dosing_checkbox_2 = input$wt_based_dosing_checkbox_2,
      wt_based_dosing_name_2     = input$wt_based_dosing_name_2,
      model_dur_checkbox_2       = input$model_dur_checkbox_2,
      model_rate_checkbox_2      = input$model_rate_checkbox_2
      # 
    ),
    
    # --- Simulation options ---
    sim = list(
      time_unit              = input$time_unit,
      x_axis_label           = input$x_axis_label,
      yaxis_name             = input$yaxis_name,
      yaxis_name_model_2     = input$yaxis_name_model_2,
      y_axis_label           = input$y_axis_label,
      plot_title_sim         = input$plot_title_sim,
      log_y_axis             = input$log_y_axis,
      log_x_axis             = input$log_x_axis,
      geom_point_sim_option  = input$geom_point_sim_option,
      show_model_1           = input$show_model_1,
      show_model_2           = input$show_model_2,
      do_sim_plotly          = input$do_sim_plotly,
      ##############################################
      tgrid_max              = input$tgrid_max,
      delta                  = input$delta,
      custom_sampling_time_text = input$custom_sampling_time_text,
      custom_sampling_time_cb   = input$custom_sampling_time_cb,
      add_time_zero             = input$add_time_zero,
      ##############################################
      nonmem_y_axis          = input$nonmem_y_axis,
      filter_cmt             = input$filter_cmt,
      color_data_by          = input$color_data_by,
      stat_sum_data_by       = input$stat_sum_data_by,
      combine_nmdata         = input$combine_nmdata,
      stat_sum_data_option   = input$stat_sum_data_option,
      geom_point_data_option = input$geom_point_data_option
      
    ),
    
    # --- PSA state ---
    psa = list(
      param_selector_model_1        = input$param_selector_model_1,
      digits_model_1                = input$digits_model_1,
      dp_checkbox_model_1           = input$dp_checkbox_model_1,
      min_nca_obs_time_model_1      = input$min_nca_obs_time_model_1,
      max_nca_obs_time_model_1      = input$max_nca_obs_time_model_1,      
      log_y_axis_model_1            = input$log_y_axis_model_1,
      geom_point_sim_option_model_1 = input$geom_point_sim_option_model_1,
      log_x_axis_model_1            = input$log_x_axis_model_1,      
      geom_point_data_option_model_1 = input$geom_point_data_option_model_1,
      geom_vline_option_model_1      = input$geom_vline_option_model_1,
      combine_nmdata_1_model_1       = input$combine_nmdata_1_model_1,
      geom_ribbon_option_model_1     = input$geom_ribbon_option_model_1,
      stat_sum_data_option_model_1   = input$stat_sum_data_option_model_1,
      plot_title_psa_model_1         = input$plot_title_psa_model_1,
      do_psa_plotly_model_1          = input$do_psa_plotly_model_1,
      ##############################################
      param_selector_model_2        = input$param_selector_model_2,
      digits_model_2                = input$digits_model_2,
      dp_checkbox_model_2           = input$dp_checkbox_model_2,
      min_nca_obs_time_model_2      = input$min_nca_obs_time_model_2,
      max_nca_obs_time_model_2      = input$max_nca_obs_time_model_2,   
      log_y_axis_model_2            = input$log_y_axis_model_2,
      geom_point_sim_option_model_2 = input$geom_point_sim_option_model_2,
      log_x_axis_model_2            = input$log_x_axis_model_2,      
      geom_point_data_option_model_2 = input$geom_point_data_option_model_2,
      geom_vline_option_model_2      = input$geom_vline_option_model_2,
      combine_nmdata_1_model_2       = input$combine_nmdata_1_model_2,
      geom_ribbon_option_model_2     = input$geom_ribbon_option_model_2,
      stat_sum_data_option_model_2   = input$stat_sum_data_option_model_2,
      plot_title_psa_model_2         = input$plot_title_psa_model_2,
      do_psa_plotly_model_2          = input$do_psa_plotly_model_2,
      ##############################################
      min_tor_obs_time_model_1       = input$min_tor_obs_time_model_1,
      max_tor_obs_time_model_1       = input$max_tor_obs_time_model_1, 
      tor_lower_model_1              = input$tor_lower_model_1,
      tor_upper_model_1              = input$tor_upper_model_1,
      tor_fix_model_1                = input$tor_fix_model_1,
      tor_show_digits_model_1        = input$tor_show_digits_model_1,
      tor_do_gradient_model_1        = input$tor_do_gradient_model_1,
      tor_var_model_1                = input$tor_var_model_1,
      select_tor_metric_model_1      = input$select_tor_metric_model_1,
      tor_display_as_model_1         = input$tor_display_as_model_1,
      trim_tor_model_1               = input$trim_tor_model_1,
      show_bioeq_model_1             = input$show_bioeq_model_1,
      plot_title_tor_model_1         = input$plot_title_tor_model_1,
      xlab_tor_model_1               = input$xlab_tor_model_1,
      tor_display_text_model_1       = input$tor_display_text_model_1,
      spi_normalize_model_1          = input$spi_normalize_model_1,
      do_tor_plotly_model_1          = input$do_tor_plotly_model_1,
      ##############################################
      min_tor_obs_time_model_2       = input$min_tor_obs_time_model_2,
      max_tor_obs_time_model_2       = input$max_tor_obs_time_model_2, 
      tor_lower_model_2              = input$tor_lower_model_2,
      tor_upper_model_2              = input$tor_upper_model_2,
      tor_fix_model_2                = input$tor_fix_model_2,
      tor_show_digits_model_2        = input$tor_show_digits_model_2,
      tor_do_gradient_model_2        = input$tor_do_gradient_model_2,
      tor_var_model_2                = input$tor_var_model_2,
      select_tor_metric_model_2      = input$select_tor_metric_model_2,
      tor_display_as_model_2         = input$tor_display_as_model_2,
      trim_tor_model_2               = input$trim_tor_model_2,
      show_bioeq_model_2             = input$show_bioeq_model_2,
      plot_title_tor_model_2         = input$plot_title_tor_model_2,
      xlab_tor_model_2               = input$xlab_tor_model_2,
      tor_display_text_model_2       = input$tor_display_text_model_2,
      spi_normalize_model_2          = input$spi_normalize_model_2,
      do_tor_plotly_model_2          = input$do_tor_plotly_model_2      
      
    ),
    
    # --- Variability matrices ---
    variability = list(
      db_model_1             = input$db_model_1,
      n_subj_model_1         = input$n_subj_model_1,
      seed_number_model_1    = input$seed_number_model_1,
      age_db_model_1         = input$age_db_model_1,
      wt_db_model_1          = input$wt_db_model_1,
      males_db_model_1       = input$males_db_model_1,
      bmi_db_model_1         = input$bmi_db_model_1,
      custom_cov_1_model_1   = input$custom_cov_1_model_1,
      custom_cov_1_dist_model_1 = input$custom_cov_1_dist_model_1,
      custom_cov_2_model_1   = input$custom_cov_2_model_1,
      custom_cov_2_dist_model_1 = input$custom_cov_2_dist_model_1,
      custom_cov_3_model_1   = input$custom_cov_3_model_1,
      custom_cov_3_dist_model_1 = input$custom_cov_3_dist_model_1,      
      ##############################################
      db_model_2             = input$db_model_2,
      n_subj_model_2         = input$n_subj_model_2,
      seed_number_model_2    = input$seed_number_model_2,
      age_db_model_2         = input$age_db_model_2,
      wt_db_model_2          = input$wt_db_model_2,
      males_db_model_2       = input$males_db_model_2,
      bmi_db_model_2         = input$bmi_db_model_2,
      custom_cov_1_model_2   = input$custom_cov_1_model_2,
      custom_cov_1_dist_model_2 = input$custom_cov_1_dist_model_2,
      custom_cov_2_model_2   = input$custom_cov_2_model_2,
      custom_cov_2_dist_model_2 = input$custom_cov_2_dist_model_2,
      custom_cov_3_model_2   = input$custom_cov_3_model_2,
      custom_cov_3_dist_model_2 = input$custom_cov_3_dist_model_2,
      ##############################################
      upper_quartile         = input$upper_quartile,
      lower_quartile         = input$lower_quartile,
      show_ind_profiles      = input$show_ind_profiles,
      do_iiv_plotly          = input$do_iiv_plotly,
      show_iiv_model_1       = input$show_iiv_model_1,
      log_y_axis_iiv         = input$log_y_axis_iiv,
      show_iiv_model_2       = input$show_iiv_model_2,
      log_x_axis_iiv         = input$log_x_axis_iiv,
      combine_nmdata_iiv     = input$combine_nmdata_iiv,
      stat_sum_data_option_iiv = input$stat_sum_data_option_iiv,
      geom_point_data_option_iiv = input$geom_point_data_option_iiv,
      plot_title_iiv         = input$plot_title_iiv,
      y_value_threshold      = input$y_value_threshold,
      x_value_threshold      = input$x_value_threshold,
      show_y_intercept_threshold = input$show_y_intercept_threshold,
      show_x_intercept_threshold = input$show_x_intercept_threshold,
      ##############################################
      select_exp             = input$select_exp,
      min_exp_obs_time_model = input$min_exp_obs_time_model,
      max_exp_obs_time_model = input$max_exp_obs_time_model,      
      exp_yaxis_label        = input$exp_yaxis_label,
      exp_model_1_name       = input$exp_model_1_name,
      exp_model_2_name       = input$exp_model_2_name,
      exp_show_model_1       = input$exp_show_model_1,
      exp_show_model_2       = input$exp_show_model_2,
      plot_title_exp_model   = input$plot_title_exp_model,
      exp_display_stats      = input$exp_display_stats,
      do_exp_plotly          = input$do_exp_plotly
      
    )
  )
}

#' Restore a saved MVP session state
#'
#' Restores a previously saved session state by updating all UI inputs and
#' reactive values to match the saved configuration. Intended to be called
#' inside an \code{observeEvent()} triggered by a session file upload, and
#' again after model compilation completes for inputs that require a compiled
#' model to exist first.
#'
#' @param state A named list produced by \code{\link{get_session_state}},
#'   typically loaded via \code{readRDS()}. Must contain a \code{version} field
#'   for compatibility checking.
#' @param input The Shiny \code{input} object from the current server session.
#' @param session The Shiny \code{session} object passed to all
#'   \code{updateXxx()} calls.
#' @param uploaded_data_override A \code{\link[shiny]{reactiveVal}} used to
#'   inject the restored dataset into \code{uploaded_data()} without requiring
#'   a file upload.
#' @param show_note Logical (no default). Whether to display a version
#'   compatibility notification on restore.
#' @param selectize_choices Workaround for restoring nonmem_y_axis and color_data_by   
#'
#' @return \code{NULL} invisibly. Called for its side effects.
#'
#' @details
#' Restoration covers: dataset cleaning options, NCA settings, plot options,
#' dosing regimens for both models, simulation settings, PSA options, and
#' variability settings. Inputs are restored via the appropriate
#' \code{updateXxx()} function for each widget type.
#'
#' A version compatibility notification is shown if \code{show_note = TRUE}
#' and a \code{version} field is present in \code{state}, but restoration
#' proceeds regardless of version match.
#' 
#' @importFrom shinyAce updateAceEditor
#' @importFrom shinyWidgets updatePrettySwitch updatePickerInput
#'
#' @seealso \code{\link{get_session_state}} for the complementary save function.
#'
#' @noRd
restore_session_state <- function(state, input, session, uploaded_data_override, show_note,
                                  selectize_choices = NULL) {
  
  # --- Version compatibility check ---
  if(show_note) {
    if (!is.null(state$version) && !identical(state$version, mvp_version())) {
      showNotification(
        paste0("Session was saved with v", state$version, ". ",
               "Some settings may not restore correctly if you are on a different version."),
        type = "message", duration = 10
      )
    }
  }
  
  uploaded_data_override(state$data$uploaded_data)
  shinyAce::updateAceEditor(session, "codes",          value    = state$data$codes)
  
  # --- Model code & settings ---
  updateSelectInput(session, "model_select",         selected = '--------------------------------------------') # Dummy choice to not update model code
  updateSelectInput(session, "model_select2",        selected = '--------------------------------------------') # Dummy choice to not update model code
  shinyAce::updateAceEditor(session, "model_input",  value    = state$model$model_input)
  shinyAce::updateAceEditor(session, "model_input2", value    = state$model$model_input2)
  
  # --- Data: Dataset cleaning ---
  updateCheckboxInput(session, "create_cmt_col",      value    = state$data$create_cmt_col)
  updateCheckboxInput(session, "create_id_col",        value    = state$data$create_id_col)
  updateCheckboxInput(session, "create_time_col",      value    = state$data$create_time_col)
  updateCheckboxInput(session, "BLQ_filter",           value    = state$data$BLQ_filter)
  updateCheckboxInput(session, "EVID_filter",          value    = state$data$EVID_filter)
  updateCheckboxInput(session, "distinct_by_ID",       value    = state$data$distinct_by_ID)
  updateCheckboxInput(session, "turn_all_numeric",     value    = state$data$turn_all_numeric)
  updateSelectizeInput(session, "column",              selected = state$data$column)
  updateCheckboxInput(session, "enableAutocomplete",   value    = state$data$enableAutocomplete)
  
  # --- Data: Summary statistics ---
  updateCheckboxInput(session, "transpose_data_info",  value    = state$data$transpose_data_info)
  
  # --- Data: NCA options ---
  updateSelectizeInput(session, "subject_colname",     selected = state$data$subject_colname)
  updateSelectizeInput(session, "additional_keys",     selected = state$data$additional_keys)
  updateSelectizeInput(session, "time_colname",        selected = state$data$time_colname)
  updateTextInput(    session, "desc_time_unit",        value    = state$data$desc_time_unit)
  updateSelectizeInput(session, "conc_colname",        selected = state$data$conc_colname)
  updateTextInput(    session, "desc_conc_unit",        value    = state$data$desc_conc_unit)
  updateSelectizeInput(session, "dose_colname",        selected = state$data$dose_colname)
  updateTextInput(    session, "desc_dose_unit",        value    = state$data$desc_dose_unit)
  updateSelectizeInput(session, "adm_route",           selected = state$data$adm_route)
  updateNumericInput( session, "dur_inf",               value    = state$data$dur_inf)
  updateSelectizeInput(session, "down_method",         selected = state$data$down_method)
  updateNumericInput( session, "mw_value",              value    = state$data$mw_value)
  updateSelectizeInput(session, "nca_sigfigs",         selected = state$data$nca_sigfigs)
  updateCheckboxInput(session, "transpose_nca",        value    = state$data$transpose_nca)
  
  # --- Data: General plot options ---
  updateSelectizeInput(session, "y_axis",              selected = state$data$y_axis)
  updateSelectizeInput(session, "x_axis",              selected = state$data$x_axis)
  updateSelectizeInput(session, "color",               selected = state$data$color)
  updateSelectizeInput(session, "median_line_by",      selected = state$data$median_line_by)
  updateSelectizeInput(session, "facet_by",            selected = state$data$facet_by)
  updateSelectizeInput(session, "filter_cmt_data",     selected = state$data$filter_cmt_data)
  updateCheckboxInput(session, "log_y_axis_data",      value    = state$data$log_y_axis_data)
  updateCheckboxInput(session, "log_x_axis_data",      value    = state$data$log_x_axis_data)
  updateCheckboxInput(session, "insert_smoother",      value    = state$data$insert_smoother)
  updateCheckboxInput(session, "median_line_data",     value    = state$data$median_line_data)
  updateCheckboxInput(session, "insert_lm_eqn",        value    = state$data$insert_lm_eqn)
  shinyWidgets::updatePrettySwitch(session, "do_boxplot", value = state$data$do_boxplot)
  updateTextInput(    session, "plot_title_data",       value    = state$data$plot_title_data)
  updateSelectInput(  session, "select_label_size",    selected = state$data$select_label_size)
  updateSelectInput(  session, "quantize_x",           selected = state$data$quantize_x)
  updateCheckboxInput(session, "do_data_plotly",       value    = state$data$do_data_plotly)
  
  # --- Data: Individual plot options ---
  updateSelectizeInput(session, "filter_by_id",        selected = state$data$filter_by_id)
  updateSelectInput(  session, "page_number",          selected = state$data$page_number)
  updateSelectInput(  session, "number_of_rows",       selected = state$data$number_of_rows)
  updateSelectInput(  session, "number_of_cols",       selected = state$data$number_of_cols)
  updateSelectizeInput(session, "highlight_var",       selected = state$data$highlight_var)
  updateSelectizeInput(session, "highlight_var_values",selected = state$data$highlight_var_values)
  updateSelectizeInput(session, "lloq_colname",        selected = state$data$lloq_colname)
  updateSelectizeInput(session, "ind_dose_colname",    selected = state$data$ind_dose_colname)
  updateTextInput(    session, "dose_units",            value    = state$data$dose_units)
  updateCheckboxInput(session, "insert_dosing",        value    = state$data$insert_dosing)
  updateCheckboxInput(session, "fixed_scale",          value    = state$data$fixed_scale)
  updateCheckboxInput(session, "do_data_ind_plotly",   value    = state$data$do_data_ind_plotly)
  updateTextInput(    session, "plot_title_ind",        value    = state$data$plot_title_ind)
  updateSelectizeInput(session, "sort_by_ind",         selected = state$data$sort_by_ind)
  updateSelectizeInput(session, "strat_by_ind",        selected = state$data$strat_by_ind)
  updateSelectizeInput(session, "outlier_threshold",   selected = state$data$outlier_threshold)
  
  # --- Data: Correlation plot options ---
  updateSelectizeInput(session, "var_corr",            selected = state$data$var_corr)
  updateSelectizeInput(session, "color_corr",          selected = state$data$color_corr)
  
  # --- Data: Histogram options ---
  updateSelectizeInput(session, "var_hist",            selected = state$data$var_hist)
  updateNumericInput( session, "bin_size",              value    = state$data$bin_size)
  
  # --- Dosing: Model 1 ---
  updateSelectInput( session, "cmt1_model_1",   selected = state$dosing$cmt1_model_1)
  updateNumericInput(session, "amt1",            value    = state$dosing$amt1)
  updateNumericInput(session, "delay_time1",     value    = state$dosing$delay_time1)
  updateNumericInput(session, "total1",          value    = state$dosing$total1)
  updateNumericInput(session, "ii1",             value    = state$dosing$ii1)
  updateNumericInput(session, "tinf1",           value    = state$dosing$tinf1)
  
  updateSelectInput( session, "cmt2_model_1",   selected = state$dosing$cmt2_model_1)
  updateNumericInput(session, "amt2",            value    = state$dosing$amt2)
  updateNumericInput(session, "delay_time2",     value    = state$dosing$delay_time2)
  updateNumericInput(session, "total2",          value    = state$dosing$total2)
  updateNumericInput(session, "ii2",             value    = state$dosing$ii2)
  updateNumericInput(session, "tinf2",           value    = state$dosing$tinf2)
  
  updateSelectInput( session, "cmt3_model_1",   selected = state$dosing$cmt3_model_1)
  updateNumericInput(session, "amt3",            value    = state$dosing$amt3)
  updateNumericInput(session, "delay_time3",     value    = state$dosing$delay_time3)
  updateNumericInput(session, "total3",          value    = state$dosing$total3)
  updateNumericInput(session, "ii3",             value    = state$dosing$ii3)
  updateNumericInput(session, "tinf3",           value    = state$dosing$tinf3)
  
  updateSelectInput( session, "cmt4_model_1",   selected = state$dosing$cmt4_model_1)
  updateNumericInput(session, "amt4",            value    = state$dosing$amt4)
  updateNumericInput(session, "delay_time4",     value    = state$dosing$delay_time4)
  updateNumericInput(session, "total4",          value    = state$dosing$total4)
  updateNumericInput(session, "ii4",             value    = state$dosing$ii4)
  updateNumericInput(session, "tinf4",           value    = state$dosing$tinf4)
  
  updateSelectInput( session, "cmt5_model_1",   selected = state$dosing$cmt5_model_1)
  updateNumericInput(session, "amt5",            value    = state$dosing$amt5)
  updateNumericInput(session, "delay_time5",     value    = state$dosing$delay_time5)
  updateNumericInput(session, "total5",          value    = state$dosing$total5)
  updateNumericInput(session, "ii5",             value    = state$dosing$ii5)
  updateNumericInput(session, "tinf5",           value    = state$dosing$tinf5)
  
  updateCheckboxInput(session, "mw_checkbox",                value    = state$dosing$mw_checkbox)
  updateNumericInput( session, "mw",                         value    = state$dosing$mw)
  updateNumericInput( session, "multi_factor",               value    = state$dosing$multi_factor)
  updateCheckboxInput(session, "wt_based_dosing_checkbox",   value    = state$dosing$wt_based_dosing_checkbox)
  updateTextInput(    session, "wt_based_dosing_name",       value    = state$dosing$wt_based_dosing_name)
  updateCheckboxInput(session, "model_dur_checkbox",         value    = state$dosing$model_dur_checkbox)
  updateCheckboxInput(session, "model_rate_checkbox",        value    = state$dosing$model_rate_checkbox)
  
  # --- Dosing: Model 2 ---
  updateSelectInput( session, "cmt1_model_2",   selected = state$dosing$cmt1_model_2)
  updateNumericInput(session, "amt1_2",          value    = state$dosing$amt1_2)
  updateNumericInput(session, "delay_time1_2",   value    = state$dosing$delay_time1_2)
  updateNumericInput(session, "total1_2",        value    = state$dosing$total1_2)
  updateNumericInput(session, "ii1_2",           value    = state$dosing$ii1_2)
  updateNumericInput(session, "tinf1_2",         value    = state$dosing$tinf1_2)
  
  updateSelectInput( session, "cmt2_model_2",   selected = state$dosing$cmt2_model_2)
  updateNumericInput(session, "amt2_2",          value    = state$dosing$amt2_2)
  updateNumericInput(session, "delay_time2_2",   value    = state$dosing$delay_time2_2)
  updateNumericInput(session, "total2_2",        value    = state$dosing$total2_2)
  updateNumericInput(session, "ii2_2",           value    = state$dosing$ii2_2)
  updateNumericInput(session, "tinf2_2",         value    = state$dosing$tinf2_2)
  
  updateSelectInput( session, "cmt3_model_2",   selected = state$dosing$cmt3_model_2)
  updateNumericInput(session, "amt3_2",          value    = state$dosing$amt3_2)
  updateNumericInput(session, "delay_time3_2",   value    = state$dosing$delay_time3_2)
  updateNumericInput(session, "total3_2",        value    = state$dosing$total3_2)
  updateNumericInput(session, "ii3_2",           value    = state$dosing$ii3_2)
  updateNumericInput(session, "tinf3_2",         value    = state$dosing$tinf3_2)
  
  updateSelectInput( session, "cmt4_model_2",   selected = state$dosing$cmt4_model_2)
  updateNumericInput(session, "amt4_2",          value    = state$dosing$amt4_2)
  updateNumericInput(session, "delay_time4_2",   value    = state$dosing$delay_time4_2)
  updateNumericInput(session, "total4_2",        value    = state$dosing$total4_2)
  updateNumericInput(session, "ii4_2",           value    = state$dosing$ii4_2)
  updateNumericInput(session, "tinf4_2",         value    = state$dosing$tinf4_2)
  
  updateSelectInput( session, "cmt5_model_2",   selected = state$dosing$cmt5_model_2)
  updateNumericInput(session, "amt5_2",          value    = state$dosing$amt5_2)
  updateNumericInput(session, "delay_time5_2",   value    = state$dosing$delay_time5_2)
  updateNumericInput(session, "total5_2",        value    = state$dosing$total5_2)
  updateNumericInput(session, "ii5_2",           value    = state$dosing$ii5_2)
  updateNumericInput(session, "tinf5_2",         value    = state$dosing$tinf5_2)
  
  updateCheckboxInput(session, "mw_checkbox_2",                value    = state$dosing$mw_checkbox_2)
  updateNumericInput( session, "mw_2",                         value    = state$dosing$mw_2)
  updateNumericInput( session, "multi_factor_2",               value    = state$dosing$multi_factor_2)
  updateCheckboxInput(session, "wt_based_dosing_checkbox_2",   value    = state$dosing$wt_based_dosing_checkbox_2)
  updateTextInput(    session, "wt_based_dosing_name_2",       value    = state$dosing$wt_based_dosing_name_2)
  updateCheckboxInput(session, "model_dur_checkbox_2",         value    = state$dosing$model_dur_checkbox_2)
  updateCheckboxInput(session, "model_rate_checkbox_2",        value    = state$dosing$model_rate_checkbox_2)
  
  # --- Simulation options ---
  updateSelectizeInput( session, "time_unit",                 selected= state$sim$time_unit)
  updateTextInput(   session, "x_axis_label",              value    = state$sim$x_axis_label)
  updateSelectInput( session, "yaxis_name",                selected = state$sim$yaxis_name)
  updateSelectInput( session, "yaxis_name_model_2",        selected = state$sim$yaxis_name_model_2)
  updateTextInput(   session, "y_axis_label",              value    = state$sim$y_axis_label)
  updateTextInput(   session, "plot_title_sim",            value    = state$sim$plot_title_sim)
  updateCheckboxInput(session, "log_y_axis",               value    = state$sim$log_y_axis)
  updateCheckboxInput(session, "log_x_axis",               value    = state$sim$log_x_axis)
  updateCheckboxInput(session, "geom_point_sim_option",    value    = state$sim$geom_point_sim_option)
  updateCheckboxInput(session, "show_model_1",             value    = state$sim$show_model_1)
  updateCheckboxInput(session, "show_model_2",             value    = state$sim$show_model_2)
  updateCheckboxInput(session, "do_sim_plotly",            value    = state$sim$do_sim_plotly)
  updateNumericInput( session, "tgrid_max",                value    = state$sim$tgrid_max)
  updateNumericInput( session, "delta",                    value    = state$sim$delta)
  updateTextInput(   session, "custom_sampling_time_text", value    = state$sim$custom_sampling_time_text)
  updateCheckboxInput(session, "custom_sampling_time_cb",  value    = state$sim$custom_sampling_time_cb)
  updateCheckboxInput(session, "add_time_zero",            value    = state$sim$add_time_zero)
  
  # --- Simulation options (Dataset) ---
  updateSelectizeInput(session, "filter_cmt",              selected = state$sim$filter_cmt)
  
  if (!is.null(selectize_choices)) {
    updateSelectizeInput(session, "nonmem_y_axis", choices = selectize_choices, selected = state$sim$nonmem_y_axis)
    updateSelectizeInput(session, "color_data_by", choices = selectize_choices, selected = state$sim$color_data_by)
  } else {
    updateSelectizeInput(session, "nonmem_y_axis", selected = state$sim$nonmem_y_axis)
    updateSelectizeInput(session, "color_data_by", selected = state$sim$color_data_by)
  }
  
  updateSelectizeInput(session, "stat_sum_data_by",         selected = state$sim$stat_sum_data_by)
  updateCheckboxInput(session, "combine_nmdata",           value    = state$sim$combine_nmdata)
  updateCheckboxInput(session, "stat_sum_data_option",     value    = state$sim$stat_sum_data_option)
  updateCheckboxInput(session, "geom_point_data_option",   value    = state$sim$geom_point_data_option)
  
  # --- PSA ---
  updateSelectInput(  session, "param_selector_model_1",       selected = state$psa$param_selector_model_1)
  updateNumericInput( session, "digits_model_1",                  value = state$psa$digits_model_1)
  updateCheckboxInput(session, "dp_checkbox_model_1",            value    = state$psa$dp_checkbox_model_1)
  shinyWidgets::updatePickerInput( session,  "min_nca_obs_time_model_1",       selected = state$psa$min_nca_obs_time_model_1)
  shinyWidgets::updatePickerInput( session,  "max_nca_obs_time_model_1",       selected = state$psa$max_nca_obs_time_model_1)
  updateCheckboxInput(session, "log_y_axis_model_1",             value    = state$psa$log_y_axis_model_1)
  updateCheckboxInput(session, "geom_point_sim_option_model_1",  value    = state$psa$geom_point_sim_option_model_1)
  updateCheckboxInput(session, "log_x_axis_model_1",             value    = state$psa$log_x_axis_model_1)
  updateCheckboxInput(session, "geom_point_data_option_model_1", value    = state$psa$geom_point_data_option_model_1)
  updateCheckboxInput(session, "geom_vline_option_model_1",      value    = state$psa$geom_vline_option_model_1)
  updateCheckboxInput(session, "combine_nmdata_1_model_1",       value    = state$psa$combine_nmdata_1_model_1)
  updateCheckboxInput(session, "geom_ribbon_option_model_1",     value    = state$psa$geom_ribbon_option_model_1)
  updateCheckboxInput(session, "stat_sum_data_option_model_1",   value    = state$psa$stat_sum_data_option_model_1)
  updateTextInput(    session, "plot_title_psa_model_1",         value    = state$psa$plot_title_psa_model_1)
  updateCheckboxInput(session, "do_psa_plotly_model_1",          value    = state$psa$do_psa_plotly_model_1)
  
  updateSelectInput(  session, "param_selector_model_2",       selected = state$psa$param_selector_model_2)
  updateNumericInput( session, "digits_model_2",                  value = state$psa$digits_model_2)
  updateCheckboxInput(session, "dp_checkbox_model_2",            value    = state$psa$dp_checkbox_model_2)
  shinyWidgets::updatePickerInput( session,  "min_nca_obs_time_model_2",       selected = state$psa$min_nca_obs_time_model_2)
  shinyWidgets::updatePickerInput( session,  "max_nca_obs_time_model_2",       selected = state$psa$max_nca_obs_time_model_2)
  updateCheckboxInput(session, "log_y_axis_model_2",             value    = state$psa$log_y_axis_model_2)
  updateCheckboxInput(session, "geom_point_sim_option_model_2",  value    = state$psa$geom_point_sim_option_model_2)
  updateCheckboxInput(session, "log_x_axis_model_2",             value    = state$psa$log_x_axis_model_2)
  updateCheckboxInput(session, "geom_point_data_option_model_2", value    = state$psa$geom_point_data_option_model_2)
  updateCheckboxInput(session, "geom_vline_option_model_2",      value    = state$psa$geom_vline_option_model_2)
  updateCheckboxInput(session, "combine_nmdata_1_model_2",       value    = state$psa$combine_nmdata_1_model_2)
  updateCheckboxInput(session, "geom_ribbon_option_model_2",     value    = state$psa$geom_ribbon_option_model_2)
  updateCheckboxInput(session, "stat_sum_data_option_model_2",   value    = state$psa$stat_sum_data_option_model_2)
  updateTextInput(    session, "plot_title_psa_model_2",         value    = state$psa$plot_title_psa_model_2)
  updateCheckboxInput(session, "do_psa_plotly_model_2",          value    = state$psa$do_psa_plotly_model_2)
  
  # --- Tornado - Model 1 ---
  shinyWidgets::updatePickerInput( session, "min_tor_obs_time_model_1", selected = state$psa$min_tor_obs_time_model_1)
  shinyWidgets::updatePickerInput( session, "max_tor_obs_time_model_1", selected = state$psa$max_tor_obs_time_model_1)
  updateNumericInput(  session, "tor_lower_model_1",          value    = state$psa$tor_lower_model_1)
  updateNumericInput(  session, "tor_upper_model_1",          value    = state$psa$tor_upper_model_1)
  updateSelectizeInput( session, "tor_fix_model_1",            selected = state$psa$tor_fix_model_1)
  updateCheckboxInput( session, "tor_show_digits_model_1",    value    = state$psa$tor_show_digits_model_1)
  updateCheckboxInput( session, "tor_do_gradient_model_1",    value    = state$psa$tor_do_gradient_model_1)
  updateSelectizeInput(session, "tor_var_model_1",            selected = state$psa$tor_var_model_1)
  updateSelectInput(   session, "select_tor_metric_model_1",  selected = state$psa$select_tor_metric_model_1)
  updateSelectInput(   session, "tor_display_as_model_1",     selected = state$psa$tor_display_as_model_1)
  updateNumericInput( session, "trim_tor_model_1",           value    = state$psa$trim_tor_model_1)
  updateCheckboxInput( session, "show_bioeq_model_1",         value    = state$psa$show_bioeq_model_1)
  updateTextInput(     session, "plot_title_tor_model_1",     value    = state$psa$plot_title_tor_model_1)
  updateTextInput(     session, "xlab_tor_model_1",           value    = state$psa$xlab_tor_model_1)
  updateCheckboxInput( session, "tor_display_text_model_1",   value    = state$psa$tor_display_text_model_1)
  updateCheckboxInput( session, "spi_normalize_model_1",      value    = state$psa$spi_normalize_model_1)
  updateCheckboxInput( session, "do_tor_plotly_model_1",      value    = state$psa$do_tor_plotly_model_1)
  
  # --- Tornado - Model 2 ---
  shinyWidgets::updatePickerInput( session, "min_tor_obs_time_model_2", selected = state$psa$min_tor_obs_time_model_2)
  shinyWidgets::updatePickerInput( session, "max_tor_obs_time_model_2", selected = state$psa$max_tor_obs_time_model_2)
  updateNumericInput(  session, "tor_lower_model_2",          value    = state$psa$tor_lower_model_2)
  updateNumericInput(  session, "tor_upper_model_2",          value    = state$psa$tor_upper_model_2)
  updateSelectizeInput( session, "tor_fix_model_2",            selected = state$psa$tor_fix_model_2)
  updateCheckboxInput( session, "tor_show_digits_model_2",    value    = state$psa$tor_show_digits_model_2)
  updateCheckboxInput( session, "tor_do_gradient_model_2",    value    = state$psa$tor_do_gradient_model_2)
  updateSelectizeInput(session, "tor_var_model_2",            selected = state$psa$tor_var_model_2)
  updateSelectInput(   session, "select_tor_metric_model_2",  selected = state$psa$select_tor_metric_model_2)
  updateSelectInput(   session, "tor_display_as_model_2",     selected = state$psa$tor_display_as_model_2)
  updateNumericInput( session, "trim_tor_model_2",           value    = state$psa$trim_tor_model_2)
  updateCheckboxInput( session, "show_bioeq_model_2",         value    = state$psa$show_bioeq_model_2)
  updateTextInput(     session, "plot_title_tor_model_2",     value    = state$psa$plot_title_tor_model_2)
  updateTextInput(     session, "xlab_tor_model_2",           value    = state$psa$xlab_tor_model_2)
  updateCheckboxInput( session, "tor_display_text_model_2",   value    = state$psa$tor_display_text_model_2)
  updateCheckboxInput( session, "spi_normalize_model_2",      value    = state$psa$spi_normalize_model_2)
  updateCheckboxInput( session, "do_tor_plotly_model_2",      value    = state$psa$do_tor_plotly_model_2)
  
  # --- Variability ---
  updateSelectizeInput( session, "db_model_1",          selected= state$variability$db_model_1)
  updateNumericInput(session, "n_subj_model_1",       value    = state$variability$n_subj_model_1)
  updateNumericInput(session, "seed_number_model_1",  value    = state$variability$seed_number_model_1)
  shiny::updateSliderInput( session, "age_db_model_1",       value    = state$variability$age_db_model_1)
  shiny::updateSliderInput( session, "wt_db_model_1",        value    = state$variability$wt_db_model_1)
  shiny::updateSliderInput( session, "males_db_model_1",     value    = state$variability$males_db_model_1)
  shiny::updateSliderInput( session, "bmi_db_model_1",       value    = state$variability$bmi_db_model_1)
  updateTextInput(   session, "custom_cov_1_model_1", value    = state$variability$custom_cov_1_model_1)
  updateTextInput(   session, "custom_cov_2_model_1", value    = state$variability$custom_cov_2_model_1)
  updateTextInput(   session, "custom_cov_3_model_1", value    = state$variability$custom_cov_3_model_1)
  updateSelectInput( session, "custom_cov_1_dist_model_1", selected = state$variability$custom_cov_1_dist_model_1)
  updateSelectInput( session, "custom_cov_2_dist_model_1", selected = state$variability$custom_cov_2_dist_model_1)
  updateSelectInput( session, "custom_cov_3_dist_model_1", selected = state$variability$custom_cov_3_dist_model_1)
  
  # --- Variability: Model 2 ---
  updateSelectizeInput( session, "db_model_2",          selected= state$variability$db_model_2)
  updateNumericInput(session, "n_subj_model_2",       value    = state$variability$n_subj_model_2)
  updateNumericInput(session, "seed_number_model_2",  value    = state$variability$seed_number_model_2)
  shiny::updateSliderInput( session, "age_db_model_2",       value    = state$variability$age_db_model_2)
  shiny::updateSliderInput( session, "wt_db_model_2",        value    = state$variability$wt_db_model_2)
  shiny::updateSliderInput( session, "males_db_model_2",     value    = state$variability$males_db_model_2)
  shiny::updateSliderInput( session, "bmi_db_model_2",       value    = state$variability$bmi_db_model_2)
  updateTextInput(   session, "custom_cov_1_model_2", value    = state$variability$custom_cov_1_model_2)
  updateTextInput(   session, "custom_cov_2_model_2", value    = state$variability$custom_cov_2_model_2)
  updateTextInput(   session, "custom_cov_3_model_2", value    = state$variability$custom_cov_3_model_2)
  updateSelectInput( session, "custom_cov_1_dist_model_2", selected = state$variability$custom_cov_1_dist_model_2)
  updateSelectInput( session, "custom_cov_2_dist_model_2", selected = state$variability$custom_cov_2_dist_model_2)
  updateSelectInput( session, "custom_cov_3_dist_model_2", selected = state$variability$custom_cov_3_dist_model_2)
  
  # --- Variability: IIV plot options ---
  updateNumericInput( session, "upper_quartile",              value    = state$variability$upper_quartile)
  updateNumericInput( session, "lower_quartile",              value    = state$variability$lower_quartile)
  updateCheckboxInput(session, "show_ind_profiles",           value    = state$variability$show_ind_profiles)
  updateCheckboxInput(session, "do_iiv_plotly",               value    = state$variability$do_iiv_plotly)
  updateCheckboxInput(session, "show_iiv_model_1",            value    = state$variability$show_iiv_model_1)
  updateCheckboxInput(session, "log_y_axis_iiv",              value    = state$variability$log_y_axis_iiv)
  updateCheckboxInput(session, "show_iiv_model_2",            value    = state$variability$show_iiv_model_2)
  updateCheckboxInput(session, "log_x_axis_iiv",              value    = state$variability$log_x_axis_iiv)
  updateCheckboxInput(session, "combine_nmdata_iiv",          value    = state$variability$combine_nmdata_iiv)
  updateCheckboxInput(  session, "stat_sum_data_option_iiv",    value = state$variability$stat_sum_data_option_iiv)
  updateCheckboxInput(  session, "geom_point_data_option_iiv",  value = state$variability$geom_point_data_option_iiv)
  updateTextInput(    session, "plot_title_iiv",              value    = state$variability$plot_title_iiv)
  updateNumericInput( session, "y_value_threshold",           value    = state$variability$y_value_threshold)
  shinyWidgets::updatePickerInput( session,  "x_value_threshold",           selected = state$variability$x_value_threshold)
  updateCheckboxInput(session, "show_y_intercept_threshold",  value    = state$variability$show_y_intercept_threshold)
  updateCheckboxInput(session, "show_x_intercept_threshold",  value    = state$variability$show_x_intercept_threshold)
  
  # --- Variability: Exposure box plot options ---
  shinyWidgets::updatePickerInput( session,  "min_exp_obs_time_model",      selected = state$variability$min_exp_obs_time_model)
  shinyWidgets::updatePickerInput( session,  "max_exp_obs_time_model",      selected = state$variability$max_exp_obs_time_model)
  updateSelectInput(  session, "select_exp",                  selected = state$variability$select_exp)
  updateTextInput(    session, "exp_yaxis_label",             value    = state$variability$exp_yaxis_label)
  updateTextInput(    session, "exp_model_1_name",            value    = state$variability$exp_model_1_name)
  updateTextInput(    session, "exp_model_2_name",            value    = state$variability$exp_model_2_name)
  updateCheckboxInput(session, "exp_show_model_1",            value    = state$variability$exp_show_model_1)
  updateCheckboxInput(session, "exp_show_model_2",            value    = state$variability$exp_show_model_2)
  updateTextInput(    session, "plot_title_exp_model",        value    = state$variability$plot_title_exp_model)
  updateCheckboxInput(session, "exp_display_stats",           value    = state$variability$exp_display_stats)
  updateCheckboxInput(session, "do_exp_plotly",               value    = state$variability$do_exp_plotly)
  
  invisible(NULL)
}
