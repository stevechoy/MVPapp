# Page Title 
default_page_title    <- 'Model Visualization Platform (MVP)'
page_title            <- default_page_title # replaced when mvp_app(page_title = ) is given

## UI settings
sidebar_width         <- 5  
mainbar_width         <- 12 - sidebar_width
debounce_timer_fast   <- 500
debounce_timer_slow   <- 900
max_samples           <- 20000 # Max samples = max sampling time * frequency of samples

# Colours
mvp_navy               <- "#003366" # Navbar / loading-spinner colour
slider_color          <- "#FFB600" # yellow
model_1_color         <- "#F8766D" # Shade of red
model_2_color         <- "#7570B3" # Shade of blue   

#### Page 1 Data Input
bspop_dataset_cleaning<- 'The uploaded dataset is automatically "cleaned" by the options included here.<br><br>Note: some options are always enabled to ensure compatibility in plotting.'
bspop_deselect        <- '(Optional) choose any number of columns to remove from the dataset prior to passing onto the editor below to help simplify the dataset.'
bspop_apply           <- 'After pressing Apply, check the Console below to see if there are any errors.'
bspop_select_columns  <- 'ID, TIME, and DV are required columns for the Dataset to be fully supported in plotting. <br><br> If missing, the user can create their own in the code editor.'
bspop_dataset_code    <- '(Optional) Adjust the uploaded dataset to facilitate quick data exploration or to ensure full compatibility with the App (requires the ID, TIME, and DV columns).<br><br>At minimum, the ID column needs to be present for Data Exploration.'

transpose_checkbox    <- 'Transpose Table'
bspop_transpose       <- 'Check to transpose summary table such that each original column becomes a row instead.'

bspop_nca_tooltip     <- 'Some fields are automatically pre-filled based on standard column names. If errors are encountered, try filtering the dataset on the editor on the left hand side.<br><br>Note - for a non-NONMEM dataset, it is necessary to uncheck the "Coerce Dataset to Numeric" option within the "Built-in Dataset Cleaning Options" tab to retain characters.'
bspop_calc_nca        <- 'Whenever the above settings are adjusted, a new NCA should be generated.'
bspop_dose_value      <- '(Optional) Provide a dose amount to derive metrics normalized by dose.'
bspop_mw_value        <- '(Optional) Provide a molecular weight to derive metrics related to Clearance and Volume (assumed to be in L).'

## Plotting options
select_x_axis         <- 'Select X-axis'
select_y_axis         <- 'Select Y-axis'
select_color          <- 'Color by:'
select_median         <- 'Median line by:'
median_line_label     <- 'Median line'
log_x_axis_label      <- 'Log X-axis'
log_y_axis_label      <- 'Log Y-axis'
plot_title_label      <- 'Plot Title'
insert_lm_eqn_label   <- 'Linear Regression'
insert_smoother_label <- 'Smoother'
facet_by_label        <- 'Facet(s) by:'
plot_title_placeholder<- "Insert optional plot title"
select_label_size_label<- "Text Label Size"
bspop_select_label_size<- "Label sizes are only applicable for linear regression formulae, counts (N=x), or dosing info."
quantize_x_label       <- "Quantize X-axis"
bspop_quantize_x       <- "(Optional) Split a continuous (i.e. numeric) X-variable into a number of discrete quantiles with approximately equal number of observations in each quantile.<br><br>Note: the plot will be converted into a box plot automatically (not grouped by ID, unlike the normal box plot option)."

add_data_stat_sum     <- 'Median line [dataset]'
bspop_data_stat_sum   <- 'Insert a line connecting median values for grouped observations (up to 20 quantiled bins) for continuous plots. Bins that are within 5% to each other will be merged.'
boxplot_label         <- 'Box Plot (by unique ID)'
bspop_do_boxplot      <- 'Box plot figure assumes the X-axis variable is categorical, the Y-axis variable is continuous, and counts (N) represents the number of unique IDs i.e. usually the baseline value.<br><br>Turning off interactive plots may achieve better visual results for box plots (Color variable is dodged and box plot widths are proportional to the square-roots of the number of observations in the groups).'
bspop_do_data_plotly  <- 'Interactive plots using the "plotly" package could take a longer time for big datasets. Turning it off may achieve better visual results for box plots.'# <br><br>Note - "Download Options" do not apply when plots are not interactive.'
label_fixed_scale     <- 'Same Scale'
bspop_fixed_scale     <- 'Check to ensure the same scale is used for all plots, otherwise uncheck to allow free X- and Y- scaling for each sub-plot.' # <br><br>Note: For Log axis, the same scale is used for plots on the same page only.'
label_insert_dosing   <- 'Show Dose Info'
bspop_insert_dosing   <- 'Shows dosing info as scaled vertical lines in continuous plots, with text labels denoting each unique dose (if "EVID" column is found, and expanded accordingly by "ADDL", "II", and "RATE").<br><br>Note: Scales may be affected due the way dose lines are handled. Turning off interactive plot may achieve better results.'
label_dose_colname    <- 'Dose Column'
bspop_dose_colname    <- 'Provide column name that contains the dosing info, usually "AMT" or "DOSE".'
label_dose_units      <- 'Dose Units'
bspop_dose_units      <- '(Optional) Provide units to be appended on to the text label.'
label_lloq_colname    <- 'LLOQ Column' # shiny::HTML(paste0("LLOQ Column <i class='fa fa-question-circle' title='(Optional) Provide LLOQ/BLQ column to be plotted as a dashed horizontal line. Not applicable for box plots.' style='margin-left: 3px;'></i>"))
bspop_lloq_colname    <- '(Optional) Provide LLOQ/BLQ column to be plotted as dashed horizontal lines.'

bspop_download_plot   <- 'Adjust output settings in the "Download Options" box below.<br><br>This button will be disabled when the plot is interactive, instead please download by using the Camera icon on the top right of the plot.'
plotly_filename_label <- "Plot name"
bspop_plotly_file_name_label <- 'The plot can be downloaded by hovering over the plot and clicking on the Camera icon (for interactive plots), or by clicking the Download Plot button (for non-interactive plots).'
plotly_format_label   <- "File format"
plotly_width_label    <- "Width (px)"
plotly_height_label   <- "Height (px)"
bspop_plotly_width_height <- 'Leave empty to use current dimensions.'
bspop_plotly_width_height_corr <- 'Leave empty to use current dimensions.<br><br>Tip: a larger size (e.g. >5000) is recommended if there are more than a few variables.'
bspop_data_plot_options <- 'Most options set here (with the exception of "Facet by" and "Quantize X-axis") will be carried over to the "Ind. Plot" tab.'
bspop_download_plot_all <- 'Generate and download all individual profiles in a single file (.pdf only). Adjust output settings in the "Download Options" box below.<br><br>Note: It can be time consuming if there are a large number of subjects! (Tip - More plots per page tends to be quicker.)'
bspop_ind_plot_options  <- 'Most options (with the exception of "Facet by") are carried over from the "General Plot" tab.<br><br>Note: Smoother and median lines are not relevant for individual plots and are not inserted.'
label_filter_by_id      <- 'Select ID(s):'
bspop_filter_by_id      <- '(Optional) Page, Row, and Column settings will be ignored when any IDs are selected.'
label_sort_by_ind       <- shiny::HTML(paste0("Sort by: <i class='fa fa-question-circle' title='(Optional) Arrange the order of plots by one or more variables (default is ID only). Note: First value of each ID is used (not time-varying).' style='margin-left: 3px;'></i>"))
bspop_sort_by_ind       <- '(Optional) Arrange the order of plots by one or more variables (default is ID only).<br><br>Note: First value of each ID is used (not time-varying).'
label_strat_by_ind      <- shiny::HTML(paste0("Flag Outliers by: <i class='fa fa-question-circle' title='(Optional) Try to stratify by a group where potential outliers (more than X% above/below the arithmetic mean of the group) will be appended to the ID label. Not applicable for box plots.' style='margin-left: 3px;'></i>"))
bspop_strat_by_ind      <- '(Optional) Stratify by a grouping variable (usually a dose group).<br><br>Note: First value of each ID is used for stratifying (not time-varying).'
label_outlier_threshold <- 'Outlier Threshold:'
bspop_outlier_threshold <- '(Optional) Define a threshold (%) where if the arithmetic mean of the Y-axis of an ID is above/below the corresponding stratification group by this %, that ID will be flagged as a potential outlier. Only applicable when Flag Outliers is used.'
label_highlight         <- shiny::HTML(paste0("Flag Variable: <i class='fa fa-question-circle' title='(Optional) First select a variable and then choose any of its value(s) to be highlighted in bigger asterisks. Not applicable for box plots.' style='margin-left: 3px;'></i>"))
bspop_highlight_var     <- '(Optional) First select a variable and then choose any of its value(s) to be highlighted in bigger asterisks (only applicable for non- box plots).'
label_highlight_values  <- 'Flag Value(s):'
plotly_filename_all_label <- 'Plot name (all individuals)'
plotly_format_all_label <- "File format"
bspop_plotly_file_name_all_label <- 'All individual profiles can be downloaded by clicking the "Generate All Individuals" button.'
plotly_width_all_label <- 'Width (inches)'
plotly_height_all_label <- 'Height (inches)'

## shinyAce placeholder
code_editor_init      <- '# Please apply all filtering and data cleaning directly using dplyr syntax.\n# e.g.: filter(...) %>% mutate(...)\n# Note: Please wait a few seconds after editing code before pressing Apply.\n\nfilter() %>% mutate()'

#### Page 2 Simulation
bspop_select_model    <- 'Choose a template model, or select "Blank Template" to use a basic model as a starting point. Uploading of .cpp files is also supported. After finishing code editing, click the "Generate Model" button.<br><br>Note: Code changes are not permanent and will be reset when a different model is selected.'
bspop_param_values    <- 'Once a model has been generated, all fixed effects parameters from the model code (i.e. inside $PARAM) will become available to be adjusted dynamically in real-time (however it does not feedback into the model code text).<br><br>Note: Non-sensible / unphysiological values may result in the app crashing and should be avoided.'
bspop_download_cpp_model <- 'Save current model, dosing, and demographics info to a standalone .zip bundle, with a simulation-ready R script for reproducibility. Any real-time changes to the parameter values will be included.<br><br>Note: The model must first be successfully generated before the code can be downloaded!'
bspop_model_code      <- 'Refer to the mrgsolve user guide (provided on the first line in the code editor) on the required syntax. After editing, click on the "Generate Model" button.<br><br>Note: If the model fails to compile, check the "Model Info (Console)" box below for more information.'
bspop_model_info_console <- 'If the model build was successful, the generated model info will be summarized below. Otherwise, it will display the error message during compilation.'
llm_pdffile_label     <- shiny::HTML(paste0("Upload File(s) ",
                                            '<span style="color: red; font-weight: bold;">[Experimental]</span>',
                                            "<i class='fa fa-question-circle' title='This feature requires a valid API key associated with the selected LLM provider. For quicker results (and reduced token usage), the file(s) should contain only relevant pages (e.g. model description, parameters, equations, and/or schematic).' style='margin-left: 3px;'></i>"))
llm_parse_locally    <- shiny::HTML(paste0("Parse Locally",
                                           "<i class='fa fa-question-circle' title='Parse PDF / text file (.txt/.mod/.ctl) locally and bypass file upload, which may be required if the LLM provider does not natively support file attachments (it can also reduce token usage). Note that this method extracts only text, which may be less accurate for files with lots of images.' style='margin-left: 3px;'></i>"))
llm_temp_label       <- shiny::HTML(paste0("Temperature",
                                           "<i class='fa fa-question-circle' title='0 = more deterministic, 1 = more creative (Newer models (from mid-2026) tend NOT to support this setting).' style='margin-left: 3px;'></i>"))
llm_seed_label       <- shiny::HTML(paste0("Seed",
                                            "<i class='fa fa-question-circle' title='Seed for reproducibility (Not all providers support this setting).' style='margin-left: 3px;'></i>"))
llm_memory_label     <- shiny::HTML(paste0("Memory",
                                           "<i class='fa fa-question-circle' title='Keep same conversation during retries to improve results at the cost of increased token usage.' style='margin-left: 3px;'></i>"))
llm_translate_label  <- shiny::HTML(paste0("Translate to:",
                                           "<i class='fa fa-question-circle' title='NONMEM and RxODE2 translation is provided for testing purposes only (no retries will be done as MVP only supports mrgsolve).' style='margin-left: 3px;'></i>"))
llm_deep_pdfscan_label  <- shiny::HTML(paste0("Vision",
                                           "<i class='fa fa-question-circle' title='Convert each page of a PDF into image data for more accurate parsing (slower).' style='margin-left: 3px;'></i>"))
llm_force_parse_label  <- shiny::HTML(paste0("Force Parse",
                                           "<i class='fa fa-question-circle' title='Enforce parsing even if previous model was cached.' style='margin-left: 3px;'></i>"))
llm_nonmem2mrgsolve_label  <- shiny::HTML(paste0("nonmem2mrgsolve",
                                             "<i class='fa fa-question-circle' title='Use nonmem2mrgsolve to deterministically parse NONMEM files initially, when the user uploads EXACTLY 2 files: a .ctl/.mod file AND a .ext file.' style='margin-left: 3px;'></i>"))
llm_nonmem2rx_label    <- shiny::HTML(paste0("nonmem2rx",
                                                 "<i class='fa fa-question-circle' title='Use nonmem2rx to deterministically parse NONMEM files, when the user uploads AT LEAST a model file (.ctl/.mod/.lst/.xml), and optionally any auxiliary files (.phi/.ext/.cov/.grd).' style='margin-left: 3px;'></i>"))
llm_accept_single_types <- c(".pdf", ".txt", ".docx", ".html", ".md", ".markdown", ".xls", ".xlsx", ".csv", ".ppt", ".pptx", ".eml", ".msg", ".json", ".yaml", ".yml", ".mod", ".ctl", ".cpp")
llm_accept_multi_types  <- c(".pdf", ".txt", ".mod", ".ctl", ".cpp")
llm_choices_label     <- shiny::HTML(paste0("Select LLM",
                                            "<i class='fa fa-question-circle' title='MVP supports several main LLM providers. This requires the .Renviron file be configured correctly (note: the api_chat URL argument must also be provided if using a OpenAI-compatible API).' style='margin-left: 3px;'></i>"))
llm_max_retries_label <- shiny::HTML(paste0("Retries ",
                                            "<i class='fa fa-question-circle' title='Number of automatic fix attempts if model compilation fails.' style='margin-left: 3px;'></i>"))
llm_compile_success   <- paste0("\u2705 Internal compilation check OK! Please still check code for correctness.")
llm_compile_error     <- paste0("\u274c Internal compilation failed!")
llm_max_retries_error <- paste0("\u274c Max retries reached. Model still contains errors.")



## Dosing Regimen
placeholder_password  <- 'Optional password to unlock more model choices and changes default values.'
bspop_password        <- 'Please contact the App maintainer if you would like your current project to be included as a passworded-model.<br><br>Note: This feature is intended for Project Pharmacometricians to facilitate internal discussions only.'
bspop_password_github <- 'Please visit the <a href="https://github.com/stevechoy/MVPapp" target="_blank">MVPapp GitHub</a> page to install MVP as a R package to use this feature locally.'
bspop_generate_model  <- "Please wait a few seconds after selecting models or editing code before clicking the Generate Model button."
bspop_download_session <- "Download session settings as .rds file.<br><br>Note: Real-time changes to parameter values will not be recorded, i.e. the parameters will be saved according to what has been defined in the code editor."
bspop_upload_session  <- "Restore previous session settings (.rds file). Please wait a few seconds during restoration.<br><br>If you used both models, please also navigate to the other model tab after restoring settings to have it re-populated."
label_dose            <- 'Dose Amount'
bspop_dose            <- 'Dose Amount is intentionally unitless, as the user has the flexibility to adjust the Y-axis label option in "Plotting Options".<br><br> For dosing in molecular weight or weight-based dosing, navigate to the Transform Dose tab.'

label_para            <- 'Simulate in Parallel (experimental)'
bspop_para            <- 'Simulation in parallel is only relevant for multiple subjects (i.e. in the Variability tab).<br><br> When the number of subjects being simulated is >= the threshold set in "Subjects Threshold for Parallelization", parallel simulation will be utilized.'
label_para_n          <- 'Subjects Threshold for Parallelization'

bspop_dosing_options  <- 'Up to 5 completely independent dosing regimens per Model are supported (e.g. loading dose + maintenance doses), which will be combined to form an overall dosing scenario.<br><br>Dosing amounts can be scaled i.e. molar doses or weight-based dosing in the "Transform Dose" sub-tab.'
label_first_dose_time <- 'First Dose Time'
label_input_cmt       <- 'Input CMT'
label_total_doses     <- 'Total Doses'
label_ii              <- 'Dosing Interval'
bspop_time_units      <- 'Time units are dependent on model code, which are typically in hours'
bspop_time_sampling   <- paste0('Time units are dependent on model code, which are typically in hours.<br><br>Note: Total samples (i.e. Max Sampling Time * Sampling Frequency) should not exceed ', max_samples, ' to avoid memory issues.')

label_infdur          <- 'Inf. Duration'
bspop_infdur          <- 'If either "Model Duration" or "Model Rate" option is checked, Infusion Duration will be ignored. <br><br>Note: the mrgsolve code needs to have the D_[compartment] (if modeling duration) or R_[compartment] (if modeling rate) syntax in $MAIN or the code will fail.'
bspop_infdur_cb       <- 'If the "Model Duration" option is checked, Infusion Duration will be ignored  (this option toggle will be handled automatically). <br><br>Note: the mrgsolve code needs to have the D_[compartment] syntax in $MAIN or the code will fail.'
bspop_rate            <- 'If the "Model Rate" option is checked, Infusion Duration will be ignored (this option toggle will be handled automatically). <br><br>Note: the mrgsolve code needs to have the R_[compartment] syntax in $MAIN or the code will fail.'

bspop_solversettings  <- 'Please visit the <a href="https://mrgsolve.org/user-guide/components.html#solver-settings" target="_blank">mrgsolve user guide</a> for an explanation of the solver settings.'

## Simulation options
label_multi_number    <- 'MW Multiplication Factor'

label_MW              <- 'Molecular Weight (g/mol)'
bw_MW                 <- 'Will only be applied when the "Molecular Weight" checkbox is applied.'

set_max_observation   <- 'Max Sampling Time'
set_delta_observation <- 'Sampling Frequency'

label_MW_checkbox     <- 'Molecular Weight'
bspop_MW_checkbox     <- 'When checked, Dose Amount will be divided by MW and then multiplied by the Multiplication Factor.'

label_wt_based_dosing_checkbox <- "Weight-Based Dosing"
bspop_wt_based_dosing_checkbox <- 'When checked, Dose Amount will be multiplied by the Weight parameter as defined in $PARAM.'
label_wt_based_dosing_name     <- "Name of Weight Parameter"
bspop_wt_based_dosing_name     <- "Name provided here must match what is defined in $PARAM."

model_dur_checkbox    <- 'Model Duration (Auto)'
label_filter_cmt      <- 'Filter CMT:'
bspop_custom_sampling_text <- "The Custom Sampling Times checkbox needs to be set for this to take effect. Only accepts numeric vectors. <br><br>Will override the Max Sampling Time and Sampling Frequency."
bspop_custom_sampling_cb <- "When checked, the Custom Sampling Schedule will override the Max Sampling Time and Sampling Frequency."
bspop_time_0_text     <- 'Append Sampling at Time 0. This option is recommended when modeling PD to better calculate change from baselines.<br><br>Note: Dosing events are not counted as valid sampling times.'
model_rate_checkbox   <- 'Model Rate (Auto)'


## Plotting options
label_x_axis          <- 'X-axis Label'
label_y_axis          <- 'Y-axis Label'

label_scale_x_axis    <- 'Scale X-axis (Divide by)'
bspop_scale_x_axis    <- 'Hours, Days, Weeks, Months corresponds to dividing by 1 (no change), 24, 168, or 672 (a "month" is 4 weeks; note: this assumes original time unit is in hours). <br><br>X-axis labels will be automatically changed. <br><br>Users can also insert their own custom scaling if desired (reminder: change the x-axis label accordingly).'
bspop_x_axis_label    <- 'Automatic tick sizes will be provided if the labels match one of the following:<br><br>Time (hours), Time (days), Time (weeks), Time (months)<br><br>Note - change the label slightly if you do not want automatic tick sizes, e.g. "Time (h)" instead of "Time (hours)"'

label_y_axis_sim_1    <- 'Select Y-axis for Model 1'
label_y_axis_sim_2    <- 'Select Y-axis for Model 2'
bspop_y_axis_sim      <- 'Variables inside $CAPTURE and $CMT in the model code will be available for selection.'

label_y_axis_data     <- 'Select Y-axis for Dataset'
label_color_data_by   <- 'Color by:'
bspop_color_data_by   <- 'Stratify the observations by the selected variable in the dataset (column name).<br><br>Note: Click on the legend in the interactive plot to toggle on/off each sub-stratification.'
label_stat_sum_data_by<- 'Median line for Dataset by:'
bspop_stat_sum_data_by<- 'Displays median line for each unique value in the selected variable from the dataset (column name). Defaults to the user-defined "Color by" choice.'
label_main_sim_plot   <- 'Simulated Model Plot'

show_model_1_label    <- "Show Model 1 (red)"
show_model_2_label    <- "Show Model 2 (blue)"
bspop_do_sim_plotly   <- 'Interactive plots using the "plotly" package could take a long time for simulations with lots of data (e.g. from overlaying datasets).' #<br><br>Note - "Download Options" do not apply when plots are not interactive.'


label_combine_nm_data <- 'Overlay Dataset'
bspop_combine_nm_data <- "Requires a valid dataset to be uploaded first on the Data Input page."
add_sim_geom_point    <- 'Show Sampling Points [sims]'
add_data_geom_point   <- 'Show Sampling Points [dataset]'

unsupported_dataset   <- 'No Dataset imported or ID, DV, and TIME columns are not present.'

#### Page 3 Parameter Sensitivity Analysis
bspop_select_psa      <- 'Select any structural parameter from the model (i.e. inside $PARAM) carried over from the Simulation tab to perform sensitivity analysis.'
add_geom_vline        <- 'Show Time Intervals'
add_geom_ribbon       <- 'Highlight AUC'
bspop_adjust_param    <- 'Once a parameter has been selected, 3 separate profiles perturbing the parameter value (default: 50% [green], 100% [orange], 150% [blue]) will be simulated.'
bspop_metrics_by_param_range <- 'PK/PD metrics are updated dynamically for each profile based on the selected time range (see right hand side).<br><br>Notes:<br>Cmax/Cmin/Tmax/Tmin returns the first occurrence if there are multiple matches.<br>Cavg is derived as AUC divided by the length of the time range.<br>AUC is derived using trapezoidal rule (linear up/down).<br>Mean %CFB is derived using mean(((Y-variable - Baseline) / Baseline) * 100), i.e. only accurate if the time samples are equidistant.'

min_param_multiple    <- 0.5 # i.e. 50%
mid_param_multiple    <- 1   # i.e. no change
max_param_multiple    <- 1.5 # i.e. 150%

digits_name           <- "Number of Digits"
dp_checkbox_name      <- "Use decimal places"
bspop_dp_checkbox     <- "By default the rounding uses significant digits. Check this box to use rounding by decimal places instead."

bspop_select_time_interval <- "(Optional) Select the desired time intervals to derive metrics shown on the left hand side of the page (e.g. for partial AUC). The original time scale is provided for selection, independent of whether any scaling of the x-axis from the Simulation page was performed.<br><br>Note: The time interval is inclusive on both ends (i.e. start time <= time interval <= end time). If a dose is given at the end time, the pre-dose (trough) value is used; if a dose is given at the start time, the post-dose value is used."
label_batch_table     <- shiny::HTML(paste0("Batch Run Parameters: <i class='fa fa-question-circle' title='(Optional) Tweak any parameter value for batch runs. Changes to reference values will automatically update the corresponding bounds. Edit upper/lower bounds last to avoid values being reset. Note: Changes will be lost when the original model updates from the \"Simulation\" page.' style='margin-left: 3px;'></i>"))
bspop_batch_table     <- '(Optional) Tweak any parameter value for batch runs. Changes to reference values will automatically update the corresponding bounds. Edit upper/lower bounds last to avoid values being accidentally overwritten. Note: Changes will be lost when the original model updates from the \"Simulation\" page.'
bspop_trim_tor         <- 'Filter the data to only display the top X number of parameters, sorted in order of impact on metric.'
#label_trim_tor        <- shiny::HTML(paste0("Param Numbers: <i class='fa fa-question-circle' title='Filter the data to only display the top X number of parameters.' style='margin-left: 3px;'></i>"))
label_lower_multiplier <- "Lower Multiplier"
label_upper_multiplier <- "Upper Multiplier"
label_tor_show_digits  <- 'Display Values Verbatim'
bspop_tor_show_digits  <- 'Display all parameter values as-is (i.e. treating it as a character and not rounded to 2 dp in the UI).<br><br>Note: This option should be toggled before editing the table as values may reset.'
label_tor_do_gradient  <- 'Gradients'
bspop_tor_do_gradient  <- 'Insert additional (n=3) equidistant batch runs between each bound and the reference value, i.e. 6x more runs in total for better coverage between the bounds (recommended for spider plots, does not affect tornado plots).<br><br>E.g. If a bound is 2 and the reference value is 10, 3 extra runs (4, 6, 8) will be added.'
label_tor_variable     <- 'Select Variable to Derive Metrics'
label_tor_display      <- 'Display as:'
label_trim_tor         <- 'Limit Params'
label_tor_display_text <- "Show Text"
bspop_tor_display_text <- "Displays effect size on each end of the bar. Effect sizes which are <1% change relative to the reference are not displayed. Only applicable for tornado plots."
label_spi_normalize    <- "Normalize Fold-Change"
bspop_spi_normalize    <- "Normalize Parameter Fold-Change for spider plots, i.e. treating x-axis as discrete factors."
bspop_show_bioeq       <- 'Insert visual guide to represent the bioequivalence criteria of 80% - 125%.'
label_show_bioeq       <- 'Bioequivalence'

label_fix_parameters  <- shiny::HTML(paste0("Fix Parameters: <i class='fa fa-question-circle' title='(Optional) Remove any parameters from the list of batch runs. Note: The entire table will be reset.' style='margin-left: 3px;'></i>"))
bspop_reset_reference <- 'Press to reset all reference values back to the original model from \"Simulation\" page.'
bspop_generate_batch  <- 'Press to generate batch runs according to the Batch Run Parameters Table. The total number of simulations are 2 * number of parameters + 1 (the reference model).<br><br>Note: if \"Gradients\" is selected, the total number of simulations will be 8 * number of parameters + 1.'
bspop_batch_runs      <- 'Perform sensitivity analysis for all parameters to visualize the impact on exposure metrics, ordered by relative importance (Tornado Plots and Spider Plots). Parameters are pre-configured with a upper/lower bound (default: \u00B150%).<br><br>Press the "Batch Run" button after any further adjustments to the table below.' #<br><br>Note: could be time consuming for larger models and/or frequent sampling.'
bspop_bounds          <- 'Pre-configures batch runs by multiplying the reference value of all parameters by this number as the upper/lower bound.<br><br>The Table can also be further fine-tuned (i.e. directly edit) for all values, including the reference value.'


#### Page 5 Variability
bspop_varmat          <- 'Initial values are carried over from the Simulation Tab. For the first model in the session, click on "Update Model" to initialize the plot.<br><br>As long as it is the same model, the plot will be refreshed whenever fixed effects ($PARAM), dosing, or demographics are updated (either from this page or from the Simulation Tab). Any changes to the variability values below require clicking on "Update Model" to refresh the plot.'
bspop_varsim          <- 'Pre-configured demographics from external databases and/or custom covariate distributions can be inserted to be used as the input dataset for the simulations.<br><br>Note: The relevant covariate name(s) must first exist in the $PARAM section for it to be replaced.'
max_sim_n             <- 5000 # Max number of allowed simulated subjects
matrix_info_message   <- "If the user input values don't match the current model matrices below after pressing Update, \nit means the user input is invalid and has caused an error."
bspop_update_model    <- "After pressing Update, please also check the Matrix Info below to confirm the inputs are valid. If not, the model will re-use the last known valid matrices."
bspop_percentiles     <- "Note: Percentiles are applicable only when Show Individual Profiles is unchecked, i.e. does not affect individual subjects."
bspop_nsubj_warning   <- paste0("Caution is advised when simulating a large number of subjects (e.g. >1000) with a frequent sampling schedule may result in long computational times and/or app instability.<br><br>Note: Maximum number of subjects able to be simulated is currently capped to ", max_sim_n,".")
max_sim_n_error       <- paste0("Maximum number of subjects cannot exceed ", max_sim_n, ". Reverting to using 20.")
label_db              <- "Select Virtual Patients Database:"
bspop_db              <- 'The choice of database is specific to project needs, where:<br><br>"None" - only number of subjects and seed are relevant, other settings are disabled (apart from custom covariates, if defined).<br><br><a href="https://wwwn.cdc.gov/nchs/nhanes/Default.aspx" target="_blank">NHANES - All ages</a> (general population [1999 - 2023], usually suitable for adults (>18))<br><br><a href="https://www.cdc.gov/growthcharts/percentile_data_files.htm" target="_blank">CDC - Age 0 - 20 yo</a> (general ped use between 2 - <18 yo)<br><br><a href="https://www.who.int/toolkits/child-growth-standards/standards/weight-for-age" target="_blank">WHO - Age 0 to 10 yo</a> (better granularity for neonates to toddlers)<br><br>Note: The covariates will replace whatever it is defined in $PARAM'
bspop_seed            <- 'Seed number is applicable to both the sampling of databases (including covariates) AND simulations (uses current date as default value).'
bspop_select_y_value_threshold <- 'The percentage (%) of subjects above the selected y-value at a certain time will be displayed below.'
age_range_label       <- "Age Range ('AGE', y) [Post-Natal]"
weight_range_label    <- "Weight Range ('WT', kg) [Total Body Mass]"
bmi_range_label       <- "Body Mass Index Range ('BMI', kg/m\u00b2)"
male_range_label      <- "Male Percentage ('SEX = 0', %) [Biological]"
custom_cov_label      <- "Covariate Name"
custom_cov_placeholder<- "E.g. EGFR"
bspop_custom_cov      <- "Covariate names must match the corresponding parameter name in the model code (i.e. inside $PARAM) for it to take effect.<br><br>E.g. If EGFR is defined inside $PARAM in the model code, the covariate name should also be called EGFR to replace the pre-defined parameter value.<br><br>Note: name cannot be AGE, AGEMO, SEX, WT, BMI, BSA, and must be unique from each other."
custom_cov_dist_lab   <- "Select Distribution"
bspop_cov_mean        <- "Mean value of the covariate to be used for a normal distribution."
bspop_cov_meanlog     <- "Mean value of the covariate to be used for a log-normal distribution. Log of this value will be taken (cannot be < 0)."
bspop_cov_sd          <- "Note that standard deviations cannot be negative."
bspop_cov_sdlog       <- "Log of this value will be taken (cannot be < 1)."
bspop_catvalue1       <- "The percentage slider determines the approximate proportion of subjects with this numeric value."
bspop_catvalue2       <- "(100 - percentage slider) determines the approximate proportion of subjects with this numeric value."
bspop_do_iiv_plotly   <- 'Interactive plots using the "plotly" package could take a long time for simulations with more than a few hundred subjects.'#<br><br>Note - "Download Options" do not apply when plots are not interactive.'
show_mean_iiv_label   <- 'Show Mean Trend'
bspop_select_time_interval_exp <- "(Optional) Select the desired time intervals to derive metrics to be plotted above. The original time scale is provided for selection, independent of whether any scaling of the x-axis from the Simulation page was performed.<br><br>Note: The time interval is inclusive on both ends (i.e. start time <= time interval <= end time). If a dose is given at the end time, the pre-dose (trough) value is used; if a dose is given at the start time, the post-dose value is used."
use_dataset_cov_dist_label <- shiny::HTML(paste0("Use Distributions from Dataset: <i class='fa fa-question-circle' title='(Optional) If a dataset has been uploaded, you may pull in covariate distribution(s) / EBEs (baseline, i.e. subject-level) to replace parameters with the same name in the model. Note: If the number of unique IDs equals the number of subjects to be simulated, it will be sampled without replacement (i.e. 1:1). Otherwise, it will be sampled with replacement.' style='margin-left: 3px;'></i>"))
bspop_download_demog  <- 'Download generated virtual patients (and any custom covariates, if applicable) in a .csv file.'

#infoBox
infoBox_width         <- 3
font_size             <- 'font-size: 23px;'

## css formatting
navbar_bg_color <-   htmltools::tags$head(
  htmltools::tags$style(
    htmltools::HTML('.navbar-static-top {background-color: #003366;}',
                    '.navbar-default .navbar-nav>.active>a {background-color: #003366;}',
                    '.navbar-default .navbar-nav>li>a:hover {background-color: transparent;}',
                    # '#param_output_model_1 {font-size: 10px; }', 
                    # '#param_output_model_1 {font-size: 12px; }',
                    '#console_output_model_1 { font-size: 10px; }',
                    '#console_output_model_2 { font-size: 10px; }',
                    '#console_output_iiv_model_1 { font-size: 10px; }',
                    '#console_output_iiv_model_2 { font-size: 10px; }',
                    '#console_data_1 { font-size: 10px; }',
                    '.popover {
             max-width: 500px;
             height: auto;
             overflow-y: auto !important; /* Add a scrollbar if the content exceeds the max-height */
          }'
    )
  ),
  ## Scroll bar focus workaround from https://stackoverflow.com/questions/60110678/in-shiny-dt-the-focus-is-lost-when-there-is-lot-of-columns-with-scrollx-true
  ## Method 1 CSS - most are commented out
  # htmltools::tags$style(type = "text/css", htmltools::HTML(paste0(".datatables                       {position: relative; } "))),
  # htmltools::tags$style(type = "text/css", htmltools::HTML(paste0(".datatables  [class^='col-']      {position: inherit;} "))),
  htmltools::tags$style(type = "text/css", htmltools::HTML(paste0("
                                                   .datatables .dataTables_wrapper   {
                                                         overflow-x: auto;
                                                         overflow-y: hidden;
                                                         padding-top: 35px;
                                                         padding-bottom: 35px;
                                                   }"))),
  # htmltools::tags$style(type = "text/css", htmltools::HTML(paste0(".datatables .dataTables_length    {position: absolute; margin-top: -35px; left:  0px;}"))),
  # htmltools::tags$style(type = "text/css", htmltools::HTML(paste0(".datatables .dataTables_filter    {position: absolute; margin-top: -35px; right: 0px;}"))),
  # htmltools::tags$style(type = "text/css", htmltools::HTML(paste0(".datatables .dataTables_info      {position: absolute; left:  0px;}"))),
  # htmltools::tags$style(type = "text/css", htmltools::HTML(paste0(".datatables .dataTables_paginate  {position: absolute; right: 0px;}"))),
  # htmltools::tags$style(type = "text/css", htmltools::HTML(paste0("
  #                                                  .datatables .dataTables_wrapper .dt-buttons  {
  #                                                        left: 150px;
  #                                                        position: absolute;
  #                                                        top: 0;
  #                                                  }")))#,
)

shinydashboard_box_format <- htmltools::tags$style(htmltools::HTML("
                                         .box.box-solid.box-primary>.box-header {
                                            color:#fff;
                                            background:#003366
                                                              }
                                          
                                          .box.box-solid.box-primary{
                                          border-bottom-color:#003366;
                                          border-left-color:#003366;
                                          border-right-color:#003366;
                                          border-top-color:#003366;
                                          }
                                          
                                          .popover{
                                          width:400px;
                                          height:270px;   
                                          }
                                                         ")
)

#-------------------------------------------------------------------------------
# Optional branding (mvp_app(brand_color = , brand_text_color = , brand_logo = ,
# brand_favicon = , page_title = )). Everything below is only used when one of
# these arguments is not NULL; the default look is unchanged.
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
#' @name brand_hex
#' @title Validate a colour and return it as "#RRGGBB"
#' @param color a colour name ("orange") or hex string
#' @param arg argument name, for the error message
#' @returns a character string
#' @noRd
#-------------------------------------------------------------------------------
brand_hex <- function(color, arg = "brand_color") {
  if (!is.character(color) || length(color) != 1 || is.na(color) || !nzchar(color)) {
    stop("`", arg, "` must be a single colour, e.g. \"#F36633\" or \"orange\".", call. = FALSE)
  }
  rgb <- tryCatch(grDevices::col2rgb(color)[, 1],
                  error = function(e) stop("`", arg, "` is not a valid colour: \"", color, "\".", call. = FALSE))
  grDevices::rgb(rgb[1], rgb[2], rgb[3], maxColorValue = 255)
}

#-------------------------------------------------------------------------------
#' Readable text colour ("#FFFFFF" or "#000000") for a background colour
#'
#' White is used when its WCAG contrast ratio with the background is at least 3
#' (the large-text threshold, which also suits the bold navbar), otherwise black.
#' @param bg background colour as "#RRGGBB"
#' @noRd
#-------------------------------------------------------------------------------
brand_contrast_text <- function(bg) {
  rgb <- grDevices::col2rgb(bg)[, 1] / 255
  lin <- ifelse(rgb <= 0.03928, rgb / 12.92, ((rgb + 0.055) / 1.055)^2.4)
  lum <- 0.2126 * lin[1] + 0.7152 * lin[2] + 0.0722 * lin[3]
  if (1.05 / (lum + 0.05) >= 3) "#FFFFFF" else "#000000"
}

#-------------------------------------------------------------------------------
#' URL for a branding image
#'
#' Web (http/https) and data: URLs are used as they are. A local file is served
#' through a Shiny resource path, so it does not have to be inside the package.
#' @param path file path or URL
#' @param prefix resource path prefix for a local file
#' @param arg argument name, for the error message
#' @returns a URL
#' @noRd
#-------------------------------------------------------------------------------
brand_asset_url <- function(path, prefix, arg) {
  if (!is.character(path) || length(path) != 1 || is.na(path) || !nzchar(path)) {
    stop("`", arg, "` must be a single file path or URL.", call. = FALSE)
  }
  if (grepl("^(https?:|data:)", path, ignore.case = TRUE)) return(path)
  if (!file.exists(path) || dir.exists(path)) {
    stop("`", arg, "` file not found: ", path, call. = FALSE)
  }
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  shiny::addResourcePath(prefix, dirname(path))
  paste0(prefix, "/", utils::URLencode(basename(path)))
}

#-------------------------------------------------------------------------------
#' @name resolve_branding
#' @title Validate the branding arguments of mvp_app() and turn them into the
#'   values used by the app
#' @param params named list of the mvp_app() settings
#' @returns `params`, with the brand settings validated (colours as "#RRGGBB",
#'   images as URLs); settings that are NULL stay NULL
#' @noRd
#-------------------------------------------------------------------------------
resolve_branding <- function(params) {
  if (!is.null(params$page_title)) {
    if (!is.character(params$page_title) || length(params$page_title) != 1 || is.na(params$page_title)) {
      stop("`page_title` must be a single character string.", call. = FALSE)
    }
  }
  if (!is.null(params$brand_color)) {
    params$brand_color <- brand_hex(params$brand_color, "brand_color")
    params$brand_text_color <- if (is.null(params$brand_text_color)) {
      brand_contrast_text(params$brand_color)
    } else {
      brand_hex(params$brand_text_color, "brand_text_color")
    }
  } else if (!is.null(params$brand_text_color)) {
    warning("`brand_text_color` is ignored because `brand_color` is not set.", call. = FALSE)
    params$brand_text_color <- NULL
  }
  if (!is.null(params$brand_logo)) {
    params$brand_logo <- brand_asset_url(params$brand_logo, "mvp-brand-logo", "brand_logo")
  }
  if (!is.null(params$brand_favicon)) {
    params$brand_favicon <- brand_asset_url(params$brand_favicon, "mvp-brand-favicon", "brand_favicon")
  }
  params
}

#-------------------------------------------------------------------------------
#' @name default_favicon_tags
#' @title Favicon links for the MVP icon shipped with the package
#'
#' Used when `brand_favicon` is not set: two concentration-time curves on the
#' navy of the default theme. An SVG for modern browsers, with a PNG for the
#' ones that do not support SVG icons.
#' @returns a list of `<link>` tags (empty if the icon files are not installed)
#' @noRd
#-------------------------------------------------------------------------------
default_favicon_tags <- function() {
  dir <- system.file("favicon", package = "MVPapp")
  if (!nzchar(dir)) return(list())
  shiny::addResourcePath("mvp-favicon", dir)
  list(htmltools::tags$link(rel = "icon", type = "image/png", sizes = "64x64", href = "mvp-favicon/mvp-favicon.png"),
       htmltools::tags$link(rel = "icon", type = "image/svg+xml", sizes = "any", href = "mvp-favicon/mvp-favicon.svg"))
}

#-------------------------------------------------------------------------------
#' @name brand_head_tags
#' @title Page head content for the branding: colour overrides and favicon
#' @param color,text_color validated colours ("#RRGGBB") or NULL
#' @param favicon favicon URL or NULL
#' @returns an htmltools tag list (empty if nothing is set)
#' @noRd
#-------------------------------------------------------------------------------
brand_head_tags <- function(color = NULL, text_color = NULL, favicon = NULL) {
  tags <- list()
  if (!is.null(color)) {
    text_color <- text_color %||% brand_contrast_text(color)
    css <- paste0(
      # navbar and active tab
      ".navbar-static-top, .navbar-default {background-color: ", color, " !important; border-color: ", color, " !important;}\n",
      ".navbar-default .navbar-brand, .navbar-default .navbar-nav>li>a {color: ", text_color, " !important;}\n",
      ".navbar-default .navbar-nav>.active>a, .navbar-default .navbar-nav>.active>a:hover, .navbar-default .navbar-nav>.active>a:focus ",
      "{background-color: ", color, " !important; color: ", text_color, " !important; border-bottom: 3px solid ", text_color, ";}\n",
      ".navbar-default .navbar-nav>li>a:hover {background-color: transparent !important; color: ", text_color, " !important; opacity: 0.85;}\n",
      # box headers and borders (shinydashboard "primary")
      ".box.box-solid.box-primary>.box-header {background: ", color, " !important; color: ", text_color, " !important;}\n",
      ".box.box-solid.box-primary {border-bottom-color: ", color, " !important; border-left-color: ", color, " !important; ",
      "border-right-color: ", color, " !important; border-top-color: ", color, " !important;}\n",
      # (sub-tab links such as "Regimen 1" keep the theme's green accent on purpose)
      # company logo in the navbar
      ".mvp-brand-logo {height: 28px; margin: -5px 10px 0 0; vertical-align: middle;}\n"
    )
    tags <- c(tags, list(htmltools::tags$style(htmltools::HTML(css))))
  } else {
    tags <- c(tags, list(htmltools::tags$style(htmltools::HTML(
      ".mvp-brand-logo {height: 28px; margin: -5px 10px 0 0; vertical-align: middle;}\n"))))
  }
  if (!is.null(favicon)) {
    ext  <- tolower(tools::file_ext(sub("[?#].*$", "", favicon)))
    type <- switch(ext, svg = "image/svg+xml", ico = "image/x-icon", jpg = , jpeg = "image/jpeg",
                   gif = "image/gif", "image/png")
    tags <- c(tags, list(htmltools::tags$link(rel = "icon", type = type, href = favicon)))
  } else {
    tags <- c(tags, default_favicon_tags())
  }
  htmltools::tags$head(htmltools::tagList(tags))
}

#-------------------------------------------------------------------------------
#' @name brand_title
#' @title Navbar title: the page title, with the company logo in front if set
#' @param title page title
#' @param logo logo URL or NULL
#' @noRd
#-------------------------------------------------------------------------------
brand_title <- function(title, logo = NULL) {
  if (is.null(logo)) return(htmltools::div(title))
  htmltools::div(htmltools::tags$img(src = logo, class = "mvp-brand-logo", alt = ""), title)
}
