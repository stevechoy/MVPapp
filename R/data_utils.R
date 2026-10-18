# Dataset helpers

#-------------------------------------------------------------------------------
#' @name check_and_combine_df
#'
#' @title combined 2 datasets for download
#' @param model_1_is_valid  logical TRUE/FALSE
#' @param model_2_is_valid  logical TRUE/FALSE
#' @param input_df_1        Model 1 simulated data
#' @param input_df_2        Model 2 simulated data
#'
#' @returns a df
#' @importFrom dplyr intersect full_join
#' @noRd
#-------------------------------------------------------------------------------

check_and_combine_df <- function(model_1_is_valid,
                                 model_2_is_valid,
                                 input_df_1 = NULL,
                                 input_df_2 = NULL) {

  if(model_1_is_valid & model_2_is_valid) {

    common_columns <- dplyr::intersect(names(input_df_1), names(input_df_2))
    combined_model <- dplyr::full_join(input_df_1, input_df_2, by = common_columns)

    return(combined_model)

  } else if (model_1_is_valid) {
    return(input_df_1)
  } else if (model_2_is_valid) {
    return(input_df_2)
  } else {stop('There are no valid datasets to be downloaded.')}
}

#-------------------------------------------------------------------------------
#' @name search_id_col
#' @title Search for Likely ID Columns
#'
#' @description
#' Searches through several common column names that could be used to create the
#' "ID" column in the dataset, and push that to the first column of the dataset
#'
#' @param orig_df The dataframe used for searching
#' @param names_of_id_cols Likely column names, in order of search priority
#'
#' @returns a dataframe
#' @importFrom dplyr mutate select
#' @noRd
#-------------------------------------------------------------------------------

search_id_col <- function(orig_df,
                          names_of_id_cols = c("SUBJIDN", "SUBJID", "USUBJID", "PTNO")) {
  derive_column(orig_df, "ID", names_of_id_cols, first = TRUE)
}

#-------------------------------------------------------------------------------
#' @name search_time_col
#' @title Search for Likely TIME Columns
#'
#' @description
#' Searches through several common column names that could be used to create the
#' "TIME" column in the dataset
#'
#' @param orig_df The dataframe used for searching
#' @param names_of_time_cols Likely column names, in order of search priority
#'
#' @returns a dataframe
#' @importFrom dplyr mutate select
#' @noRd
#-------------------------------------------------------------------------------

search_time_col <- function(orig_df,
                            names_of_time_cols = c("TAFD", "TSFD", "ATFD", "ATSD")) {
  derive_column(orig_df, "TIME", names_of_time_cols)
}

#-------------------------------------------------------------------------------
#' @name derive_column
#'
#' @title Create a missing column from the first available alternative column
#'
#' @param df          dataset
#' @param target      column to create, e.g. "ID"
#' @param candidates  columns to copy it from, in order of preference
#' @param first       move the new column to the front
#'
#' @returns df, unchanged when `target` exists or no candidate is found
#' @noRd
#-------------------------------------------------------------------------------
derive_column <- function(df, target, candidates, first = FALSE) {
  source <- intersect(candidates, names(df))[1]
  if (target %in% names(df) || is.na(source)) return(df)

  df <- dplyr::mutate(df, !!target := .data[[source]])
  if (first) df <- dplyr::relocate(df, dplyr::all_of(target))
  shiny::showNotification(paste0(target, " column has been created from '", source, "' column."),
                          type = "message", duration = 10)
  df
}
