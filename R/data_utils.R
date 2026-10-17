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

#-----------------------------------------------------------------------------
#' @name split_data_frame
#' @title Function to split dataframes into roughly equal portions to facilitate
#' distribution to each core
#'
#' @param df data.frame to split
#' @param N number of chunks / cores
#'
#' @returns A list containing N data.frames. If N=1, then the original df
#'         is returned unchanged
#' @noRd
#-----------------------------------------------------------------------------

split_data_frame <- function(df, N = 4) {
  if(N == 1) {
    return(list(df)) # Note we're coercing df to a list when single core is desired
  } else if (N < 1) { # This may be redundant as n.cores already have a safeguard
    stop("N can't be less than one")
  } else if (N > nrow(df)) { # This may be redundant as n.cores already have a safeguard
    stop("N must be less than nrows(df)")
  }

  # split the data.frame into a list
  res <- split(df, cut(seq_len(nrow(df)), N, labels = FALSE))
  return(res)
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

  df <- orig_df

  for(i in seq_along(names_of_id_cols)) {
    if('ID' %in% names(df)) {
      return(df)
    } else {
      if(names_of_id_cols[i] %in% names(df)) {
        df <- df %>% dplyr::mutate(ID = !!dplyr::sym(names_of_id_cols[i])) %>%
          dplyr::select(ID, dplyr::everything())
        shiny::showNotification(paste0("ID column has been created from '", names_of_id_cols[i], "' column."), type = "message", duration = 10)
      }
    }
  } # end of loop

  return(orig_df) # if can't find any
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

  df <- orig_df

  for(i in seq_along(names_of_time_cols)) {
    if('TIME' %in% names(df)) {
      return(df)
    } else {
      if(names_of_time_cols[i] %in% names(df)) {
        df <- df %>% dplyr::mutate(TIME = !!dplyr::sym(names_of_time_cols[i])) #%>%
        #dplyr::select(ID, dplyr::everything())
        shiny::showNotification(paste0("TIME column has been created from '", names_of_time_cols[i], "' column."), type = "message", duration = 10)
      }
    }
  } # end of loop

  return(orig_df) # if can't find any
}
