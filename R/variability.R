# Variability (OMEGA / SIGMA) matrices

#-------------------------------------------------------------------------------
#' @name extract_matrix
#'
#' @title Function to output a matrix from mrgsolve model object
#'
#' @param input_model_object  mrgsolve model object to extract from
#' @param name_of_matrix      either "omega" or "sigma", will use "omat" or "smat" accordingly
#' @param remove_upper_tri    When TRUE, turns upper triangle of matrix to NA_real_
#' @param remove_zero         In case any of them are 0, don't replace the diagonals
#' @param coerce_to_character Default TRUE, turns all output into a character as a workaround to prevent rounding
#' @param debug               When TRUE, show debugging messages
#'
#' @returns a matrix
#' @noRd
#-------------------------------------------------------------------------------

extract_matrix <- function(input_model_object,
                           name_of_matrix      = "omega",
                           remove_upper_tri    = TRUE,
                           remove_zero         = TRUE,
                           coerce_to_character = TRUE,
                           debug               = FALSE) {

  if(debug) {
    message("running extract_matrix() for ", name_of_matrix)
  }

  if(name_of_matrix == "omega") {
    om <- mrgsolve::omat(input_model_object)
    names_list <- as.list(unlist(mrgsolve::labels(om))) # must use mrgsolve::labels
    omsm_matrix <- mrgsolve::collapse_omega(input_model_object, name = "omega") %>%
      mrgsolve::omat() %>%
      mrgsolve::as.matrix() # must use mrgsolve::as.matrix, S4
  }

  if(name_of_matrix == "sigma") {
    sm <- mrgsolve::smat(input_model_object)
    names_list <- as.list(unlist(mrgsolve::labels(sm))) # must use mrgsolve::labels
    omsm_matrix <- mrgsolve::collapse_sigma(input_model_object, name = "sigma") %>%
      mrgsolve::smat() %>%
      mrgsolve::as.matrix() # must use mrgsolve::as.matrix, S4
  }

  if(debug) {
    message(paste(utils::capture.output(print(omsm_matrix)), collapse = "\n"))
  }

  if(remove_upper_tri) {
    omsm_matrix[upper.tri(omsm_matrix, diag = FALSE)] <- NA_real_
  }
  if(remove_zero) {
    diag_elements <- diag(omsm_matrix) # In case any of them are 0, don't replace the diagonals
    omsm_matrix[omsm_matrix == 0] <- NA_real_
    diag(omsm_matrix) <- diag_elements
  }

  if(coerce_to_character) {
    omsm_matrix <- apply(omsm_matrix, c(1, 2), as.character) %>%
      matrix(., nrow = nrow(.), ncol= ncol(.))
  }

  colnames(omsm_matrix) <- names_list

  if(debug) {
    message("finish extract_matrix()")
  }

  return(omsm_matrix)
}

#-------------------------------------------------------------------------------
#' @name reconstruct_matrices
#'
#' @title
#' Converts a matrix object into a list
#' @description
#' Each element in the list corresponds to each omega block, corresponding to provided model object
#'
#' @param input_model_object original model object that contains matrix information
#' @param input_matrix       single collapsed matrix (assumes NA's for upper triangle and non-blocks)
#' @param name_of_matrix     "omega" or "sigma"
#' @param coerce_to_numeric  Default TRUE, will turn all input to numeric
#' @param debug              When TRUE, show debugging messages
#'
#' @note
#' The original model object is needed as the configurations of matrices
#'        are dependent on how $OMEGA and/or $SIGMA is defined in the model code.
#'        It seems like for each repetition of $OMEGA and/or $SIGMA, a new matrix
#'        is created.
#'
#' @returns a list with each element containing a block matrix
#' @noRd
#-------------------------------------------------------------------------------

reconstruct_matrices <- function(input_model_object,
                                 input_matrix,
                                 name_of_matrix    = "omega",
                                 coerce_to_numeric = TRUE,
                                 debug             = FALSE) {

  if(nrow(input_matrix) == 0) {
    if(debug) {
      message('no input matrix')
    }
    return(input_matrix)
  }

  if(debug) {
    message('Reconstructing Matrix')
  }

  ## Store how many matrices there are from the model object
  if(name_of_matrix == "omega") {
    number_of_matrices <- length(input_model_object$omega)
  }

  if(name_of_matrix == "sigma") {
    number_of_matrices <- length(input_model_object$sigma)
  }

  if(length(number_of_matrices) == 0 || number_of_matrices == 0) {
    if(debug) {
      message('model has no ', name_of_matrix, ' matrix')
    }
    return(list())
  }

  if(coerce_to_numeric) {
    input_matrix <- apply(input_matrix, c(1, 2), as.numeric) %>%
      matrix(., nrow = nrow(.), ncol= ncol(.))
  }

  result        <- list()
  start_row_col <- 1

  for(n in 1:number_of_matrices) {

    if(name_of_matrix == "omega") {
      nrow_current_matrix <- nrow(input_model_object$omega[[n]])
    }
    if(name_of_matrix == "sigma") {
      nrow_current_matrix <- nrow(input_model_object$sigma[[n]])
    }

    result[[n]] <- list()

    end_row_col <- start_row_col + nrow_current_matrix - 1
    if(debug) {
      message("Current matrix: ", n, ", start row/col: ", start_row_col)
    }
    result[[n]] <- input_matrix[start_row_col:end_row_col, start_row_col:end_row_col]

    # Mirror lower triangle to upper triangle
    lower_triangle <- lower.tri(result[[n]])
    result[[n]][!lower_triangle] <- t(result[[n]])[!lower_triangle]
    result[[n]][is.na(result[[n]])] <- 0 # Required for omat() as NA's don't work
    result[[n]] <- result[[n]] %>% as.matrix() # Required for 1x1 matrix otherwise it becomes a numeric

    start_row_col <- start_row_col + nrow_current_matrix # advancing to new starting row/col for next matrix

  } # end of number of matrix
  if(debug) {
    message('Matrix reconstruction complete')
  }
  return(result)
}

#-------------------------------------------------------------------------------
#' @name update_variability
#'
#' @title Updates variability for a mrgmod object
#'
#' @param input_model_object   original model object to be updated
#' @param input_matrix         list of matrices containing new parameter values
#' @param name_of_matrix       "omega" or "sigma"
#' @param check_validity       Default TRUE, where it checks that the matrix is sensible
#'                       If check fails, the model object is NOT updated
#' @param debug                When TRUE, show debugging messages
#'
#' @returns a mrgmod object
#' @noRd
#-------------------------------------------------------------------------------

update_variability <- function(input_model_object,
                               input_matrix,
                               name_of_matrix = "omega",
                               check_validity = TRUE,
                               debug          = FALSE) {

  if(length(input_matrix) == 0) {
    return(input_model_object) # nothing to update (e.g. the model has no such matrix)
  }

  if(check_validity) {
    matrix_is_valid <- check_matrix(input_model_object = input_model_object, input_matrix = input_matrix, debug = debug)
    if(!matrix_is_valid) {
      if(debug) {
        message("Matrix not valid. Returning original input model object")
      }
      return(input_model_object)
    } # model object is not updated if matrix is invalid
  }

  # omat()/smat() can still refuse a matrix (e.g. a dimension mismatch); keep
  # the original model object rather than letting the error close the session
  tmp <- tryCatch({
    if(name_of_matrix == "omega") {
      mrgsolve::omat(input_model_object, input_matrix)
    } else {
      mrgsolve::smat(input_model_object, input_matrix)
    }
  }, error = function(e) {
    if(shiny::isRunning()) {
      shiny::showNotification(paste0("ERROR: The ", name_of_matrix, " matrix could not be applied: ", conditionMessage(e)),
                              type = "error", duration = 10)
    }
    input_model_object
  })

  return(tmp)
}

#-------------------------------------------------------------------------------
#' @name check_matrix
#'
#' @title Checks for validity of matrix
#'
#' @param input_model_object mrgmod object
#' @param input_matrix       a list where each element is a matrix
#' @param check_diagonal     Default TRUE, check that each diagonal must be >= 0
#' @param check_symmetry     Default FALSE, check for symmetry
#' @param debug              Show debugging messages
#' @param display_error      Displays a showNotification message pop-up
#'
#' @returns logical (TRUE/FALSE), where TRUE means pass (valid matrix)
#' @importFrom stringr str_detect
#' @noRd
#-------------------------------------------------------------------------------

check_matrix <- function(input_model_object,
                         input_matrix,
                         check_diagonal = TRUE,
                         check_symmetry = FALSE,
                         debug = FALSE,
                         display_error = TRUE) {
  for(i in seq_along(input_matrix)) {

    if(any(!is.finite(input_matrix[[i]]))) {
      if(display_error) {
        shiny::showNotification("ERROR: Each matrix element must be a finite number", type = "error", duration = 10)
      }
      return(FALSE)
    }

    if(check_diagonal) {
      if(any(diag(input_matrix[[i]]) < 0)) {
        if(display_error) {
          shiny::showNotification("ERROR: Each diagonal (variance) must be >= 0", type = "error", duration = 10)
        }
        return(FALSE)
      }
    } # End of check diagonal

    # The table always holds covariances, even when the model code uses
    # @correlation, so the matrix must always be positive semi-definite.
    # A small tolerance avoids rejecting valid rank-deficient blocks whose
    # smallest eigenvalue is a rounding error below zero.
    ev  <- eigen(input_matrix[[i]], symmetric = TRUE, only.values = TRUE)$values
    tol <- sqrt(.Machine$double.eps) * max(1, abs(ev))
    if(any(ev < -tol)) {
      if(display_error) {
        shiny::showNotification("ERROR: The matrix is not positive semi-definite", type = "error", duration = 10)
      }
      return(FALSE)
    } # End of check eigenvalues

    if(check_symmetry) {
      if (!all(identical(input_matrix[[i]], t(input_matrix[[i]])))) {
        return(FALSE)
      }
    } # End of check symmetry
  } # End of each matrix loop

  if(debug) {
    message("Check matrix PASS")
  }
  return(TRUE)
}
