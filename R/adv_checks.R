#' Gower_Mat_Check
#'
#' @param X n x p dataframe object (allows for mixed data types) or a matrix object (single data type)
#'
#' @return Returns an error message if X is not a matrix or data frame
#'
#' @keywords internal
#'
Gower_Mat_Check <- function(X){

  # Check object type
  if(!(is.matrix(X) | is.data.frame(X))){

    stop(paste("data.x input must be a matrix or data frame object. Readjust."))

  }

}

#' Var_Type_Check
#'
#' @param var.type.vec
#'
#' @return Returns an error message if var.type.vec does not follow one or more of the requirements mentioned below.
#'
#' @keywords internal
#'
Var_Type_Check <- function(var.type.vec){

  # Check object type
  if(!(is.vector(var.type.vec))){

    stop(paste("var.type.vec input must be a vector object. Readjust."))

  }

  # Check Dimensions compatible with X
  if(length(var.type.vec) != ncol(X)){

    stop(paste("Length of var.type.vec vector does not match the number of columns of data.x. Check and readjust."))

  }

  # Check vector only contains 0, 1, 2, and/or 3
  if(!all(var.type.vec %in% c(0, 1, 2, 3))){

    stop(paste("Not all elements of var.type.vec vector are 0, 1, 2, or 3. Check and readjust."))

  }

}

#' Var_Weight_Check
#'
#' @param var.weight.vec
#'
#' @return Returns an error message if var.weight.vec does not follow one or more of the requirements mentioned below.
#'
#' @keywords internal
#'
Var_Weight_Check <- function(var.weight.vec){

  # Check object type
  if(!(is.vector(var.weight.vec))){

    stop(paste("var.weight.vec input must be a vector object. Readjust."))

  }

  # Check Dimensions compatible with X
  if(length(var.weight.vec) != ncol(X)){

    stop(paste("Length of var.weight.vec vector does not match the number of columns of data.x. Check and readjust."))

  }

}


















