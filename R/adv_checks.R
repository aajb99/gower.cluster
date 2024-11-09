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



















