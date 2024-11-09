#' Gower_Cluster
#'
#' @param data.x n x p dataframe object (allows for mixed data types) or a matrix object (single data type)
#' @param var.type.vec p x 1 vector with elements indicating the data type of each variable of X
#' @param var.weight.vec (Optional) p x 1 vector with elements indicating variable weights applied in Gower's Distance calculations
#' @param dend (Optional) boolean input, "TRUE" indicating dendrogram plot is a desired output, "FALSE" otherwise
#'
#' @return A matrix
#' \item{gower.mat}{ n x n matrix representing the pairwise Gower's Distances between observations of data.x }
#' \item{dend.plot}{ (Optional) dendgrogram plot indicating potential clusters of data.x observations based on pairwise Gower's Distances }
#' @export
#'
#' @examples
#'
#' ##### Example 1
#'
#' #####################
#' ##### Example 2
#'
Gower_Cluster <- function(data.x, var_type_vec, var_weight_vec = NULL, dendrogram = FALSE){

  X <- data.x

  # Adversarial Checks #
  # Check 1: X is object type matrix/df
  Gower_Mat_Check(X)
  # Check 2:

  return(gower_mat)
}






