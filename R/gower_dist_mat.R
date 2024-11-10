#' Gower_Cluster
#'
#' @param data.x n x p dataframe object (allows for mixed data types) or a matrix object (single data type).
#' @param var.type.vec p x 1 vector with elements (values of 0-3) indicating the intended data type of each column of data.x. Mode numeric (0) will be considered as interval scaled variables; mode character or class factor (1) will be considered as categorical nominal variables; class ordered (2) will be considered as categorical ordinal variables; mode logical (3) will be considered as binary asymmetric variables (view details of gower.dist() for more info).
#' @param var.weight.vec (Optional) p x 1 vector with elements indicating variable weights applied in Gower's Distance calculations. User can specify different weights for different variables by providing a numeric value for each variable contributing to the distance. Length should equal the number of variables considered in calculating distance. Entered weights are scaled to sum up to 1.
#' @param dend (Optional) boolean input, "TRUE" indicating dendrogram plot is a desired output, "FALSE" otherwise.
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
Gower_Cluster <- function(data.x, var.type.vec, var.weight.vec = NULL, dend = FALSE){

  X <- data.x

  # Adversarial Checks:
  ###
  # Check 1: X is object type matrix/df
  Gower_Mat_Check(X)
  # Check 2: var.type.vec compatibility checks
  Var_Type_Check(var.type.vec)
  # Check 3: var.weight.vec compatibility checks
  Var_Weight_Check(var.weight.vec)
  # Check 4: dend is logical type object
  Dend_Check(dend)

  # Var Type Vec Implementation:
  ###
  # Apply var.type.vec to adjust feature types





  ###
  return(gower.mat)
}






