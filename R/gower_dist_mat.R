#' Gower_Cluster
#'
#' @param data.x n x p dataframe object (allows for mixed data types) or a matrix object (single data type).
#' @param var.type.vec p x 1 vector with elements (values of 0-3) indicating the intended data type of each column of data.x. Mode numeric (0) will be considered as interval scaled variables; mode character or class factor (1) will be considered as categorical nominal variables; class ordered (2) will be considered as ordered categorical variables; mode logical (3) will be considered as binary asymmetric variables (view details of gower.dist() for more info).
#' @param var.weight.vec (Optional) p x 1 vector with elements indicating variable weights applied in Gower's Distance calculations. User can specify different weights for different variables by providing a numeric value for each variable contributing to the distance. Length should equal the number of variables considered in calculating distance. Entered weights are scaled to sum up to 1.
#' @param dend (Optional) boolean input, "TRUE" indicating dendrogram plot is a desired output, "FALSE" otherwise.
#' @param ordered.cat.levels.vec (Optional) vector is required if columns of type 2 (ordered categorical variables) are specified. Say there are e variables of type 2 specified in var.type.vec and i=1,...,e. The ith element of ordered.cat.levels.vec is a list of "ordered levels" corresponding to the ith ordered categorical variable (i.e. the number of elements in ordered.cat.levels.vec should match the number of ordered categorical variables specified in var.type.vec). This is necessary for setting features to this feature type.
#'
#' @return A matrix
#' \item{gower.mat}{ n x n matrix representing the pairwise Gower's Distances between observations of data.x }
#' \item{dend.plot}{ (Optional) dendgrogram plot indicating potential clusters of data.x observations based on pairwise Gower's Distances }
#' @export
#'
#' @examples
#'
#' ##### Example 1
#' (in progress)
#'
#' #####################
#' ##### Example 2
#' (in progress)
#'
Gower_Cluster <- function(data.x, var.type.vec, var.weight.vec = NULL, dend = FALSE, ordered.cat.levels.vec = NULL){

  # Rename data.x object
  X <- data.x

  # Adversarial Checks:
  ###
  # Check 1: X is object type matrix/df
  Gower_Mat_Check(X, var.type.vec)
  # Check 2: var.type.vec compatibility checks
  Var_Type_Check(X, var.type.vec)
  # Check 3: var.weight.vec compatibility checks
  Var_Weight_Check(X, var.weight.vec)
  # Check 4: dend is logical type object
  Dend_Check(dend)

  # Var Type Vec Implementation:
  ###
  # Apply var.type.vec/ordered.cat.levels.vec to adjust feature types:
  X <- Adjust_Feature_Type(X, var.type.vec, ordered.cat.levels.vec)

  # Configure Gower Dist Mat:
  ###
  # Apply gower.dist from StatMatch package to X (var.weights included if specified by the user)
  gower.mat <- StatMatch::gower.dist(X, var.weights = var.weight.vec)



  ###
  return(gower.mat)
  # return(gower.mat)
}










