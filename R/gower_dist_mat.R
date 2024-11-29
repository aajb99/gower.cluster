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
  # Check 2: var.type.vec compatibility chedata:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAYAAABzenr0AAADG0lEQVR42u2XW0hTYRzA99jt0XOOmHi+s4f6vrPoJaigiOhG2bCSJHJQL711oXuZ1JQuRhZlaUpGF7M2B5q2tLW5i6YQQRcMIixQAqUIJKFcq4fT9x+dcXa2s52zNZ/6w5+x/7bz/b7/fSaTSvLMeB7HW1ZxReJq1kzWsAJZywp4HYMs6xmeFOfxZCNnxlZOEEtYRDazPC5lBMtWRhDLOES2cQhvzxfExQihGaZMBA6XshBXR5cUejbQQWGWZgQBN88WIBwOS4G+gU62aP6ywsLCmYYAwO3ZAoAAhM8fcjMCWWEIAuL9LwBkCK8/1J2P8MqCgkWzdAHwC5Zv6ep/L51o8kvlVe1SyVFH9BXegz0c+a0bQIbw+AJPwLNpITYdc5RbD7dOwKFauvP0Q6nv9ahuABmi2+P1gnc1IejD7akOVqvD91Y3gAzh7vH0Mry4geMWzk64ufLhHz99jdOh4XHJ5X0j7ahuj4MIvhrRDSBDPOrxBKGPxA4vs7vm0Id9Vj4YZOrnrxjAxOSPqG3sy7c4AFtVh/R9KpIAUHOxLq0qXO/co3YvCBystIEXQCobnsbZHw8OG64URsBnlbH36gEIvPgQtdfcDsbZTzUHDQPQFn5eCTCWDADc3t3/LqrPh0ZjtsSq6DQOwONaJUAkGYBaIARq94OWVrQZBxDES0qA8VQhkG9f7xzU6AvGPcAgckV3DuytdUcrAtyvLkPQk80BwwAcj68aqgLIAxBIRPV3M6kCumPUp+0DSgC4udwLlHmQrA/orILrqjbstCkBoNTUCQehUNsDL0cympgswk05mwX6ypDc0BhITpv1YMtkummYbAYY64TkpuZInkuW7Ie5X9HYm9E+oKsKEL6lCUA3391SjoUm4R1tACTuyzkAT1q0t2JEDkwDwD3tpZTHh3IfAvF+iq2YHJmGHHiQIgfw8VwD0GHkTPHXjFTmGoDmWVuqMiymrdJO41RNG8YZDonnYIOhiXMB5jilv0zruI7jxWs0XA30s0ZordDdoMFAjUOZUb0L2U5X8VZwOf2dA24Oh3MC2WX6L3/lDxSyAwzwmsreAAAAAElFTkSuQmCCcks
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

  # Configure Dendrogram Object (Optional):
  ###
  if (dend == TRUE){

    # Initialize Hierarchical Clustering Object:
    hclust_obj <- hclust(as.dist(gower.mat), method = hclust_method) ### NEED TO CREATE hclust_method input
    hclust_obj_size <- length(hclust_obj$order)
    # Initialize Dendrogram Object:
    #   Conditionals on size of hclust_obj to adjust dendrogram label font size:
    if (hclust_obj_size <= 200){

      dend_obj <- as.dendrogram(hclust_obj) |>
        dendextend::set('labels_cex', 1.2) # Alter label sizes

    } else {

      dend_obj <- as.dendrogram(hclust_obj) |>
        dendextend::set('labels_cex', 10 / sqrt(hclust_obj_size)) # Alter label sizes based on hclust_obj_size

    }

    # Create dendrogram plot output
    par(mar = c(4, 3, 3, 2)) # Adjusted margins
    dend_plot <- plot(dend_obj)

  }

  ###
  return(gower.mat)
  # return(gower.mat)
}










