#' Gower_Mat_Check
#'
#' @param X n x p dataframe object (allows for mixed data types) or a matrix object (single data type)
#' @param var.type.vec p x 1 vector with elements (values of 0-3) indicating the intended data type of each column of data.x. Mode numeric (0) will be considered as interval scaled variables; mode character or class factor (1) will be considered as categorical nominal variables; class ordered (2) will be considered as ordered categorical variables; mode logical (3) will be considered as binary asymmetric variables (view details of gower.dist() for more info).
#'
#' @return Returns an error message if X is not a matrix or data frame
#'
#' @keywords internal
#'
Gower_Mat_Check <- function(X, var.type.vec){

  # Check object type
  if(!(is.matrix(X) | is.data.frame(X))){

    stop(paste("data.x input must be a matrix or data frame object. Readjust."))

  }
  # If matrix, check if var.type.vec is all 0's. If not, return error
  if(is.matrix(X)) {

    if(!all(var.type.vec %in% c(0))){

      stop(paste("data.x input is a matrix object, but not all features are specified as type 0 in var.type.vec. Readjust."))

    }

  }

}

#' Var_Type_Check
#'
#' @param X n x p dataframe object (allows for mixed data types) or a matrix object (single data type)
#' @param var.type.vec p x 1 vector with elements (values of 0-3) indicating the intended data type of each column of data.x. Mode numeric (0) will be considered as interval scaled variables; mode character or class factor (1) will be considered as categorical nominal variables; class ordered (2) will be considered as ordered categorical variables; mode logical (3) will be considered as binary asymmetric variables (view details of gower.dist() for more info).
#'
#' @return Returns an error message if var.type.vec does not follow one or more of the requirements mentioned below.
#'
#' @keywords internal
#'
Var_Type_Check <- function(X, var.type.vec){

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
#' @param X n x p dataframe object (allows for mixed data types) or a matrix object (single data type)
#' @param var.type.vec p x 1 vector with elements (values of 0-3) indicating the intended data type of each column of data.x. Mode numeric (0) will be considered as interval scaled variables; mode character or class factor (1) will be considered as categorical nominal variables; class ordered (2) will be considered as ordered categorical variables; mode logical (3) will be considered as binary asymmetric variables (view details of gower.dist() for more info).
#'
#' @return Returns an error message if var.weight.vec does not follow one or more of the requirements mentioned below.
#'
#' @keywords internal
#'
Var_Weight_Check <- function(X, var.weight.vec){

  if(!is.null(var.weight.vec)){

    # Check object type
    if(!is.vector(var.weight.vec)){

      stop(paste("var.weight.vec input must be a vector object. Readjust."))

    }

    # Check Dimensions compatible with X
    if(length(var.weight.vec) != ncol(X)){

      stop(paste("Length of var.weight.vec vector does not match the number of columns of data.x. Check and readjust."))

    }

    # Check is numeric vector
    if(!all(is.numeric(var.weight.vec))){

      stop(paste("var.weight.vec vector must have all numeric inputs. Check and readjust."))

    }

  }

}

#' Cluster_Vis_Check
#'
#' @param cluster.vis
#'
#' @return Returns an error message if cluster.vis is not a logical type object.
#'
#' @keywords internal
#'
Cluster_Vis_Check <- function(cluster.vis){

  # Check object type
  if(!is.logical(cluster.vis)){

    stop(paste("cluster.vis input is not a logical type object. Readjust."))

  }

}

#' K_Range_Check
#'
#' @param silhouette_kmin
#' @param silhouette_kmax
#'
#' @return Returns an error message if silhouette_kmin and/or silhouette_kmax are incompatible.
#'
#' @keywords internal
#'
K_Range_Check <- function(silhouette_kmin, silhouette_kmax){

  # Check objects' data types
  if(!(is.numeric(silhouette_kmin) & is.numeric(silhouette_kmax))){

    stop(paste("Inputs silhouette_kmin and silhouette_kmax must both be numeric. Check and readjust."))

  }
  # Check values are integers
  if(!((silhouette_kmin %% 1 == 0) & (silhouette_kmax %% 1 == 0))) {

    stop(paste("Inputs silhouette_kmin and silhouette_kmax must both be integers. Check and readjust."))

  }
  # Check silhouette_kmin <= silhouette_kmax
  if(!(silhouette_kmin <= silhouette_kmax)) {

    stop(paste("Input silhouette_kmin must be less than or equal to silhouette_kmax. Check and readjust."))

  }
}

#' Method_Check
#'
#' @param method
#'
#' @return Returns an error message if method is incompatible.
#'
#' @keywords internal
#'
Method_Check <- function(method){

  # Check object data type
  if(!is.character(method)){

    stop(paste("method input must be a string (see Gower_Cluster details). Check and readjust."))

  }

  # Check object entry
  if(!(method == "ward.D" | method == "ward.D2" | method == "single" |
       method == "complete" | method == "average" | method == "mcquitty" |
       method == "median" | method == "centroid")){

    stop(paste("method input must match one of the possible agglomerative methods (see Gower_Cluster details). Check and readjust."))

  }

}





