#' Adjust_Feature_Type
#'
#' @param X n x p dataframe object (allows for mixed data types) or a matrix object (single data type)
#' @param var.type.vec p x 1 vector with elements (values of 0-3) indicating the intended data type of each column of data.x. Mode numeric (0) will be considered as interval scaled variables; mode character or class factor (1) will be considered as categorical nominal variables; class ordered (2) will be considered as ordered categorical variables; mode logical (3) will be considered as binary asymmetric variables (view details of gower.dist() for more info).
#' @param ordered.cat.levels.vec (Optional) vector is required if columns of type 2 (ordered categorical variables) are specified. Say there are e variables of type 2 specified in var.type.vec and i=1,...,e. The ith element of ordered.cat.levels.vec is a list of "ordered levels" corresponding to the ith ordered categorical variable (i.e. the number of elements in ordered.cat.levels.vec should match the number of ordered categorical variables specified in var.type.vec). This is necessary for setting features to this feature type.
#'
#' @return X
#'
#' @keywords internal
#'
Adjust_Feature_Type <- function(X, var.type.vec, ordered.cat.levels.vec){

  # Extra measure to ensure columns are vector types (critical in variable type adjustment)
  X <- as.data.frame(X)

  # Iterate through var.type.vec entries and adjust column features accordingly:
  for (i in 0:3){

    indices <- which(var.type.vec == i)

    if(length(indices) > 0){

      # Adjust features at indices based on i:
      # If indices correspond to numeric features:
      if(i == 0){

        # Assign to numeric feature type
        for (index in indices){

          X[, index] <- as.numeric(X[, index])

        }

      }
      # If indices correspond to unordered (default) cat features:
      else if(i == 1){

        # Assign to cat feature type
        for (index in indices){

          X[, index] <- as.factor(X[, index])

        }

      }
      # If indices correspond to ordered cat features:
      else if(i == 2){

        # CHECK: dimension of ordered.cat.levels.vec matches dimension of indices, also indices exist in data frame object
        if (!(length(indices) == length(ordered.cat.levels.vec))){

          stop(paste("Dimension of ordered.cat.levels.vec and number of ordered categorical features specified in var.type.vec are unequal. Check and readjust."))

        }

        # Iterate through user-specified level orderings
        for (j in 1:length(indices)){

          # Assign to ordered cat feature type
          X[, indices[j]] <- factor(X[, indices[j]], ordered = TRUE, levels = ordered.cat.levels.vec[[j]])

        }

      }
      # If indices correspond to logical features:
      else if(i == 3){

        for (index in indices){

          # CHECK: Do columns have multiple levels? (If so, give warning)
          col_levels <- unique(X[, index])
          if (!(length(col_levels) == 1 | length(col_levels) == 2)) {

            warning('Feature assigned to logical category does not have 1 or 2 levels. Labeling will be applied inconsistently across levels.')

          }

          # Assign to logical feature type
          X[, index] <- as.logical(X[, index])

        }

      }

    }

  }

  return(X)

}
