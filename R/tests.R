#############
### Tests ###
#############

library(vroom)

###

###############################
# Adversarial Check Functions #
###############################

# Gower_Mat_Check: #
X_test1 <- matrix(c(1, 2, 3, 1, 3, 2), nrow = 3)
df_test1_pre <- head(vroom("./Data/r_gowers_pre_df.csv", delim = ","), 5)
df_test1_main <- cbind(df_test1_pre, string_col = c('a', 'b', 'b', 'b', 'a'))
X_bad1 <- 'bad'

# Matrix input
if(!(is.matrix(X_test1) | is.data.frame(X_test1))){
  print('error')
} else {
  print('passed')
}
# DF input
if(!(is.matrix(df_test1_main) | is.data.frame(df_test1_main))){
  print('error')
} else {
  print('passed')
}
# Bad input
if(!(is.matrix(X_bad1) | is.data.frame(X_bad1))){
  print('error')
} else {
  print('passed')
}


# Var Type Check #
ncol(df_test1_main)
# Vector elements check:
v_test1 <- as.vector(c(0, 1, 2, 3, 2, 4))
!all(v_test1 %in% c(0, 1, 2, 3))

# Var Weight Check #
v_weight_testbad <- as.vector(c(1, 2, 3, 4, 'a'))
v_weight_testgood <- as.vector(c(1, 2, 3, 4))
is.vector(v_weight_testbad)
all(is.numeric(v_weight_testbad))
is.vector(v_weight_testgood)
all(is.numeric(v_weight_testgood))

# Dend type check #
dend_test1 <- TRUE
!is.logical(dend_test1)

###

############################
# Adjust Features Function #
############################

# First round of feature types testing (inputs):
v_type_test1 <- c(0, 1, 0, 1)
mat_test1 <- matrix(c(.5, .3, .4, .9,
                      1, 3, 2, 5,
                      .4, .6, .2, .8,
                      7, 3, 5, 1),
                    nrow = 4, byrow = FALSE)
df_test1 <- as.data.frame(mat_test1)

# Test other feature types (inputs):
v_type_test2 <- c(0, 2, 2, 3, 2, 0, 0)
# v_type_test2 <- c(3, 3, 3, 3, 3)
mat_test1 <- matrix(c(.5, .3, .4, .9,
                      1, 3, 2, 5,
                      1, 3, 2, 3,
                      0, 1, 0, 1),
                    nrow = 4, byrow = FALSE)
df_test1 <- as.data.frame(mat_test1)
V5 <- c('low', 'medium', 'high', 'medium')
df_test1 <- cbind(df_test1, V5)
v_type_test2_order_level_list <- list(c(1, 2, 3, 5), c(1, 2, 3), c('low', 'medium', 'high'))

###

# Main loop in Adjust Features Function:
for (i in 0:3){

  indices <- which(v_type_test2 == i)

  if(length(indices) > 0){

    # Adjust features at indices based on i:
    # If indices correspond to numeric features:
    if(i == 0){

      # Assign to numeric feature type
      df_test1[, indices] <- sapply(df_test1[, indices], as.numeric)
      # df_test1[, indices] <- lapply(df_test1[, indices], function(x) as.vector(as.numeric(x)))
      # df_test1[, indices] <- unlist(lapply(df_test1[, indices], as.numeric))

    }
    # If indices correspond to unordered (default) cat features:
    else if(i == 1){

      # Assign to cat feature type
      df_test1[, indices] <- sapply(df_test1[, indices], as.factor)
      # df_test1[, indices] <- lapply(df_test1[, indices], function(x) as.vector(as.factor(x)))
      # df_test1[, indices] <- unlist(lapply(df_test1[, indices], as.factor))

    }
    # If indices correspond to ordered cat features:
    else if(i == 2){

      # CHECK: dimension of ordered.cat.levels.vec matches dimension of indices, also indices exist in data frame object
      if (!(length(indices) == length(v_type_test2_order_level_list))){

        stop(paste("Dimension of ordered.cat.levels.vec and number of ordered categorical features specified in var.type.vec are unequal. Check and readjust."))

      }

      # Iterate through user-specified level orderings
      for (j in 1:length(indices)){

        # Assign to ordered cat feature type
        df_test1[, indices[j]] <- factor(df_test1[, indices[j]], ordered = TRUE, levels = v_type_test2_order_level_list[[j]])

      }

      # Previous attempts to adjust features to ordered factor type
      # df_test1[, indices] <- as.ordered(unlist(lapply(df_test1[, indices], function(x) factor(x, ordered = TRUE, levels = c(1, 2, 3)))))
      # df_test1[, indices[j]] <- as.ordered(unlist(lapply(df_test1[, indices[j]], function(x) factor(x, ordered = TRUE, levels = v_type_test2_order_level_list[j]))))

    }
    # If indices correspond to logical features:
    else{

      # Factorize subset for checks
      factorized_subset <- sapply(df_test1[, indices], function(col) if(is.numeric(col)) as.factor(col) else NA)
      # CHECK: Do columns have multiple levels (if so, give warning)
      sapply(factorized_subset, function(col) if(!(nlevels(col) == 1 | nlevels(col) == 2)) warning('Certain features assigned to logical category do not have 1 or 2 levels. Labeling will be applied inconsistently across levels.'))

      # Assign to logical feature type
      df_test1[, indices] <- sapply(df_test1[, indices], as.logical)
      # df_test1[, indices] <- lapply(df_test1[, indices], function(x) as.vector(as.logical(x)))
      # df_test1[, indices] <- unlist(lapply(df_test1[, indices], as.logical))

    }

  }

}



