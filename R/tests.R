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
# Adjust Features Funciton #
############################

v_type_test1 <- c(0, 1, 2, 3, 0, 1, 2, 3)
v_type_test2 <- c(0, 1, 0, 1)
mat_test1 <- matrix(c(.5, .3, .4, .9,
                      1, 3, 2, 5,
                      .4, .6, .2, .8,
                      7, 3, 5, 1),
                    nrow = 4, byrow = FALSE)
df_test1 <- as.data.frame(mat_test1)
# Test other feature types:
v_type_test2 <- c(0, 1, 2, 3)
mat_test1 <- matrix(c(.5, .3, .4, .9,
                      1, 3, 2, 5,
                      1, 3, 2, 3,
                      0, 1, 0, 1),
                    nrow = 4, byrow = FALSE)
df_test1 <- as.data.frame(mat_test1)

for (i in 0:3){

  indices <- which(v_type_test2 == i)

  if(length(indices) > 0){

    # Adjust features at indices based on i:
    if(i == 0){

      df_test1[, indices] <- unlist(lapply(df_test1[, indices], as.numeric))

    } else if(i == 1){

      df_test1[, indices] <- unlist(lapply(df_test1[, indices], as.factor))

    } else if(i == 2){

      df_test1[, indices] <- as.ordered(unlist(lapply(df_test1[, indices], function(x) factor(x, ordered = TRUE, levels = c(1, 2, 3)))))

    } else{

      df_test1[, indices] <- unlist(lapply(df_test1[, indices], as.logical))

    }

  }

}


class(df_test1$V2)
is.ordered(df_test1$V2)
is.ordered(df_test1$V2)









