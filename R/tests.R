#############
### Tests ###
#############

library(vroom)

###

###################
# Gower Mat Check #
###################
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

















