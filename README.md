### gower.cluster

### An Implementation of Gower Distance in Exploratory Cluster Analysis

#### Intended Use: 

Intended use of this package is to provide relatively simple operations to prepare data frame objects of mixed variable data for Gower Distance computations, output pairwise Gower Distances between observations, and provide certain exploratory cluster analysis tools to explore potential clusterings of the mixed variable data. The user can directly use the main `Gower_Cluster` function, and must first input mixed variable data in the form of a data frame or, if all numeric data, a matrix is also an option. Other inputs/options: `var.type.vec` is a required input and vector with elements indicating the variable type of each feature column in the data frame/matrix object (see `gower.cluster` vignette for details); `var.weight.vec` (optional) is a vector with elements indicating feature weights applied in Gower Distance calculations, in case the user prefers varying levels of "contribution" from the features in computing Gower Distances (see vignette for details); `cluster.vis` (optional) is a boolean input, "TRUE" indicating dendrogram plot is a desired output, "FALSE" otherwise; `method`, `silhouette.kmin`, and `silhouette.kmax` are all required if and only if `cluster.vis` is specified as true, and contribute to the hierarchical clustering outputs (see vignette for details); lastly, `ordered.cat.levels.vec` is required if at least one ordered categorical feature is specified in `var.type.vec`, and is a list object containing lists of ordered levels corresponding to each specified ordered categorical feature (see vignette for details). 

It should be noted that the output of Gower_Cluster is a list of objects (pairwise Gower Distance matrix and, optionally, the clustering tools). For more information on these outputs and their purpose, refer to the vignette included in the `gower.cluster` package. For more information on Gower Distance and its use cases, refer to the vignette and the references/links included in the vignette.

#### Installation Instructions: 
The `gower.cluster` R package can be installed from GitHub directly using the following code: `devtools::install_github("aajb99/gower.cluster", build_vignettes = TRUE)`. Note: `build_vignettes = TRUE` must be specified here in order to request the vignette. To access the vignette after installing the package, you simply run `vignette("gower_implementation_cluster_analysis", package = "gower.cluster")` In addition, the following is the full link to the source code: https://github.com/aajb99/gower.cluster/tree/main. 

#### Small Example:

To get started, the following example is provided using simulated data:

```
###############
# Set up data #
###############

# Construct data features of differing types
# Logical
f_logic <- sample(c(0, 1), size = 80, replace = TRUE, prob = c(.5, .5))
# Numeric
f_numeric <- f_logic + sample(rnorm(80, sd = 0.5))
# Ordered categorical (i.e. ordinal)—assume 0 is low, 2 is high
f_cat_ordered <- ifelse(f_numeric < 0.15, 0, 
                        ifelse(f_numeric < 0.85, 1, 2))
# Unordered categorical (i.e. nominal)
f_cat_unordered <- sample(c(0, 1, 2, 3), size = 80, replace = TRUE, prob = c(.25, .25, .25, .25))

# Construct dataframe
example_df <- as.data.frame(cbind(f_logic, f_numeric, f_cat_ordered, f_cat_unordered))

#######################
# Apply Gower_Cluster #
#######################

# Apply Function
gower_cluster_output <- Gower_Cluster(data.x = example_df, var.type.vec = c(3, 0, 2, 1), var.weight.vec = c(10, 5, 5, 5), 
                                      cluster.vis = TRUE, method = 'average', silhouette.kmin = 2, silhouette.kmax = 30, 
                                      ordered.cat.levels.vec = list(list(0, 1, 2)))

# Display dendrogram, silhouette plot, and optimal k value outputs:
gower_cluster_output$dend.plot()
gower_cluster_output$silhouette.plot()
gower_cluster_output$optimal.k

```

Further details regarding this specific example (how the inputs were chosen, what the outputs represent, etc.), the functionality of `Gower_Cluster`, and the general purpose of the `gower.cluster` package can be found in the vignette included in the package.


