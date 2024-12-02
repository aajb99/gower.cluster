### gower.cluster

### An Implementation of Gower Distance in Exploratory Cluster Analysis

#### Intended Use: 
Intended use of this package is to provide relatively simple operations to prepare data frame objects of mixed variable data for Gower Distance computations, output pairwise Gower Distances between observations, and provide certain exploratory cluster analysis tools to explore potential clusterings of the mixed variable data. The user can directly use the main Gower_Cluster function (gower_dist_mat.R), and must first input mixed variable data in the form of a data frame or, if all numeric data, a matrix is also an option. Other inputs/options: var.type.vec is a vector with elements indicating the variable type of each feature column in the data frame/matrix object; var.weight.vec (optional) is a vector with elements indicating feature weights applied in Gower Distance calculations, in case the user prefers varying levels of "contribution" from the features in computing Gower Distances (see details of gower.dist from StatMatch package for more information); dend (optional) is a boolean input, "TRUE" indicating dendrogram plot is a desired output, "FALSE" otherwise. It should be noted that the output of Gower_Cluster is then designed to be a list of objects (pairwise Gower Distance matrix and, optionally, the exploratory plots). For more information on Gower Distance and its use cases, refer to details of gower.dist from StatMatch package; for more information regarding the inputs previously mentioned, refer to details of Gower_Cluster from this package (currently found in R/gower_dist_mat.R).

#### Installation Instructions: 
The gower.cluster R package can be installed from GitHub directly using the following code: `devtools::install_github("aajb99/gower.cluster")`. The following is the full link to the (public) repository: https://github.com/aajb99/gower.cluster/tree/main. 







