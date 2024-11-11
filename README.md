### Package: gower.cluster

### Title: An Implementation of Gower Distance in Exploratory Cluster Analysis

#### Intended Use: 
Intended use of this package is to provide relatively simple operations to prepare data frame objects of mixed variable data for Gower Distance computations, output pairwise Gower Distances between observations, and provide certain exploratory cluster analysis tools to explore potential clusterings of the mixed variable type data. The user must first input mixed variable data in the form of a data frame or, if all numeric data, a matrix is also an option. Other inputs/options: var.type.vec is a vector with elements indicating each variable type in the data frame/matrix object; var.weight.vec (optional) is a vector with elements indicating variable weights applied in Gower Distance calculations, in case the user prefers varying levels of "contribution" from the variables in computing Gower Distances (see details of gower.dist() from gower package for more information); dend (optional) is a boolean input, "TRUE" indicating dendrogram plot is a desired output, "FALSE" otherwise. For more information on Gower Distance and its use cases, refer to details of gower.dist() from gower package; for more information regarding the inputs previously mentioned, refer to details of Gower_Cluster() from this package (currently found in R/gower_dist_mat.R).

#### Installation Instructions: 
The gower.cluster R package can be installed from GitHub directly using the following code: `devtools::install_github("aajb99/gower.cluster")`.
The (public) repository can be found here: https://github.com/aajb99/gower.cluster/tree/main.

#### Remaining Parts of the Project:
The parts of my project left for the remainder of the semester include the following: most components of the function to adjust features of the data frame input object (in adjust_features.R) have been written and tested, yet I still have to determine how to adjust for features of ordered factor type specifically. Also, the function to compute the Gower
Distance matrix output as well as the function to configure the dendrogram based on Gower Distances need to be written, tested, and added to gower_dist_mat.R. Lastly, if time permits,
I need to implement another optional exploratory cluster analysis tool for the user.






