#' Gower_Cluster
#'
#' @param data.x n x p dataframe object (allows for mixed data types) or a matrix object (single data type).
#' @param var.type.vec p x 1 vector with elements (values of 0-3) indicating the intended data type of each column of data.x. Mode numeric (0) will be considered as interval scaled variables; mode character or class factor (1) will be considered as categorical nominal variables; class ordered (2) will be considered as ordered categorical variables; mode logical (3) will be considered as binary asymmetric variables (view details of gower.dist() for more info).
#' @param var.weight.vec (Optional) p x 1 vector with elements indicating variable weights applied in Gower Distance calculations. User can specify different weights for different variables by providing a numeric value for each variable contributing to the distance. Length should equal the number of variables considered in calculating distance. Entered weights are scaled to sum up to 1.
#' @param cluster.vis (Optional) boolean input, "TRUE" indicating dendrogram plot and silhouette plot are a desired output, "FALSE" otherwise.
#' @param method (Optional) the agglomerative method to be used (in conjunction with cluster.vis). This should be (an unambiguous abbreviation of) one of "ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", or "centroid".
#' @param silhouette.kmin (Optional) numeric input used in conjunction with cluster.vis. Indicates the minimum number of clusters k to be included in the silhouette plot.
#' @param silhouette.kmax (Optional) numeric input used in conjunction with cluster.vis. Indicates the maximum number of clusters k to be included in the silhouette plot.
#' @param ordered.cat.levels.vec (Optional) vector is required if columns of type 2 (ordered categorical variables) are specified. Say there are e variables of type 2 specified in var.type.vec and i=1,...,e. The ith element of ordered.cat.levels.vec is a list of "ordered levels" corresponding to the ith ordered categorical variable (i.e. the number of elements in ordered.cat.levels.vec should match the number of ordered categorical variables specified in var.type.vec). This is necessary for setting features to this feature type.
#'
#' @return A matrix
#' \item{gower.mat}{ n x n matrix representing the pairwise Gower Distances between observations of data.x }
#' \item{hclust.object}{ object of class hclust which describes the tree produced by the clustering process. See description of hclust (stats package) for details regarding object components }
#' \item{dend.plot}{ (Optional) dendgrogram plot indicating potential clusters of data.x observations based on pairwise Gower Distances }
#' \item{silhouette.plot}{ (Optional) silhouette plot to indicate optimal k values, or number of clusters, involving Gower Distance Matrix of data.x }
#' @export
#'
#' @examples
#'
#' ### Example 1
#' ## Simulate 100 observations with a logistic and a numeric feature (see below) and compare single and complete agglomeration methods in Gower Distance Hierarchical Clustering
#'
#' # Simulate data with dataframe
#' feature1 <- sample(c(0, 1), size = 100, replace = TRUE, prob = c(.5, .5))
#' feature2 <- feature1 + sample(rnorm(100))
#' df_sim <- as.data.frame(cbind(feature1, feature2))
#'
#' # Apply Gower_Cluster function using average and complete methods
#' out_avg <- Gower_Cluster(data.x = df_sim, var.type.vec = c(3, 0), cluster.vis = TRUE, method = 'average', silhouette.kmin = 2, silhouette.kmax = 10)
#' out_comp <- Gower_Cluster(data.x = df_sim, var.type.vec = c(3, 0), cluster.vis = TRUE, method = 'complete', silhouette.kmin = 2, silhouette.kmax = 10)
#'
#' # View cluster orderings
#' cat("Cluster Ordering (Avg Method): \n",
#'     out_avg$hclust.object$order,
#'     "\nCluster Ordering (Comp Method): \n",
#'     out_avg$hclust.object$order)
#'
#' # Compare dendrograms
#' out_avg$dend.plot()
#' out_comp$dend.plot()
#'
#' # Compare silhouette plots
#' out_avg$silhouette.plot()
#' out_comp$silhouette.plot()
#'
#'
#' ### Example 2
#' ## Apply Gower_Cluster to iris data. In addition, apply weights to features to enhance the contribution of Petal.Length, which is relatively more highly correlated with Species
#'
#' # Load iris data
#' data("iris")
#'
#' # Apply Gower_Cluster function using only numeric features, weighting Petal.Length twice as much relative to other features
#' out_iris <- Gower_Cluster(data.x = iris[, 1:4], var.type.vec = c(0, 0, 0, 0), var.weight.vec = c(5, 5, 10, 5), cluster.vis = TRUE, method = 'complete', silhouette.kmin = 2, silhouette.kmax = 10)
#'
#' # View cluster orderings, dendrogram, and silhouette plot
#' out_iris$hclust.object$order
#' out_iris$dend.plot()
#' out_iris$silhouette.plot()

#'
Gower_Cluster <- function(data.x, var.type.vec, var.weight.vec = NULL,
                          cluster.vis = FALSE, method = NULL, silhouette.kmin = NULL, silhouette.kmax = NULL, ordered.cat.levels.vec = NULL){

  # Rename data.x object
  X <- data.x

  ###

  # Adversarial Checks:
  ###
  # Check 1: X is object type matrix/df
  Gower_Mat_Check(X, var.type.vec)
  # Check 2: var.type.vec compatibility checks
  Var_Type_Check(X, var.type.vec)
  # Check 3: var.weight.vec compatibility checks
  Var_Weight_Check(X, var.weight.vec)
  # Check 4: cluster.vis is logical type object
  Cluster_Vis_Check(cluster.vis)

  ###

  # Var Type Vec Implementation:
  ###
  # Apply var.type.vec/ordered.cat.levels.vec to adjust feature types:
  X <- Adjust_Feature_Type(X, var.type.vec, ordered.cat.levels.vec)

  ###

  # Configure Gower Dist Mat:
  ###
  # Apply gower.dist from StatMatch package to X (var.weights included if specified by the user)
  gower.mat <- StatMatch::gower.dist(X, var.weights = var.weight.vec)

  ###

  # Configure Dendrogram/Silhouette Objects (Optional):
  ###
  if (cluster.vis == TRUE){

    ###
    # Adversarial checks on method:
    #   Check that method is specified correctly
    Method_Check(method)
    # Adversarial checks on cluster.vis/silhouette.kmin/silhouette.kmax:
    #   Check that silhouette.kmin and silhouette.kmax are specified correctly
    #   Also, check that silhouette.kmin <= silhouette.kmax
    K_Range_Check(silhouette.kmin, silhouette.kmax)

    ###

    # Dendrogram:

    # Initialize Hierarchical Clustering Object:
    hclust_obj <- hclust(as.dist(gower.mat), method = method)
    hclust_obj_size <- length(hclust_obj$order)
    # Initialize Dendrogram Object:
    #   Conditionals on size of hclust_obj to adjust dendrogram label size:
    if (hclust_obj_size <= 200){

      dend_obj <- as.dendrogram(hclust_obj) |>
        dendextend::set('labels_cex', 0.65) # Alter label sizes

    } else {

      # Add null labels to dend_obj:
      dend_obj <- as.dendrogram(hclust_obj) |>
        dendextend::set('labels_cex', 8 / sqrt(hclust_obj_size)) # Alter label sizes based on hclust_obj_size

    }

    # Silhouette Scores:
    silhouette_scores <- sapply(silhouette.kmin:silhouette.kmax, function(k) {
      cluster_assignments <- dendextend::cutree(hclust_obj, k = k)
      mean(cluster::silhouette(cluster_assignments, dmatrix = gower.mat)[, 3]) # grab silhouette score column, compute avg
    })

    # If cluster.vis outputs are included (TRUE):
    output <- list(
      gower.mat = gower.mat,
      hclust.object = hclust_obj,
      dend.plot = function() plot(dend_obj, main = "Dendrogram"),
      silhouette.plot = function() plot(silhouette.kmin:silhouette.kmax, silhouette_scores, type = "b", xlab = "k", main = "Silhouette Scores")
    )

    return(output)

  }

  ###

  # If cluster.vis outputs are excluded (FALSE):
  return(gower.mat)

}





