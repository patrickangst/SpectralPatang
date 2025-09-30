# ############################################################
# Script Name:    ClusterAnalysis.R
# Author:         Patrick Byron Angst
# Date Created:   2025-09-30
# Last Modified:  2025-09-30
# Version:        1.0
# Description:    Performs cluster analysis on PCA-transformed hyperspectral images to determine the optimal number of clusters using WSS and k-means.
#
# Dependencies:   terra, parallel
# Input Files:    PCA-transformed hyperspectral images
# Output Files:   Optimal cluster number, cluster assignments
#
# License:        MIT
# ############################################################

#' Perform cluster analysis with wss
#'
#' This uses wss to get the optimal kmeans cluster number.
#' @param Image_File_Path character. Path of the selected PC image
#' @param Downsample boolean. Should the dataset be down sampled
#' @param Downsample_factor numeric. Factor to down sample (eg. 2 means 2x2 pixel)
#' @param Downsample_function character. Dwon sample function (sd, mean, range)
#' @param Min_Cluster numeric. Minimal amount of clusters to test.
#' @param Max_Cluster numeric. Maximal amount of clusters to test.
#'
#' @return Returns the number of optimal clusters
#' @export
#'

get_optimal_cluster_number <- function(Image_File_Path,
                                       Downsample = FALSE,
                                       Downsample_factor = 2,
                                       Downsample_function = "sd",
                                       Min_Cluster = 2,
                                       Max_Cluster = 50) {

  nstart_values <- c(1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50)
  set.seed(123)

  cat("\nProcessing file:", Image_File_Path, "\n")

  # Load the PCA GeoTIFF
  pca_data <- rast(Image_File_Path)

  # Optional downsampling
  if (Downsample) {
    fun <- match.fun(Downsample_function)
    pca_data <- aggregate(pca_data, fact = Downsample_factor, fun = fun)
  }

  # Convert raster to matrix
  pca_matrix <- as.matrix(terra::values(pca_data))

  # Track NA positions
  na_rows <- apply(pca_matrix, 1, function(x) any(is.na(x)))

  # Clean matrix
  pca_matrix_clean <- na.omit(pca_matrix)

  if (nrow(pca_matrix_clean) < Max_Cluster) {
    warning("Insufficient data points after removing NA. Skipping.")
    return(NULL)
  }

  # Scale the data
  scaled_pca_matrix <- scale(pca_matrix_clean)

  # # Find best nstart
  # best_nstart <- nstart_values[1]
  # lowest_wss <- Inf
  #
  # cat("Evaluating nstart values...\n")
  # for (nstart in nstart_values) {
  #   wss <- kmeans(scaled_pca_matrix, centers = Min_Cluster, nstart = nstart)$tot.withinss
  #   if (wss < lowest_wss) {
  #     lowest_wss <- wss
  #     best_nstart <- nstart
  #   }
  # }
  # cat("Best nstart:", best_nstart, "\n")

  # --> nstart is fix at 10 for biodivMapR
  best_nstart <- 10

  # Evaluate WSS across cluster range
  potential_k_values <- Min_Cluster:Max_Cluster
  wss_values <- sapply(potential_k_values, function(k) {
    kmeans(scaled_pca_matrix, centers = k, nstart = best_nstart)$tot.withinss
  })

  # Compute second derivative for elbow
  diff_wss <- diff(wss_values)
  diff2_wss <- diff(diff_wss)

  optimal_clusters_elbow <- Min_Cluster
  if (length(diff2_wss) > 0) {
    elbow_index <- which.min(diff2_wss) + 1
    optimal_clusters_elbow <- potential_k_values[elbow_index]
  } else {
    cat("Not enough points to compute elbow. Using Min_Cluster.\n")
  }

  cat("Optimal number of clusters (Elbow Method):", optimal_clusters_elbow, "\n")

  # Save to a text file
  output_folder_path <- dirname(Image_File_Path)
  output_file_path <- file.path(output_folder_path, 'optimal_number_of_clusters.txt')
  write(optimal_clusters_elbow, file = output_file_path)

  # return(optimal_clusters_elbow)
  return(list(
    optimal_clusters_elbow = optimal_clusters_elbow,
    best_nstart = best_nstart
  ))

}


#debug(get_optimal_cluster_number)
# path_name <- get_optimal_cluster_number(
#   '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/ang20190706t235120rfl/clustertest/result/ang20190706t235120_rfl_v2v2_img_rectified_clustertest/SPCA/PCA/OutputPCA_30_PCs_selection.tif'
# )
