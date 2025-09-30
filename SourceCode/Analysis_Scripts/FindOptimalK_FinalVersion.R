# Clean environment
rm(list = ls(all = TRUE))
gc()
graphics.off()

# Load required libraries
library(terra)
library(tools)
library(ggplot2)

# Parameters
hs_folder <- "hs/"  # Folder containing .tif files
min_clusters <- 2
max_clusters <- 50
nstart_values <- c(1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50)  # Range of nstart values to test
set.seed(123)

# Create output folder if it doesn't exist
if (!dir.exists("kmeans_analysis")) {
  dir.create("kmeans_analysis")
# ============================================================================
# Project: SpectralPatang
# Script: FindOptimalK_FinalVersion.R
# Description: Determines the optimal number of clusters (K) for k-means clustering on hyperspectral data using WSS and multiple nstart values.
# Author: Patrick Angst
# Date: 2025-09-30
# Dependencies: terra, tools, ggplot2
# ============================================================================
}

# Get list of all .tif files in "hs/"
tif_files <- list.files(hs_folder, pattern = "\\.tif$", full.names = TRUE)

# Function to calculate WSS for a given number of clusters and nstart
calculate_wss <- function(data, k, nstart) {
  kmeans_result <- kmeans(data, centers = k, nstart = nstart)
  return(kmeans_result$tot.withinss)
}

# Iterate over each .tif file
for (hyperspectral_path in tif_files) {
  
  cat("\nProcessing file:", hyperspectral_path, "\n")
  
  # Load the PCA GeoTIFF
  pca_data <- rast(hyperspectral_path)
  
  # Convert to 2D matrix (pixels x principal components)
  pca_matrix <- as.matrix(terra::values(pca_data))
  
  # Track NA locations before removing missing values
  na_rows <- apply(pca_matrix, 1, function(x) any(is.na(x)))
  
  # Remove rows with NA for clustering
  pca_matrix_clean <- na.omit(pca_matrix)
  
  if (nrow(pca_matrix_clean) < max_clusters) {
    cat("Skipping file due to insufficient data points.\n")
    next
  }
  
  # Scale the cleaned matrix
  scaled_pca_matrix <- scale(pca_matrix_clean)
  
  # Find the best nstart value
  best_nstart <- nstart_values[1]
  lowest_wss <- Inf
  
  cat("Evaluating different nstart values...\n")
  for (nstart in nstart_values) {
    wss_test <- calculate_wss(scaled_pca_matrix, k = min_clusters, nstart = nstart)
    if (wss_test < lowest_wss) {
      lowest_wss <- wss_test
      best_nstart <- nstart
    }
  }
  
  cat("Optimal nstart for", hyperspectral_path, ":", best_nstart, "\n")
  
  # Evaluate WSS for different numbers of clusters using the best nstart
  potential_k_values <- min_clusters:max_clusters
  wss_values <- numeric(length(potential_k_values))
  
  cat("Evaluating WSS for different numbers of clusters...\n")
  
  for (i in seq_along(potential_k_values)) {
    k <- potential_k_values[i]
    wss_values[i] <- calculate_wss(scaled_pca_matrix, k, best_nstart)
  }
  
  # Create a data frame for plotting WSS
  wss_df_optimal_k <- data.frame(clusters = potential_k_values, WSS = wss_values)
  
  # Determine Optimal Clusters - Elbow Method (Second Derivative)
  diff_wss <- diff(wss_df_optimal_k$WSS)
  diff2_wss <- diff(diff_wss)
  
  optimal_k_elbow <- min_clusters
  if (length(diff2_wss) > 0) {
    optimal_k_elbow_index <- which.min(diff2_wss) + 1
    optimal_k_elbow <- potential_k_values[optimal_k_elbow_index]
  } else {
    cat("Not enough k values to calculate the second derivative. Suggesting minimum clusters.\n")
  }
  
  cat("Optimal k for", hyperspectral_path, ":", optimal_k_elbow, "\n")
  
  # Perform final k-means clustering
  final_kmeans_result <- kmeans(scaled_pca_matrix, centers = optimal_k_elbow, nstart = best_nstart)
  cluster_assignments <- final_kmeans_result$cluster
  
  # Restore original shape: create a full vector with NA where necessary
  full_clusters <- rep(NA_integer_, nrow(pca_matrix)) # Initialize with NA
  full_clusters[!na_rows] <- cluster_assignments  # Fill in only non-NA rows
  
  # Reshape back into raster
  cluster_raster <- rast(pca_data, nlyr = 1)
  terra::values(cluster_raster) <- full_clusters
  
  # Save final clustered raster
  output_filename_cluster <- file.path("kmeans_analysis", paste0(file_path_sans_ext(basename(hyperspectral_path)), "_kmeans_clusters_k_", optimal_k_elbow, ".tif"))
  writeRaster(cluster_raster, filename = output_filename_cluster, overwrite = TRUE)
  cat(paste0("K-means cluster raster (k=", optimal_k_elbow, ") saved to: ", output_filename_cluster, "\n"))
  
}

cat("\n Batch k-means analysis complete.\n")
