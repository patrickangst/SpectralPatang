# PerformKMeansClustering.R Documentation

## Overview
Performs k-means clustering on PCA-transformed hyperspectral images and outputs cluster assignments as raster files.

## Main Functionality
- Loads PCA raster and converts to matrix.
- Removes NA values and applies k-means clustering.
- Outputs clustered raster and optionally visualizes results.

## Dependencies
- R packages: terra, ggplot2, scico

## Usage
Call `run_kmeans_on_pca()` with PCA file path and desired number of clusters. Output is a clustered raster file.

---
See script for details and customization.