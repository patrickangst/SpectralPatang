# FindOptimalK_FinalVersion.R Documentation

## Overview
Automates k-means clustering on PCA-transformed hyperspectral images, using the Elbow Method to determine the optimal number of clusters for each image in a batch.

## Main Functionality
- Loads PCA raster images and cleans/scales data.
- Evaluates nstart values and WSS for cluster stability.
- Applies Elbow Method to select optimal cluster number.
- Outputs clustered rasters to a dedicated folder.

## Dependencies
- R packages: terra, tools, ggplot2

## Usage
Set input folder and clustering parameters, then run the script to batch process clustering and output results for all images.

---
See script for details and customization.