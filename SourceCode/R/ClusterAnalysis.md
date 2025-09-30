# ClusterAnalysis.R Documentation

## Overview
Performs cluster analysis on PCA-transformed hyperspectral images to determine the optimal number of clusters using WSS and k-means.

## Main Functionality
- Loads raster images and optionally downsamples them.
- Prepares data for clustering and applies k-means.
- Returns the optimal number of clusters for further analysis.

## Dependencies
- R packages: terra, parallel

## Usage
Call the main function with image file path and clustering parameters to estimate optimal clusters.

---
See script for details and customization.