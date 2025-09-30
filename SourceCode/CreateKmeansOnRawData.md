# CreateKmeansOnRawData.R Documentation

## Overview
Performs k-means clustering on raw hyperspectral data, including optimal cluster estimation and evaluation of clustering parameters.

## Main Functionality
- Estimates optimal number of clusters using WSS and nstart parameter.
- Applies k-means clustering to data matrices.
- Supports downsampling and evaluation across a range of cluster numbers.

## Dependencies
- R packages: terra, scico

## Usage
Call the main function to estimate clusters and perform k-means on input data. Adjust parameters for downsampling and cluster range as needed.

---
See script for details and additional options.