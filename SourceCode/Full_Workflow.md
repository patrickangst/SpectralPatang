# Full_Workflow.R Script Documentation

## Overview
`Full_Workflow.R` is the main workflow script for the SpectralPatang project, orchestrating the processing and analysis of hyperspectral ecological data. It automates a multi-step pipeline for masking, rectification, principal component analysis (PCA), cluster analysis, and diversity mapping across multiple test sites.

## Author
- Patrick Byron Angst

## Purpose
This script executes a reproducible workflow for spectral and biodiversity analysis using R and external tools. It processes hyperspectral images, generates principal components, selects relevant components, performs clustering, and calculates alpha/beta diversity metrics for ecological research.

## Main Steps
1. **Masking, Rectification, PCA**
   - Prepares hyperspectral images by masking and rectifying them.
   - Performs PCA to reduce dimensionality and extract key spectral features.
2. **Principal Component Plotting**
   - Generates PNG visualizations for each principal component band.
3. **PC Selection Processing**
   - Selects relevant principal components based on metrics and saves them for further analysis.
4. **Cluster Analysis**
   - Determines the optimal number of clusters for spectral species mapping using KMeans.
   - Updates metrics files with cluster information.
5. **Diversity Calculation**
   - Maps spectral species, alpha diversity (Shannon/Simpson), and beta diversity (Bray-Curtis) for each test site.

## Key Functions
- `part_one()`: Masking, rectification, and PCA for each test site.
- `part_two()`: Visualization of principal component bands.
- `part_three()`: Selection and export of principal components.
- `part_four()`: Cluster analysis and optimal cluster number determination.
- `part_five()`: Spectral species mapping and diversity calculations.

## Dependencies
- R packages: SpectralPatang, biodivMapR, doParallel, terra, ggplot2, reshape2, gridExtra, RColorBrewer, readxl, writexl, dplyr
- External tools: GDAL (for raster translation and nodata handling)

## Usage
- Set the `perform_part_X` flags to `TRUE` to execute specific workflow steps.
- The script will process all test sites in the specified data directory, skipping those already completed.
- Outputs are saved in organized subfolders for further analysis and visualization.

## License
MIT (or as specified in the script header)

---
For more details on each step or function, refer to the inline comments in `Full_Workflow.R` or contact the project maintainer.