# CreateSAVIMask.R Documentation

## Overview
Generates a SAVI (Soil Adjusted Vegetation Index) mask for rectified hyperspectral images to aid in vegetation analysis.

## Main Functionality
- Locates rectified image files and reads raster data.
- Calculates SAVI index and applies thresholding.
- Exports mask raster for further analysis.

## Dependencies
- R packages: terra

## Usage
Call `create_SAVI_mask()` with rectified image folder and output path to generate SAVI masks for each dataset.

---
See script for details and customization.