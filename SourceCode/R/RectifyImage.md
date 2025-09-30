# RectifyImage.R Documentation

## Overview
Rectifies hyperspectral images using GDAL, optionally applying a cutline shapefile for spatial cropping.

## Main Functionality
- Locates raw hyperspectral image files and prepares output paths.
- Uses GDAL to rectify images, optionally with cutline shapefile.
- Exports rectified images for further analysis.

## Dependencies
- R packages: terra, BiodivMapR
- External: GDAL

## Usage
Call `rectify_Image()` with raw image folder, output folder, and cutfile options to rectify images for each test site.

---
See script for details and customization.