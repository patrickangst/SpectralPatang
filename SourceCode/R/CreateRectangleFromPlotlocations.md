# CreateRectangleFromPlotlocations.R Documentation

## Overview
Creates a rectangular shapefile (cutline) around plot locations for use in image rectification and spatial analysis.

## Main Functionality
- Reads plot location shapefiles and calculates bounding boxes.
- Applies buffer to bounding box and saves as new shapefile.
- Supports custom buffer distance and CRS assignment.

## Dependencies
- R packages: sf

## Usage
Call the main function with plot location shapefile and output folder to generate cutline shapefiles for spatial processing.

---
See script for details and customization.