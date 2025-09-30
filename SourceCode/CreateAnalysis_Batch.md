# CreateAnalysis_Batch.R Documentation

## Overview
Batch processes subfolders containing hyperspectral data for analysis. Automates rectification and masking steps for each subfolder.

## Main Functionality
- Processes each subfolder to locate rectified images and masks.
- Checks for files without extensions (raw ENVI format).
- Prepares data for further analysis.

## Dependencies
- R packages: doParallel, foreach, SpectralPatang

## Usage
Call the main function to process all subfolders in parallel. Custom logic can be added to `process_subfolder()` for specific analysis steps.

---
For details, see inline comments in the script.