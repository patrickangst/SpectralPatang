# Workspace Folder Structure Documentation

This document describes the main folders and files in the SpectralPatang workspace.

## Root Directory
- `README.md`: Project overview and instructions.
- `QGIS/`: Contains GIS-related files, including connections, project files, shapefiles, and features.
    - `ArcGIS_Connections.xml`: ArcGIS connection settings.
    - `MasterThesisSpectralPatang.qgz`: QGIS project file.
    - `search_query.qqf`: QGIS search query file.
    - `features/`: Zipped shapefiles and feature data.
    - `flight_strips/`: Contains flight boundary data and related geojson/pdf files.
- `SourceCode/`: Main R source code and scripts for analysis.
    - R scripts for data analysis, clustering, masking, and correlation.
    - `Analysis_Scripts/`: Additional R scripts for correlation testing and data extraction.
    - `data/`: Data files for analysis.
    - `KMeans/`: Scripts for KMeans clustering and evaluation methods.
    - `man/`: R documentation files.
    - `new_folder_structure/`, `R/`, `SpeciesAnalysis/`, `vignettes/`: Additional folders for organization and documentation.

## Notable Files


This structure supports GIS data processing, spectral analysis, and clustering workflows, primarily using R and QGIS tools. For more details on specific scripts or data, refer to the README or individual folder documentation.
## Notable Files
 - `DESCRIPTION`, `NAMESPACE`, `SpectralPatang.Rproj`: R project configuration files.
 - `vert.txt`: Likely a data or configuration file.

---

## Data Folder Details

### `SourceCode/data/MasterThesis/`
This is the main data directory for the project, containing results, intermediate files, and raw data for spectral and biodiversity analysis. Key subfolders include:

- **01_principle_components/**: GeoTIFF files of principal component analysis (PCA) results for each test site.
- **02_principle_components_png/**: PNG visualizations of PCA results, organized by test site.
- **03_Spectral_Species/**: GeoTIFF files representing spectral species maps for each test site.
- **04_RGB/**: RGB composite images (rectified and raw) for each test site, in GeoTIFF format.
- **05_alpha_diversity/**: GeoTIFF files of Shannon alpha diversity indices for each test site.
- **06_beta_diversity/**: GeoTIFF files of beta diversity (Bray-Curtis dissimilarity, PCO) for each test site.
- **07_Testsite_Metrics/**: Excel files with cluster assignments, metrics, and diversity at the plot level.
- **08_principle_components_selection/**: GeoTIFF files of selected principal components for each test site.
- **10_Map_Visualization/**: PNG images visualizing spectral species for each test site.
- **11_PlotClusters/**: Excel files summarizing cluster analysis, raster stats, and habitat information.
- **LRM_Plots/**: (Empty or contains plot data for LRM analysis.)
- **RawDataClustering/**: GeoTIFF files of KMeans clustering results on raw data for each test site.
- **RawDataClusteringMasked/**: GeoTIFF files of KMeans clustering results on masked raw data.
- **all_testsite_signatures/**: PNG plots of spectral signatures by habitat type and mean values for all test sites.
- **cluster_info_shp/**: Shapefiles containing cluster information for each test site and filtered metrics data.
- **final_hs_data_folder/**: Contains folders for each test site with final hyperspectral data.
- **ground_data/**: Ground truth and metadata, including Excel files for plot mapping, species lists, and habitat types.
- **plotid_signatures/**: PNG plots of spectral signatures for individual plot IDs across test sites.
- **spectral_signature/**: PNG plots of mean and individual spectral signatures by habitat and plant community for each test site.
- **ss_count_plot/**: PNG plots of spectral species counts by cluster and habitat (biodivMapR method).
- **ss_count_plot_custom/**: PNG plots of spectral species counts by cluster and habitat (custom method).

Each subfolder is organized by test site (e.g., AN_TJ_1, BRW_PW_1, etc.), and files are named to indicate the data type and site. The data supports spatial, spectral, and biodiversity analyses for ecological research.

For more details on file formats and usage, see the README or contact the project maintainer.