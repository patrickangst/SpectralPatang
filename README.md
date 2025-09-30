
# SpectralPatang

This repository contains the code and data for the SpectralPatang project, supporting GIS data processing, spectral analysis, and clustering workflows for ecological research.

## Folder Structure

See [FOLDER_STRUCTURE.md](FOLDER_STRUCTURE.md) for a detailed description of the workspace and data organization.

## Main Workflow

The main workflow is orchestrated by [`Full_Workflow.R`](SourceCode/Full_Workflow.R) ([documentation](SourceCode/Full_Workflow.md)), which automates masking, rectification, PCA, clustering, and diversity mapping across test sites.

## Key Scripts and Documentation

### SourceCode

- [`CreateAnalysis_Batch.R`](SourceCode/CreateAnalysis_Batch.R) ([doc](SourceCode/CreateAnalysis_Batch.md))
- [`CreateKmeansOnRawData.R`](SourceCode/CreateKmeansOnRawData.R) ([doc](SourceCode/CreateKmeansOnRawData.md))
- [`CreateMask_Batch.R`](SourceCode/CreateMask_Batch.R) ([doc](SourceCode/CreateMask_Batch.md))
- [`CreatePNG.R`](SourceCode/CreatePNG.md) ([doc](SourceCode/CreatePNG.md))
- [`CreateRGB_Batch.R`](SourceCode/CreateRGB_Batch.R) ([doc](SourceCode/CreateRGB_Batch.md))
- [`CreateSpectralSpeciesPlantCommunityCorrelationCustomV2.R`](SourceCode/CreateSpectralSpeciesPlantCommunityCorrelationCustomV2.R) ([doc](SourceCode/CreateSpectralSpeciesPlantCommunityCorrelationCustomV2.md))
- [`CreateSpectralSpeciesPlantCommunityCorrelationV2.R`](SourceCode/CreateSpectralSpeciesPlantCommunityCorrelationV2.R) ([doc](SourceCode/CreateSpectralSpeciesPlantCommunityCorrelationV2.md))
- [`ExtractSpectralLines_Script.R`](SourceCode/ExtractSpectralLines_Script.R) ([doc](SourceCode/ExtractSpectralLines_Script.md))
- [`ExtractSpectralLines.R`](SourceCode/ExtractSpectralLines.R) ([doc](SourceCode/ExtractSpectralLines.md))
- [`ExtractSpectralLinesCombinedV2.R`](SourceCode/ExtractSpectralLinesCombinedV2.R) ([doc](SourceCode/ExtractSpectralLinesCombinedV2.md))
- [`PerformKMeansClustering.R`](SourceCode/PerformKMeansClustering.R) ([doc](SourceCode/PerformKMeansClustering.md))
- [`Rectify_Batch.R`](SourceCode/Rectify_Batch.R) ([doc](SourceCode/Rectify_Batch.md))

### R Functions

- [`ClusterAnalysis.R`](SourceCode/R/ClusterAnalysis.R) ([doc](SourceCode/R/ClusterAnalysis.md))
- [`CreateRGB.R`](SourceCode/R/CreateRGB.R) ([doc](SourceCode/R/CreateRGB.md))
- [`CreateRectangleFromPlotlocations.R`](SourceCode/R/CreateRectangleFromPlotlocations.R) ([doc](SourceCode/R/CreateRectangleFromPlotlocations.md))
- [`CreateSAVIMask.R`](SourceCode/R/CreateSAVIMask.R) ([doc](SourceCode/R/CreateSAVIMask.md))
- [`DataAnalysis.R`](SourceCode/R/DataAnalysis.R) ([doc](SourceCode/R/DataAnalysis.md))
- [`RectifyImage.R`](SourceCode/R/RectifyImage.R) ([doc](SourceCode/R/RectifyImage.md))

### Analysis Scripts

- [`ClusterAnalysisNbClustV4.R`](SourceCode/Analysis_Scripts/ClusterAnalysisNbClustV4.R) ([doc](SourceCode/Analysis_Scripts/ClusterAnalysisNbClustV4.md))
- [`CorrelationTesting_All_Custom.R`](SourceCode/Analysis_Scripts/CorrelationTesting_All_Custom.R) ([doc](SourceCode/Analysis_Scripts/CorrelationTesting_All_Custom.md))
- [`CorrelationTesting_All_biodivMapR.R`](SourceCode/Analysis_Scripts/CorrelationTesting_All_biodivMapR.R) ([doc](SourceCode/Analysis_Scripts/CorrelationTesting_All_biodivMapR.md))
- [`CorrelationTesting_Part_C_D_Custom.R`](SourceCode/Analysis_Scripts/CorrelationTesting_Part_C_D_Custom.R) ([doc](SourceCode/Analysis_Scripts/CorrelationTesting_Part_C_D_Custom.md))
- [`CorrelationTesting_Part_C_D_biodivMapR.R`](SourceCode/Analysis_Scripts/CorrelationTesting_Part_C_D_biodivMapR.R) ([doc](SourceCode/Analysis_Scripts/CorrelationTesting_Part_C_D_biodivMapR.md))
- [`CorrelationTesting_Part_E_Custom.R`](SourceCode/Analysis_Scripts/CorrelationTesting_Part_E_Custom.R) ([doc](SourceCode/Analysis_Scripts/CorrelationTesting_Part_E_Custom.md))
- [`CorrelationTesting_Part_E_biodivMapR.R`](SourceCode/Analysis_Scripts/CorrelationTesting_Part_E_biodivMapR.R) ([doc](SourceCode/Analysis_Scripts/CorrelationTesting_Part_E_biodivMapR.md))
- [`CountComparison.R`](SourceCode/Analysis_Scripts/CountComparison.R) ([doc](SourceCode/Analysis_Scripts/CountComparison.md))
- [`CreateCommunityClusters.R`](SourceCode/Analysis_Scripts/CreateCommunityClusters.R) ([doc](SourceCode/Analysis_Scripts/CreateCommunityClusters.md))
- [`CreateSingeFiles.R`](SourceCode/Analysis_Scripts/CreateSingeFiles.R) ([doc](SourceCode/Analysis_Scripts/CreateSingeFiles.md))
- [`ExtractSpectralLines.R`](SourceCode/Analysis_Scripts/ExtractSpectralLines.R) ([doc](SourceCode/Analysis_Scripts/ExtractSpectralLines.md))
- [`ExtractSpectralLines_Batch.R`](SourceCode/Analysis_Scripts/ExtractSpectralLines_Batch.R) ([doc](SourceCode/Analysis_Scripts/ExtractSpectralLines_Batch.md))
- [`Extract_Habitat_Type.R`](SourceCode/Analysis_Scripts/Extract_Habitat_Type.R) ([doc](SourceCode/Analysis_Scripts/Extract_Habitat_Type.md))
- [`Extract_Taxonomy_Data.R`](SourceCode/Analysis_Scripts/Extract_Taxonomy_Data.R) ([doc](SourceCode/Analysis_Scripts/Extract_Taxonomy_Data.md))
- [`FindOptimalK_FinalVersion.R`](SourceCode/Analysis_Scripts/FindOptimalK_FinalVersion.R) ([doc](SourceCode/Analysis_Scripts/FindOptimalK_FinalVersion.md))

---

## Getting Started

See the documentation for each script above for usage instructions, dependencies, and workflow details. For more information on the data structure, see [FOLDER_STRUCTURE.md](FOLDER_STRUCTURE.md).

---

Let me know if you want to customize further!
