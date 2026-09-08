
# SpectralPatang

This repository contains the code and data for the SpectralPatang project, supporting GIS data processing, spectral analysis, and clustering workflows for ecological research.

## Thesis Abstract

The Arctic tundra is experiencing rapid environmental changes, triggering responses in plant biodiversity. This calls for scalable and continuous monitoring methods. The application of the Spectral Variability Hypothesis (SVH) with hyperspectral remote sensing, offers a promising approach, suggesting that spectral diversity can serve as a proxy for species diversity. In this thesis I validate the reliability of the SVH in the Arctic tundra by testing whether spectral species richness and identity, derived from hyperspectral images, can be used to infer in situ ecological metrics such as plant species richness, plant community richness or habitat type richness. Using National Aeronautics and Space Administration (NASA) Airborne Visible InfraRed Imaging Spectrometer - Next Generation (AVIRIS-NG) Level 2 surface reflectance imagery (3-7m resolution) and ground-based vegetation data from the Arctic Vegetation Archive (AVA) across 13 test sites in northern Alaska, I employed the **biodivMapR** package for the R statistical computing software to generate "spectral species" maps via k-means clustering. I also implemented a novel, data-driven approach using the Within-Cluster Sum of Squares (WCSS) metric to optimize the number of spectral species for each site. I compared these results against a workflow I developed, referred to in the thesis as the *Custom* workflow, which preserved the surface reflectance information throughout the analysis. I then validated the derived spectral diversity metrics against three key in situ biodiversity indicators: plant species richness, plant community richness and habitat type richness. The results revealed no statistically significant correlations between spectral species richness and any of the in situ ecological metrics in either workflow. While analyses of spectral species identity showed significant associations with field-based ecological classifications at a few of the sites, these findings were not consistent or generalizable across the study area. Furthermore, an analysis of the reflectance curves extracted at the plot locations showed a high variability and spectral overlap between habitat types. This prevented a visual distinction of habitat classification. The lack of a consistent relationship is attributed to several key limitations. Ecologically, these include the combined challenges of the mixed pixel problem, where sensor resolution is coarser than the fine-scale heterogeneity of tundra vegetation and the complex structural and functional convergence of Arctic plant communities. Another factor could be the fact that there is a gap of 2 to 21 years between the collection of in situ data and the creation of the hyperspectral images. This greatly reduces the likelihood of finding a correlation between spectral data and ecological metrics. Based on my study, I conclude that the SVH, as implemented through the spectral species concept and given the chosen methods and parameters, is not a reliable proxy for biodiversity in the Arctic tundra. The findings show that further studies using different approaches, such as deep learning, data fusion and trait-based ecology, are needed to fully leverage the potential of hyperspectral remote sensing in this environment.

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
