# clean environment
rm(list = ls(all = TRUE))
gc()
graphics.off()

library(terra)
library(sf)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(viridis)
library(lme4)
library(lmerTest)
library(writexl)
library(spdep)
library(units)
library(ape)
library(performance)
library(DHARMa)
library(see)

testsite_name <- 'AN_TJ_1'

img_file_path <- file.path('hs_raw',testsite_name)
hyperspectral <- rast(img_file_path)


cluster_shp <- 'cluster_info_shp/metrics_data_filtered.shp'
cluster_shp_data <- st_read(cluster_shp)

cluster_shp_data <- st_transform(cluster_shp_data, crs(hyperspectral))

AN_TJ_1 <- cluster_shp_data %>%
  filter(Testsit==testsite_name)

AN_TJ_1_pixel_values <- terra::extract(hyperspectral, vect(AN_TJ_1))

# Extract mean of pixel values within a 5-meter buffer around each point/polygon
AN_TJ_1_pixel_values <- terra::extract(
  hyperspectral,
  vect(AN_TJ_1),
  buffer = 5,           # buffer size in map units (e.g., meters)
  fun = mean,           # function to apply (mean, median, etc.)
  na.rm = TRUE,         # ignore NA values
  df = TRUE             # return as data frame
# ============================================================================
# Project: SpectralPatang
# Script: ExtractSpectralLines.R
# Description: Extracts spectral lines from hyperspectral raster data for a given test site and associates them with cluster shapefile data.
# Author: Patrick Angst
# Date: 2025-09-30
# Dependencies: terra, sf, dplyr, ggplot2, tidyr, stringr, viridis, lme4, lmerTest, writexl, spdep, units, ape, performance, DHARMa, see
# ============================================================================
)

AN_TJ_1_combined <- bind_cols(AN_TJ_1, AN_TJ_1_pixel_values)

AN_TJ_1_combined_df <- AN_TJ_1_combined %>%
  select(-c(Tblnmbr,ID)) %>%
  st_drop_geometry() %>%  # Completely remove spatial attributes
  as.data.frame()         # Convert to a standard data frame

# Identify spectral band columns (exclude metadata)
band_columns <- setdiff(names(AN_TJ_1_combined_df), c("HbttTyp", "Testsit", "PltIdnt"))

# Rename band columns sequentially
names(AN_TJ_1_combined_df)[match(band_columns, names(AN_TJ_1_combined_df))] <- paste0("Band ", seq_along(band_columns))


# Convert spectral data into long format for plotting
AN_TJ_1_pixel_values_long <- AN_TJ_1_combined_df %>%
  pivot_longer(-c(HbttTyp, Testsit, PltIdnt),
               names_to = "Band",
               values_to = "Reflectance") %>%
  mutate(Wavelength = as.numeric(str_extract(Band, "\\d+"))) %>%
  mutate(Reflectance = Reflectance)  # Scaling reflectance if needed

################################################################################
################################################################################

AN_TJ_1_pixel_values_long <- AN_TJ_1_pixel_values_long %>%
  mutate(
    Reflectance = case_when(
      Wavelength >= 191 & Wavelength <= 211 ~ NA_real_,
      Wavelength >= 285 & Wavelength <= 321 ~ NA_real_,
      Wavelength >= 416 ~ NA_real_,
      TRUE ~ Reflectance
    )
  )

# Ensure wavelengths are sorted correctly
unique_wavelengths_signature <- sort(unique(AN_TJ_1_pixel_values_long$Wavelength))

# Generate spectral signature plot
signature_plot <- ggplot(
  AN_TJ_1_pixel_values_long,
  aes(
    x = Wavelength,
    y = Reflectance,
    color = HbttTyp,
    group = PltIdnt
  )
) +
  geom_line() +
  labs(x = "Wavelength (nm)", y = "Reflectance", title = paste0("Spectral signatures per habitat type ",testsite_name)) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(AN_TJ_1_pixel_values_long$Wavelength), max(AN_TJ_1_pixel_values_long$Wavelength), by = 5)) + # Example: breaks every 50 nm
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_viridis(discrete = TRUE)

# Display the plot
print(signature_plot)

################################################################################
################################################################################

AN_TJ_1_pixel_values_summary <- AN_TJ_1_pixel_values_long %>%
  group_by(HbttTyp, Wavelength) %>%
  summarize(
    MeanReflectance = mean(Reflectance, na.rm = TRUE),
    MinReflectance = min(Reflectance, na.rm = TRUE),
    MaxReflectance = max(Reflectance, na.rm = TRUE),
    .groups = "drop"
  )

mean_signature_plot <- ggplot(
  AN_TJ_1_pixel_values_summary,
  aes(x = Wavelength, y = MeanReflectance, color = HbttTyp, fill = HbttTyp)
) +
  geom_ribbon(
    aes(ymin = MinReflectance, ymax = MaxReflectance),
    alpha = 0.2,
    color = NA
  ) +
  geom_line(linewidth = 1.2) +
  labs(
    x = "Wavelength (nm)",
    y = "Mean Reflectance",
    title = paste0("Mean spectral signatures per habitat type ", testsite_name)
  ) +
  theme_minimal() +
  scale_x_continuous(
    breaks = seq(min(AN_TJ_1_pixel_values_summary$Wavelength),
                 max(AN_TJ_1_pixel_values_summary$Wavelength), by = 5)
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_viridis(discrete = TRUE) +
  scale_fill_viridis(discrete = TRUE)

# Display the plot
print(mean_signature_plot)

#
# Statistical Testing
#
AN_TJ_1_pixel_values_long_stat <- AN_TJ_1_pixel_values_long %>%
  dplyr::select(-c(Band,Testsit))  %>%
  mutate(Wavelength = as.factor(Wavelength)) %>%
  mutate(HbttTyp = as.factor(HbttTyp)) %>%
  mutate(PltIdnt = as.factor(PltIdnt))

str(AN_TJ_1_pixel_values_long_stat)

model_lm <- lm(Reflectance ~ HbttTyp  + PltIdnt + Wavelength + HbttTyp:Wavelength ,
               data = AN_TJ_1_pixel_values_long_stat)
anova(model_lm)
par(mfrow = c(2, 2))
plot(model_lm)