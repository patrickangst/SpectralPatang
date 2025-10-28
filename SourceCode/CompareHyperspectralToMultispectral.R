# Clean environment
rm(list = ls(all = TRUE))
gc()
graphics.off()

library(terra)
library(tidyverse)
library(ggplot2)
library(tidyr)
library(dplyr)


# Load multiband raster
raster_path <- "~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/final_hs_data_folder/AN_TJ_1/image_rectified/ang20220711t002111_rfl_v2aa2_img_rectified"
img <- terra::rast(raster_path)  # or raster::stack()


img_extent <- ext(img)
center_x <- (img_extent$xmin + img_extent$xmax) / 2
center_y <- (img_extent$ymin + img_extent$ymax) / 2

# Define coordinate (in CRS of raster)
# Example: UTM or projected coordinates
target_coord <- cbind(x = center_x, y = center_y)

print(target_coord)

# plot(img)

# Extract reflectance values for the pixel
pixel_values <- terra::extract(img, target_coord)
print(pixel_values)

pixel_values_cleaned <- pixel_values

# Remove " Nanometers" and convert to numeric
clean_names <- gsub(" Nanometers", "", names(pixel_values_cleaned))
numeric_wavelengths <- as.integer(clean_names)

# Assign cleaned names back
names(pixel_values_cleaned) <- numeric_wavelengths


spectral_df <- pixel_values_cleaned %>%
  pivot_longer(cols = everything(), names_to = "Wavelength", values_to = "Reflectance") %>%
  mutate(Wavelength = as.integer(Wavelength))


################################################################################
# Hyperspectral Line
################################################################################
hyperspectral_df <- spectral_df %>%
  mutate(
    Reflectance = case_when(
      Wavelength >= 0 & Wavelength <= 442 ~ NA_real_,
      Wavelength >= 1310 & Wavelength <= 1429 ~ NA_real_,
      Wavelength >= 1800 & Wavelength <= 1980 ~ NA_real_,
      Wavelength >= 2456 ~ NA_real_,
      TRUE ~ Reflectance
    )
  )


################################################################################
# Multispectral Line
################################################################################

# Define the wavelengths you want to keep
keep_wavelengths <- c(442, 492, 557, 662,702, 742, 782, 832, 863, 943, 1614, 2000)

# Set all other reflectance values to NA
multispectral_df <- spectral_df %>%
  mutate(
    Reflectance = if_else(Wavelength %in% keep_wavelengths, Reflectance, NA_real_)
  )


# Define spectral regions
band_data <- tibble(
  Lower = c(375, 450, 485, 500, 565, 590, 625, 740, 1100),
  Upper = c(450, 485, 500, 565, 590, 625, 740, 1100, 2500),
  FillCategory = c(
    "Violet", "Blue", "Cyan", "Green", "Yellow", "Orange",
    "Red", "Near-Infrared", "Shortwave-Infrared"
  )
)

# Set Source as a factor with desired legend order
hyperspectral_df <- hyperspectral_df %>% mutate(Source = factor("Hyperspectral (AVIRIS-NG)", levels = c("Multispectral (Sentinel-2)", "Hyperspectral (AVIRIS-NG)")))
multispectral_df <- multispectral_df %>% mutate(Source = factor("Multispectral (Sentinel-2)", levels = c("Multispectral (Sentinel-2)", "Hyperspectral (AVIRIS-NG)")))


combined_df <- bind_rows(hyperspectral_df, multispectral_df)

plot_multispectral_only <- ggplot() +
  geom_rect(
    data = band_data,
    aes(xmin = Lower, xmax = Upper, ymin = -Inf, ymax = Inf, fill = FillCategory),
    alpha = 0.2
  ) +
  geom_point(
    data = multispectral_df,
    aes(x = Wavelength, y = Reflectance, color = Source),
    size = 2,
    na.rm = TRUE
  ) +
  labs(
    # title = "Multispectral Signature",
    x = "Wavelength [nm]",
    y = "Reflectance",
    color = "Data Source"
  ) +
  theme_minimal() +
  scale_fill_manual(
    name = "Spectral Region",
    values = c(
      "Violet" = "violet", "Blue" = "blue", "Cyan" = "cyan", "Green" = "green",
      "Yellow" = "yellow", "Orange" = "darkorange", "Red" = "red",
      "Near-Infrared" = "darkgrey", "Shortwave-Infrared" = "indianred"
    )
  ) +
  scale_color_manual(values = c("Multispectral (Sentinel-2)" = "blue")) +
  scale_y_continuous(limits = c(0, 0.4))

print(plot_multispectral_only)

plot_combined <- ggplot() +
  geom_rect(
    data = band_data,
    aes(xmin = Lower, xmax = Upper, ymin = -Inf, ymax = Inf, fill = FillCategory),
    alpha = 0.2
  ) +
  geom_line(
    data = combined_df,
    aes(x = Wavelength, y = Reflectance, color = Source),
    linewidth = 1,
    na.rm = TRUE
  ) +
  geom_point(
    data = filter(combined_df, Source == "Multispectral (Sentinel-2)"),
    aes(x = Wavelength, y = Reflectance, color = Source),
    size = 2,
    na.rm = TRUE
  ) +
  labs(
    # title = "Hyperspectral vs Multispectral Signature",
    x = "Wavelength [nm]",
    y = "Reflectance",
    color = "Data Source"
  ) +
  theme_minimal() +
  scale_fill_manual(
    name = "Spectral Region",
    values = c(
      "Violet" = "violet", "Blue" = "blue", "Cyan" = "cyan", "Green" = "green",
      "Yellow" = "yellow", "Orange" = "darkorange", "Red" = "red",
      "Near-Infrared" = "darkgrey", "Shortwave-Infrared" = "indianred"
    )
  ) +
  scale_color_manual(
    values = c(
      "Multispectral (Sentinel-2)" = "blue",
      "Hyperspectral (AVIRIS-NG)" = "black"
    )
  ) +
  scale_y_continuous(limits = c(0, 0.4))


print(plot_combined)

target_folder_path <- '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/hs_vs_ms'

multispectral_only_path <- file.path(target_folder_path,'multispectral_only.png')
multispectral_vs_hyperspectral_path <- file.path(target_folder_path,'multispectral_vs_hyperspectral.png')

ggsave(multispectral_only_path, plot_multispectral_only, width = 12, height = 8, dpi = 300)
ggsave(multispectral_vs_hyperspectral_path, plot_combined, width = 12, height = 8, dpi = 300)

