# ==============================================================================
# GOSAT-2 Methane Mapping Framework for Malaysia
# Target Output: 12 Monthly Maps + 1 Annual Aggregate Map
# ==============================================================================

# 1. LIBRARY LOADING -----------------------------------------------------------
# Loading the Tidyverse for data manipulation and visualization
library(tidyverse) 
# Lubridate for robust ISO 8601 date parsing
library(lubridate)
# sf and rnaturalearth for geospatial handling and basemaps
library(sf)
library(rnaturalearth)
# Scico for scientifically accurate, perceptually uniform color palettes
library(scico)
# ggspatial for cartographic elements (North Arrow, Scale Bar)
library(ggspatial)
library(readxl)

# 2. DATA INGESTION & PRE-PROCESSING -------------------------------------------
# Define file path
file_path <- "D:/GOSAT 2 data/2024_GOSAT2_Methane Concentration_excel/GOSAT2_2024_Malaysia_Whole.xlsx"

# Load data using read_excel
# NOTICE: We removed the 'col_types = cols(...)' section entirely.
gosat_data <- read_excel(file_path)

# Filter and Feature Engineering
clean_data <- gosat_data %>%
  filter(Quality_Flag == 0) %>%
  mutate(
    # Because read_excel might read Time as a string, we explicitly parse it here
    datetime = ymd_hms(Time), 
    month_label = month(datetime, label = TRUE, abbr = FALSE),
    month_idx = month(datetime)
  )

# Filter and Feature Engineering
clean_data <- gosat_data %>%
  # CRITICAL: Filter for Quality Flag 0 (Good Data) per GOSAT-2 specs
  filter(Quality_Flag == 0) %>%
  # Parse ISO 8601 timestamps
  mutate(
    datetime = ymd_hms(Time),
    month_label = month(datetime, label = TRUE, abbr = FALSE), # e.g., "January"
    month_idx = month(datetime) # Numeric 1-12 for sorting
  ) %>%
  # Drop any parsing errors
  drop_na(datetime, Methane_XCH4)

# 3. GEOSPATIAL SETUP ----------------------------------------------------------
# Retrieve Malaysia country boundary (Medium scale 1:50m)
malaysia_map <- ne_countries(scale = "medium", country = "Malaysia", returnclass = "sf")

# Define the bounding box to zoom into the region effectively
# Covers Peninsular + Borneo
geo_bounds <- c(xmin = 99, xmax = 120, ymin = 0.5, ymax = 8)

# 4. COLOR SCALE DEFINITION ----------------------------------------------------
# We must lock the color scale to the global min/max for the entire year
# This ensures Jan (low) and Dec (high) are comparable visually.
global_min <- min(clean_data$Methane_XCH4)
global_max <- max(clean_data$Methane_XCH4)

# 5. MAP GENERATION LOOP (Maps 1-12) -------------------------------------------
# Iterate through months to generate individual reports
for(i in 1:12) {
  
  # Filter for current month
  monthly_subset <- clean_data %>% filter(month_idx == i)
  current_month_name <- month.name[i]
  
  # Skip mapping if no data exists (common in monsoon months)
  if(nrow(monthly_subset) == 0) {
    message(paste("Skipping", current_month_name, "- No valid data."))
    next
  }
  
  # Convert to sf object for plotting
  points_sf <- st_as_sf(monthly_subset, coords = c("Longitude", "Latitude"), crs = 4326)
  
  # Plot Construction
  p <- ggplot() +
    # Layer 1: Basemap
    geom_sf(data = malaysia_map, fill = "#F0F0F0", color = "#555555", size = 0.3) +
    # Layer 2: Methane Data
    geom_sf(data = points_sf, aes(color = Methane_XCH4), size = 2, alpha = 0.8) +
    # Layer 3: Color Scale (Scico 'lajolla' - excellent for thermal data)
    scale_color_scico(palette = "lajolla", 
                      limits = c(global_min, global_max), 
                      name = "XCH4 (ppm)") +
    # Layer 4: Cartography
    annotation_scale(location = "bl", width_hint = 0.3) +
    annotation_north_arrow(location = "tl", which_north = "true", 
                           style = north_arrow_fancy_orienteering) +
    # Layer 5: Aesthetics
    coord_sf(xlim = c(geo_bounds["xmin"], geo_bounds["xmax"]), 
             ylim = c(geo_bounds["ymin"], geo_bounds["ymax"])) +
    labs(
      title = paste("GOSAT-2 Methane Concentration: Malaysia"),
      subtitle = paste(current_month_name, "2024 | Quality Flag = 0 Only"),
      caption = "Source: GOSAT-2 TANSO-FTS-2 | Analysis via R"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 16),
      legend.position = "bottom",
      legend.key.width = unit(2, "cm")
    )
  
  # Output Saving
  ggsave(filename = paste0("2024_GOSAT2_Map_", sprintf("%02d", i), "_", current_month_name, ".png"), 
         plot = p, width = 12, height = 7, dpi = 300)
}

# 6. ANNUAL AGGREGATE MAP (Map 13) ---------------------------------------------
# Convert entire dataset to sf
all_points_sf <- st_as_sf(clean_data, coords = c("Longitude", "Latitude"), crs = 4326)

p_annual <- ggplot() +
  geom_sf(data = malaysia_map, fill = "#F0F0F0", color = "#555555", size = 0.3) +
  # Use smaller points for aggregate to reduce overplotting
  geom_sf(data = all_points_sf, aes(color = Methane_XCH4), size = 1.5, alpha = 0.6) +
  scale_color_scico(palette = "lajolla", 
                    limits = c(global_min, global_max), 
                    name = "XCH4 (ppm)") +
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "tl", style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(geo_bounds["xmin"], geo_bounds["xmax"]), 
           ylim = c(geo_bounds["ymin"], geo_bounds["ymax"])) +
  labs(
    title = "Annual GOSAT-2 Methane Concentration: Malaysia (2024)",
    subtitle = "Aggregate Visualization of All Quality-Assured Retrievals",
    caption = "Source: GOSAT-2 TANSO-FTS-2 | Analysis via R"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "bottom",
    legend.key.width = unit(2, "cm")
  )

ggsave(filename = "2024_GOSAT2_Map_13_Annual_Aggregate.png", plot = p_annual, width = 12, height = 7, dpi = 300)

