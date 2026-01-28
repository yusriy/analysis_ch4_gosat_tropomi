# ==============================================================================
# R Script for GOSAT Methane Mapping over Malaysia (2019)
# ==============================================================================

# ----------------------------
# 1. Library Initialization
# ----------------------------
# These packages constitute the modern R geospatial stack.
library(readxl)         # For reading the.xlsx source file [14]
library(tidyverse)      # For dplyr (manipulation) and ggplot2 (plotting)
library(lubridate)      # For handling Unix timestamps and dates
library(sf)             # For Simple Features spatial data handling [17]
library(rnaturalearth)  # For downloading country boundaries [19]
library(rnaturalearthdata)
library(viridis)        # For scientifically accurate color scales [24]

# ----------------------------
# 2. Data Ingestion & Cleaning
# ----------------------------
# Define the path as requested by the user
file_path <- "C:/Users/ASUS/Documents/GOSAT_2019_Whole_Malaysia.xlsx"

# Read the Excel file. 
# We use 'guess_max' to ensure column types (like numeric xch4) are detected correctly 
# even if the top rows are messy.
gosat_data <- read_excel(file_path, sheet = 1, guess_max = 20000)

# Process the data pipeline
gosat_clean <- gosat_data %>%
  # 2.1 Time Conversion
  # The 'time' column is Unix Epoch (seconds since 1970). 
  mutate(
    date_time = as_datetime(time),
    month_num = month(date_time),
    # Create an ordered factor for months to ensure chronological plotting (Jan->Dec)
    # rather than alphabetical (Apr->Aug) [12]
    month_label = factor(month(date_time, label = TRUE, abbr = FALSE), 
                         levels = month.name) 
  ) %>%
  
  # 2.2 Quality Control Filtering
  # CRITICAL STEP: Filter out bad data. 
  # UoL Proxy protocols dictate that only flag 0 is valid for science.[3, 6]
  # This removes artifacts like the 1952 ppb outlier seen in the raw data.
  filter(xch4_quality_flag == 0) %>%
  
  # 2.3 Temporal Filtering
  # Ensure we strictly look at 2019 data (though filename implies it)
  filter(year(date_time) == 2019)

# ----------------------------
# 3. Spatial Object Creation
# ----------------------------
# Convert the tabular data to a Simple Features object.
# CRS 4326 = WGS84 (Standard Latitude/Longitude) [18]
gosat_sf <- st_as_sf(gosat_clean, coords = c("longitude", "latitude"), crs = 4326)

# ----------------------------
# 4. Basemap Generation
# ----------------------------
# Retrieve vector map data for Malaysia and surrounding region for context.
# 'scale = medium' provides enough detail for national mapping without slow rendering.
basemap <- ne_countries(scale = "medium", returnclass = "sf", 
                        country = c("Malaysia", "Indonesia", "Brunei", "Singapore", "Thailand"))

# Define the bounding box for the map to zoom in on Malaysia
# Longitude: 99E to 120E, Latitude: 0N to 8N
roi_xlim <- c(99, 120)
roi_ylim <- c(0, 8)

# ----------------------------
# 5. Plotting: Maps 1-12 (Monthly Facets)
# ----------------------------
# This creates the 12 requested maps in a single grid layout.

map_monthly <- ggplot() +
  # Layer A: Basemap
  geom_sf(data = basemap, fill = "grey92", color = "grey60", size = 0.3) +
  
  # Layer B: GOSAT Data
  # We map the 'color' aesthetic to the 'xch4' variable.
  geom_sf(data = gosat_sf, aes(color = xch4), size = 2, alpha = 0.8) +
  
  # Layer C: Faceting
  # This command generates the 12 sub-maps automatically [13]
  facet_wrap(~month_label, ncol = 3) +
  
  # Layer D: Scales and Coordinate System
  # Viridis 'magma' palette is used for high contrast and colorblind safety.
  scale_color_viridis_c(option = "magma", name = "CH4 (ppb)", direction = -1) +
  coord_sf(xlim = roi_xlim, ylim = roi_ylim, expand = FALSE) +
  
  # Layer E: Aesthetics and Labels
  labs(
    title = "Spatio-Temporal Distribution of Methane (XCH4) over Malaysia (2019)",
    subtitle = "Monthly breakdown of GOSAT Proxy Retrievals (Quality Flag = 0)",
    x = "Longitude", y = "Latitude"
  ) +
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "white"), # Clean look for facet headers
    strip.text = element_text(face = "bold"),
    legend.position = "bottom",
    legend.key.width = unit(2, "cm")
  )

# Display the monthly maps
print(map_monthly)

# ----------------------------
# 6. Plotting: Map 13 (Annual Aggregate)
# ----------------------------
# This map shows all valid data points for the entire year.

map_aggregate <- ggplot() +
  geom_sf(data = basemap, fill = "grey92", color = "grey60") +
  geom_sf(data = gosat_sf, aes(color = xch4), size = 2, alpha = 0.7) +
  scale_color_viridis_c(option = "magma", name = "CH4 (ppb)", direction = -1) +
  coord_sf(xlim = roi_xlim, ylim = roi_ylim, expand = FALSE) +
  labs(
    title = "Aggregate Methane Concentration over Malaysia (Jan-Dec 2019)",
    subtitle = "Cumulative valid observations (UoL GOSAT Proxy v9.0)",
    caption = "Source: GOSAT_2019_Whole_Malaysia.xlsx",
    x = "Longitude", y = "Latitude"
  ) +
  theme_bw()

# Display the aggregate map
print(map_aggregate)

# ----------------------------
# 7. Saving Results to Computer
# ----------------------------
# Define the output directory
output_dir <- "C:/Users/ASUS/Documents/"

# Save the Monthly Faceted Map (Maps 1-12)
# Filename requested: GOSAT_MONTH_Menthane_map
ggsave(filename = paste0(output_dir, "GOSAT_MONTH_Menthane_map.png"), 
       plot = map_monthly, 
       width = 16, height = 12, dpi = 300)

# Save the Aggregate Map (Map 13)
# Saving with a suffix to avoid overwriting the monthly map
ggsave(filename = paste0(output_dir, "GOSAT_MONTH_Menthane_map_Aggregate.png"), 
       plot = map_aggregate, 
       width = 10, height = 8, dpi = 300)

message(paste("Files saved to:", output_dir))
