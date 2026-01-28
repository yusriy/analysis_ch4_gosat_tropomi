# ==============================================================================
# TROPOMI Methane Concentration Mapping - Malaysia 2024
# ==============================================================================

# 1. SETUP & LIBRARY LOADING ---------------------------------------------------
# Ensure these packages are installed. 
# Note: 'rnaturalearthhires' is required for high-res maps but is not on CRAN.
# If missing, install via: remotes::install_github("ropensci/rnaturalearthhires")

library(readxl)             # Read Excel files
library(tidyverse)          # Data manipulation (dplyr) and visualization (ggplot2)
library(sf)                 # Spatial data handling
library(rnaturalearth)      # Country boundaries
library(rnaturalearthhires) # High-resolution map data
library(viridis)            # Scientifically accurate color scales
library(lubridate)          # Date parsing

# 2. DATA INGESTION ------------------------------------------------------------
# The specific location provided for the dataset
input_path <- "C:/Users/ASUS/Documents/TROPOMI_2019_Whole_Malaysia.xlsx"

message("Attempting to load data from: ", input_path)

# Read the Excel file
# We suppress warnings to keep the console clean, but check column types if issues arise
raw_data <- read_xlsx(input_path)

# 3. DATA PROCESSING -----------------------------------------------------------
message("Processing data and parsing dates...")

processed_sf <- raw_data %>%
  # Standardize column names to lowercase (e.g., 'Lat' -> 'lat')
  rename_with(tolower) %>%
  mutate(
    # Parse the 'date' column. 
    # 'as_datetime' handles ISO strings ("2019-01-02...") or Excel serials automatically
    date_clean = as_datetime(date),
    
    # Create Month Name for labeling (Ordered Factor: Jan < Feb < Mar)
    month_name = month(date_clean, label = TRUE, abbr = FALSE),
    
    # Create Numeric Month for sorting filenames (01, 02, 03...)
    month_num = sprintf("%02d", month(date_clean))
  ) %>%
  # Remove invalid rows (missing coords or data)
  filter(!is.na(lat),!is.na(lon),!is.na(ch4_ppb)) %>%
  # Convert to Spatial Object (WGS84 Coordinates)
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

# 4. MAP CONFIGURATION ---------------------------------------------------------
# Define global limits for the color scale.
# This ensures 1900 ppb is the exact same color on ALL 13 maps for comparison.
min_ch4 <- min(processed_sf$ch4_ppb, na.rm = TRUE)
max_ch4 <- max(processed_sf$ch4_ppb, na.rm = TRUE)

message(paste("Global Color Scale set to:", round(min_ch4, 1), "-", round(max_ch4, 1), "ppb"))

# Load Malaysia Basemap (High Resolution)
# scale = 10 provides the detail needed for coastlines and islands
basemap <- ne_countries(scale = 10, country = "Malaysia", returnclass = "sf")

# Get bounding box to zoom the camera on Malaysia
map_bbox <- st_bbox(basemap)

# 5. PLOTTING FUNCTION ---------------------------------------------------------
# A reusable function to generate and save a map
create_and_save_map <- function(map_data, map_title, filename) {
  
  p <- ggplot() +
    # Layer 1: Malaysia Landmass (Grey background)
    geom_sf(data = basemap, fill = "grey95", color = "grey60", size = 0.3) +
    
    # Layer 2: Methane Data Points
    # alpha = 0.6 makes points semi-transparent to show density in overlapping areas
    geom_sf(data = map_data, aes(color = ch4_ppb), size = 0.8, alpha = 0.6, stroke = 0) +
    
    # Layer 3: Color Scale (Viridis 'Plasma')
    # We force the limits to be the global min/max calculated earlier
    scale_color_viridis_c(
      option = "C", 
      name = "Methane\n(ppb)", 
      limits = c(min_ch4, max_ch4)
    ) +
    
    # Layer 4: Viewport and Formatting
    coord_sf(
      xlim = c(map_bbox["xmin"], map_bbox["xmax"]), 
      ylim = c(map_bbox["ymin"], map_bbox["ymax"])
    ) +
    labs(
      title = map_title,
      subtitle = "Source: TROPOMI / Sentinel-5P",
      x = "Longitude", 
      y = "Latitude"
    ) +
    theme_bw() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      legend.position = "right",
      panel.grid.major = element_line(color = "grey90", linetype = "dotted")
    )
  
  # Save the file (High resolution 300 DPI)
  ggsave(filename, plot = p, width = 12, height = 7, dpi = 300)
  message(paste("Saved:", filename))
}

# 6. EXECUTION LOOP ------------------------------------------------------------

# --- Map 13: All Data Combined ---
message("Generating Aggregate Map (All Data)...")
create_and_save_map(
  map_data = processed_sf, 
  map_title = "Malaysia Methane Concentration: Year 2019 (All Data)", 
  filename = "13_TROPOMI_Methane_Map_All_Data.png"
)

# --- Maps 1-12: Monthly Loop ---
# Identify which months actually exist in the file
months_present <- unique(processed_sf$month_name) %>% sort()

message("Generating Monthly Maps...")

for(m in months_present) {
  # Filter data for the current month
  monthly_subset <- processed_sf %>% filter(month_name == m)
  
  # Extract numeric string (e.g., "01") for the filename
  m_num <- unique(monthly_subset$month_num)
  
  # Construct Title and Filename
  plot_title <- paste("Malaysia Methane Concentration:", m, "2019")
  file_name <- paste0(m_num, "_Methane_Map_", m, ".png")
  
  # Generate
  create_and_save_map(monthly_subset, plot_title, file_name)
}

message("Job Complete. Check your working directory for the PNG files.")
