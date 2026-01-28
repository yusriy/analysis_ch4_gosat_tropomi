# ==============================================================================
# 2024 COMPARISON: TROPOMI vs GOSAT-2
# Output: 1. Data Table (CSV)  2. Clean Map (PNG)
# ==============================================================================

# 1. SETUP
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, sf, rnaturalearth, readxl, lubridate)

# 2. DEFINE PATHS (2024 Data)
path_trop_24  <- "D:/TROPOMI data/2024_TROPOMI_Methane Concentration_excel/TROPOMI_2024_Whole_Malaysia.xlsx"
path_g2_24    <- "D:/GOSAT 2 data/2024_GOSAT2_Methane Concentration_excel/GOSAT2_2024_Malaysia_Whole.xlsx"

# 3. DATA LOADER
load_data_24 <- function(path, sat_name) {
  if (!file.exists(path)) { message("❌ Missing: ", path); return(NULL) }
  
  if (grepl("xlsx$", path)) df <- read_excel(path) else df <- read_csv(path, show_col_types=F)
  names(df) <- tolower(names(df)) 
  
  # Standardize Column Names
  if ("ch4_ppb" %in% names(df)) { 
    df <- df %>% rename(Lat = lat, Lon = lon, Date = date, Value = ch4_ppb)
  } else if ("methane_xch4" %in% names(df)) { 
    df <- df %>% rename(Lat = latitude, Lon = longitude, Date = time, Value = methane_xch4)
  }
  
  if (!"Date" %in% names(df)) return(NULL)
  
  # Clean Data
  df %>%
    mutate(
      Date = as_datetime(Date), Day = as.Date(Date), Satellite = sat_name,
      Value = ifelse(Value < 10, Value * 1000, Value) # Ensure ppb units
    ) %>%
    filter(!is.na(Value), Lat > 0, Lat < 8, Lon > 99, Lon < 120) %>%
    select(Day, Lat, Lon, Value, Satellite)
}

message("⏳ Loading 2024 Datasets...")
t_data  <- load_data_24(path_trop_24, "TROPOMI")
g2_data <- load_data_24(path_g2_24, "GOSAT-2")

# 4. FIND MATCHES (TROPOMI vs GOSAT-2)
message("🔍 Finding Overlaps for 2024...")
matches_df <- data.frame()

if(!is.null(t_data) & !is.null(g2_data)) {
  s1_sf <- st_as_sf(t_data, coords=c("Lon","Lat"), crs=4326) # TROPOMI
  s2_sf <- st_as_sf(g2_data, coords=c("Lon","Lat"), crs=4326) # GOSAT-2
  
  common_days <- intersect(unique(t_data$Day), unique(g2_data$Day))
  
  if(length(common_days) > 0) {
    for(d in common_days) {
      day_val <- as.Date(d, origin="1970-01-01")
      s1_d <- s1_sf %>% filter(Day == day_val)
      s2_d <- s2_sf %>% filter(Day == day_val)
      
      if(nrow(s1_d)>0 & nrow(s2_d)>0) {
        # Find TROPOMI points within 10km of GOSAT-2
        m <- st_join(s2_d, s1_d, join=st_is_within_distance, dist=10000, left=FALSE)
        
        if(nrow(m)>0) {
          clean <- m %>%
            mutate(Lat=st_coordinates(.)[,2], Lon=st_coordinates(.)[,1],
                   Diff = Value.y - Value.x) %>% # TROPOMI - GOSAT-2
            st_drop_geometry() %>%
            transmute(
              Date = Day.x, 
              Lat, Lon, 
              Dataset_1 = "TROPOMI", Value_1_ppb = round(Value.y, 2),
              Dataset_2 = "GOSAT-2", Value_2_ppb = round(Value.x, 2),
              Bias_ppb = round(Diff, 2),
              Match_Type = "TROPOMI vs GOSAT-2"
            )
          matches_df <- rbind(matches_df, clean)
        }
      }
    }
  }
}

# ==============================================================================
# 5. OUTPUT 1: SAVE DATA TO CSV (For Excel)
# ==============================================================================
if(nrow(matches_df) > 0) {
  csv_filename <- "TROPOMI_vs_GOSAT2_Matches_2024.csv"
  write_csv(matches_df, csv_filename)
  message(paste("✅ TABLE SAVED: Open", csv_filename, "in Excel to see the bias data."))
} else {
  message("⚠️ No overlaps found between TROPOMI and GOSAT-2 in 2024.")
}

# ==============================================================================
# 6. OUTPUT 2: SAVE CLEAN MAP (PNG - No Table)
# ==============================================================================
if(nrow(matches_df) > 0) {
  message("🎨 Generating Map...")
  world <- ne_countries(scale = "medium", returnclass = "sf")
  
  p_map <- ggplot() +
    geom_sf(data = world, fill = "gray95", color = "gray60") +
    coord_sf(xlim = c(99, 120), ylim = c(0, 8)) +
    
    # Plot ONLY the matching points
    geom_point(data = matches_df, aes(x=Lon, y=Lat), 
               color="#2ecc71", size=3, alpha=0.8, shape=16) + # Green dots
    
    theme_bw() +
    labs(
      title = "Validated Overlaps: TROPOMI vs GOSAT-2 (2024)",
      subtitle = paste("Showing", nrow(matches_df), "locations where both satellites coincided."),
      x = "Longitude", y = "Latitude"
    )
  
  ggsave("Map_TROPOMI_vs_GOSAT2_2024_Clean.png", p_map, width = 10, height = 7, dpi = 300)
  message("✅ MAP SAVED: Map_TROPOMI_vs_GOSAT2_2024_Clean.png")
}
