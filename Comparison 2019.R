# ==============================================================================
# COMPLETE VALIDATION REPORT: TROPOMI vs GOSAT vs GOSAT-2
# Output: 1. Clean Map (PNG)  2. Complete Data Table (CSV)
# ==============================================================================

# 1. SETUP
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, sf, rnaturalearth, readxl, lubridate)

# 2. DEFINE PATHS
path_trop_19  <- "D:/TROPOMI data/2019_TROPOMI_Methane Concentration_excel/TROPOMI_2019_Whole_Malaysia.xlsx"
path_gosat_19 <- "D:/GOSAT data/2019_GOSAT_Methane Concentration_excel/GOSAT_2019_Whole_Malaysia.xlsx"
path_g2_19    <- "D:/GOSAT 2 data/2019_GOSAT2_Methane Concentration_excel/GOSAT2_2019_Malaysia_Whole.xlsx"

# 3. DATA LOADER
load_data_19 <- function(path, sat_name) {
  if (!file.exists(path)) return(NULL)
  if (grepl("xlsx$", path)) df <- read_excel(path) else df <- read_csv(path, show_col_types=F)
  names(df) <- tolower(names(df)) 
  
  if ("ch4_ppb" %in% names(df)) { 
    df <- df %>% rename(Lat = lat, Lon = lon, Date = date, Value = ch4_ppb)
  } else if ("xch4" %in% names(df)) { 
    if ("time_readable" %in% names(df)) df <- df %>% rename(Date=time_readable) else df <- df %>% rename(Date=time)
    df <- df %>% rename(Lat = latitude, Lon = longitude, Value = xch4)
  } else if ("methane_xch4" %in% names(df)) { 
    df <- df %>% rename(Lat = latitude, Lon = longitude, Date = time, Value = methane_xch4)
  }
  
  if (!"Date" %in% names(df)) return(NULL)
  
  df %>%
    mutate(
      Date = as_datetime(Date), Day = as.Date(Date), Satellite = sat_name,
      Value = ifelse(Value < 10, Value * 1000, Value)
    ) %>%
    filter(!is.na(Value), Lat > 0, Lat < 8, Lon > 99, Lon < 120) %>%
    select(Day, Lat, Lon, Value, Satellite)
}

message("⏳ Loading Data...")
t_data  <- load_data_19(path_trop_19, "TROPOMI")
g1_data <- load_data_19(path_gosat_19, "GOSAT")
g2_data <- load_data_19(path_g2_19, "GOSAT-2")

# 4. FIND OVERLAPS
find_overlaps <- function(sat1, sat2, name1, name2) {
  s1_sf <- st_as_sf(sat1, coords=c("Lon","Lat"), crs=4326)
  s2_sf <- st_as_sf(sat2, coords=c("Lon","Lat"), crs=4326)
  common_days <- intersect(unique(sat1$Day), unique(sat2$Day))
  res <- data.frame()
  
  for(d in common_days) {
    day_val <- as.Date(d, origin="1970-01-01")
    s1_d <- s1_sf %>% filter(Day == day_val)
    s2_d <- s2_sf %>% filter(Day == day_val)
    
    if(nrow(s1_d)>0 & nrow(s2_d)>0) {
      m <- st_join(s2_d, s1_d, join=st_is_within_distance, dist=10000, left=FALSE)
      if(nrow(m)>0) {
        clean <- m %>%
          mutate(Lat=st_coordinates(.)[,2], Lon=st_coordinates(.)[,1],
                 Diff = Value.y - Value.x, 
                 Match_Type = paste(name1, "vs", name2)) %>%
          st_drop_geometry() %>%
          transmute(Date=Day.x, Lat, Lon, 
                    Sat1_Name=name1, Sat1_Value=round(Value.y,2), 
                    Sat2_Name=name2, Sat2_Value=round(Value.x,2), 
                    Bias_ppb=round(Diff, 2), Match_Type)
        res <- rbind(res, clean)
      }
    }
  }
  return(res)
}

message("🔍 Finding Overlaps...")
# A. TROPOMI vs GOSAT (Added Back)
ov_trop_g1 <- find_overlaps(t_data, g1_data, "TROPOMI", "GOSAT")

# B. TROPOMI vs GOSAT-2
ov_trop_g2 <- find_overlaps(t_data, g2_data, "TROPOMI", "GOSAT-2")

# C. GOSAT vs GOSAT-2
ov_g1_g2   <- find_overlaps(g1_data, g2_data, "GOSAT", "GOSAT-2")

# Combine All
all_matches <- rbind(ov_trop_g1, ov_trop_g2, ov_g1_g2)

# ==============================================================================
# 5. OUTPUT 1: SAVE THE DATA TABLE (CSV)
# ==============================================================================
output_filename <- "All_Matches_2019_Complete.csv"
write_csv(all_matches, output_filename)
message(paste("✅ DATA SAVED:", output_filename))

# ==============================================================================
# 6. OUTPUT 2: SAVE THE MAP (PNG)
# ==============================================================================
message("🎨 Generating Map...")
world <- ne_countries(scale = "medium", returnclass = "sf")

p_map <- ggplot() +
  geom_sf(data = world, fill = "gray95", color = "gray60") +
  coord_sf(xlim = c(99, 120), ylim = c(0, 8)) +
  
  # Plot Matches
  geom_point(data = all_matches, aes(x=Lon, y=Lat, color=Match_Type, shape=Match_Type), size=3, stroke=1.5) +
  
  # Define Colors for ALL 3 Pairs
  scale_color_manual(values = c(
    "TROPOMI vs GOSAT" = "#3498db",   # Blue
    "TROPOMI vs GOSAT-2" = "#2ecc71", # Green
    "GOSAT vs GOSAT-2" = "#9b59b6"    # Purple
  )) +
  scale_shape_manual(values = c(16, 17, 18)) +
  
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(
    title = "Validated Methane Overlaps (2019)",
    subtitle = "Comparing TROPOMI, GOSAT, and GOSAT-2 Coincidence Points",
    x = "Longitude", y = "Latitude"
  )

ggsave("Validated_Map_2019_Complete.png", p_map, width = 10, height = 8, dpi = 300)
message("✅ MAP SAVED: Validated_Map_2019_Complete.png")