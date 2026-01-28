library(ncdf4)
library(tidyverse)
library(writexl)
library(lubridate)

# ==========================================
# 1. SETUP PARAMETERS
# ==========================================
data_dir <- "D:/TROPOMI data/2019"
file_list <- list.files(path = data_dir, pattern = "\\.nc$", full.names = TRUE)

# --- DEFINE REGIONS ---
# We define a function to check if a point is in a box
# Region 1: Whole Malaysia
my_lat_min <- 0
my_lat_max <- 8
my_lon_min <- 99
my_lon_max <- 120

# Region 2: Peninsular Malaysia Only (Subset of above)
pen_lat_min <- 1
pen_lat_max <- 7
pen_lon_min <- 99
pen_lon_max <- 105

all_data_list <- list()

# ==========================================
# 2. DATA EXTRACTION LOOP
# ==========================================
print(paste("Found", length(file_list), "files. Scanning for Malaysia data..."))

for (file_path in file_list) {
  tryCatch({
    nc <- nc_open(file_path)
    
    # Check if this file even covers Malaysia (Optimization)
    # We look at the min/max lat/lon of the whole file first
    # Note: Variable names might vary slightly, but standard TROPOMI L2 is usually consistent
    file_lats <- ncvar_get(nc, "PRODUCT/latitude")
    file_lons <- ncvar_get(nc, "PRODUCT/longitude")
    
    # If the file's coverage doesn't touch Malaysia coordinates, skip it!
    if (max(file_lats) < my_lat_min | min(file_lats) > my_lat_max |
        max(file_lons) < my_lon_min | min(file_lons) > my_lon_max) {
      nc_close(nc)
      next # Skip to next file
    }
    
    # If it overlaps, extract the Methane data
    ch4_values <- ncvar_get(nc, "PRODUCT/methane_mixing_ratio_bias_corrected")
    qa_values <- ncvar_get(nc, "PRODUCT/qa_value")
    
    # Get Time
    time_str <- ncatt_get(nc, 0, "time_coverage_start")$value
    file_date <- as_datetime(time_str)
    
    nc_close(nc)
    
    # Create DataFrame
    df_temp <- data.frame(
      lat = as.vector(file_lats),
      lon = as.vector(file_lons),
      ch4_ppb = as.vector(ch4_values),
      qa = as.vector(qa_values)
    )
    
    # --- FILTERING ---
    # 1. Quality Filter
    df_temp <- df_temp %>% filter(qa > 0.5)
    
    # 2. Spatial Filter: Keep Data for WHOLE MALAYSIA
    df_temp <- df_temp %>% 
      filter(lat >= my_lat_min & lat <= my_lat_max & 
               lon >= my_lon_min & lon <= my_lon_max)
    
    # Add date
    df_temp$date <- file_date
    df_temp <- df_temp %>% drop_na(ch4_ppb)
    
    if (nrow(df_temp) > 0) {
      all_data_list[[file_path]] <- df_temp
      print(paste("Match found in:", basename(file_path), "| Rows:", nrow(df_temp)))
    }
    
  }, error = function(e) { 
    # Ignore errors to keep loop running
  })
}

# ==========================================
# 3. EXPORT RESULTS
# ==========================================
full_malaysia_data <- bind_rows(all_data_list)

if (nrow(full_malaysia_data) > 0) {
  print(paste("Total Malaysia data points:", nrow(full_malaysia_data)))
  
  # --- EXPORT 1: WHOLE MALAYSIA ---
  write_xlsx(full_malaysia_data, "TROPOMI_2019_Whole_Malaysia.xlsx")
  print("Saved: TROPOMI_2019_Whole_Malaysia.xlsx")
  
  # --- EXPORT 2: PENINSULAR MALAYSIA ONLY ---
  # We just filter the "Whole Malaysia" dataset again
  peninsular_data <- full_malaysia_data %>% 
    filter(lat >= pen_lat_min & lat <= pen_lat_max &
             lon >= pen_lon_min & lon <= pen_lon_max)
  
  write_xlsx(peninsular_data, "TROPOMI_2019_Peninsular_Malaysia.xlsx")
  print("Saved: TROPOMI_2019_Peninsular_Malaysia.xlsx")
  
} else {
  print("No data found covering Malaysia in these files.")
}
