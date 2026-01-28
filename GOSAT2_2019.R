# Load necessary libraries
library(hdf5r)
library(dplyr)
library(writexl)

# ==========================================
# 1. SETUP PATHS
# ==========================================
# Set the folder path where your files are located
data_folder <- "D:/GOSAT 2 data/2019"

# Get a list of all .h5 files in that folder
file_list <- list.files(path = data_folder, pattern = "\\.h5$", full.names = TRUE)

# Check if files were found
if (length(file_list) == 0) {
  stop("No .h5 files found in the specified folder. Please check the path.")
} else {
  print(paste("Found", length(file_list), "files to process."))
}

# ==========================================
# 2. FUNCTION TO PROCESS ONE FILE
# ==========================================
# === REDEFINE THE READING FUNCTION TO BE SAFE ===
process_gosat_file <- function(current_file) {
  tryCatch({
    # These paths are SPECIFIC to GOSAT-2. 
    # If you are reading TROPOMI, these will fail!
    methane <- h5read(current_file, "GasColumn_Proxy/XCH4_proxy")
    quality <- h5read(current_file, "GasColumn_Proxy/XCH4_proxy_quality_flag")
    lat     <- h5read(current_file, "SoundingGeometry/latitude")
    lon     <- h5read(current_file, "SoundingGeometry/longitude")
    
    # Handle Time (sometimes stored differently)
    time    <- h5read(current_file, "SoundingAttribute/observationTime")
    
    # Create Data Frame
    df <- data.frame(
      Time         = as.character(time),
      Latitude     = as.vector(lat),
      Longitude    = as.vector(lon),
      Methane_XCH4 = as.vector(methane),
      Quality_Flag = as.vector(quality),
      stringsAsFactors = FALSE
    )
    return(df)
    
  }, error = function(e) {
    # Print the specific error so we know why it failed
    message(paste("FAILED on file:", basename(current_file)))
    message(paste("ERROR MSG:", e$message))
    return(NULL)
  })
}

# === RUN THE BATCH PROCESS ===
print("Starting batch processing...")
data_list <- lapply(file_list, process_gosat_file)

# Combine only the valid dataframes (remove NULLs)
data_list_clean <- data_list[!sapply(data_list, is.null)]
all_data <- do.call(rbind, data_list_clean)

# Check if we actually have data now
if (is.null(all_data)) {
  stop("CRITICAL ERROR: No data was extracted. Check the error messages above!")
} else {
  print(paste("Success! Total rows extracted:", nrow(all_data)))
}

# ==========================================
# 4. FILTERING FOR MALAYSIA
# ==========================================

# Filter A: Whole Malaysia
# Bounding box approx: Lat 0.8 to 7.5, Lon 99.5 to 119.5
malaysia_whole <- all_data %>%
  filter(Latitude >= 0.8 & Latitude <= 7.5,
         Longitude >= 99.5 & Longitude <= 119.5) %>%
  # Filter for Valid Data (Quality Flag 0 usually means Good)
  filter(Quality_Flag == 0 & Methane_XCH4 > 0)

# Filter B: Peninsular Malaysia Only
# Bounding box approx: Lat 1.2 to 6.8, Lon 99.5 to 104.5
malaysia_peninsular <- malaysia_whole %>%
  filter(Longitude <= 104.5)

print(paste("Rows for Whole Malaysia:", nrow(malaysia_whole)))
print(paste("Rows for Peninsular Malaysia:", nrow(malaysia_peninsular)))

# ==========================================
# 5. EXPORT TO EXCEL
# ==========================================

# Define output names
output_path_whole <- "GOSAT2_2019_Malaysia_Whole.xlsx"
output_path_peninsular <- "GOSAT2_2019_Malaysia_Peninsular.xlsx"

# Write files
write_xlsx(malaysia_whole, output_path_whole)
write_xlsx(malaysia_peninsular, output_path_peninsular)

print(paste("Files saved to your working directory:", getwd()))
