# Load necessary libraries
if (!require("writexl")) install.packages("writexl")
library(ncdf4)
library(writexl)

# 1. Define the folder path
data_folder <- "D:/GOSAT data/"

# 2. Get 2024 files
all_files <- list.files(path = data_folder, pattern = "\\.nc$", full.names = TRUE)
year_2019_files <- all_files[grepl("2019", all_files)]

print(paste("Found", length(year_2019_files), "files for 2019."))

# 3. Define variables
variable_names <- c("xch4", "xch4_uncertainty", "xch4_quality_flag", 
                    "latitude", "longitude", "time", "surface_altitude", 
                    "surface_air_pressure_apriori", "solar_zenith_angle", 
                    "raw_xco2", "raw_xch4", "model_xco2", "sensor_zenith_angle")

# 4. Processing Function
process_nc_file <- function(file_path) {
  nc_data <- tryCatch(nc_open(file_path), error = function(e) return(NULL))
  if (is.null(nc_data)) return(NULL)
  
  file_data_list <- list()
  for (var in variable_names) {
    if (var %in% names(nc_data$var)) {
      file_data_list[[var]] <- as.vector(ncvar_get(nc_data, var))
    } else {
      file_data_list[[var]] <- NA
    }
  }
  nc_close(nc_data)
  
  temp_df <- as.data.frame(file_data_list)
  temp_df$source_file <- basename(file_path)
  return(temp_df)
}

# 5. Run Batch Processing
print("Processing files... please wait.")
list_of_dfs <- lapply(year_2019_files, process_nc_file)
list_of_dfs <- list_of_dfs[!sapply(list_of_dfs, is.null)]
final_combined_df <- do.call(rbind, list_of_dfs)

# Convert time
if ("time" %in% names(final_combined_df)) {
  final_combined_df$time_readable <- as.POSIXct(final_combined_df$time, origin="1970-01-01", tz="UTC")
}

# =======================================================
# 6. FILTERING SECTION (New Code)
# =======================================================

# Filter 1: Whole Malaysia (Bounding Box)
# Latitude: 0°N to 8°N
# Longitude: 99°E to 120°E (Covers Peninsular + Sabah/Sarawak)
df_whole_malaysia <- subset(final_combined_df, 
                            latitude >= 0 & latitude <= 8 & 
                              longitude >= 99 & longitude <= 120)

# Filter 2: Peninsular Malaysia Only (Bounding Box)
# Longitude cutoff: Approx 105°E separates West from East Malaysia
df_peninsular <- subset(df_whole_malaysia, 
                        longitude <= 105)

# Optional: Filter for Good Quality Only (Flag = 0)
# Uncomment the next lines if you want to remove bad data automatically
# df_whole_malaysia <- subset(df_whole_malaysia, xch4_quality_flag == 0)
# df_peninsular <- subset(df_peninsular, xch4_quality_flag == 0)

print(paste("Rows in Whole Malaysia:", nrow(df_whole_malaysia)))
print(paste("Rows in Peninsular Malaysia:", nrow(df_peninsular)))

# =======================================================
# 7. EXPORT TO EXCEL
# =======================================================

# Save Whole Malaysia Data
write_xlsx(df_whole_malaysia, "GOSAT_2019_Whole_Malaysia.xlsx")

# Save Peninsular Malaysia Data
write_xlsx(df_peninsular, "GOSAT_2019_Peninsular_Malaysia.xlsx")

print("Files saved successfully!")
