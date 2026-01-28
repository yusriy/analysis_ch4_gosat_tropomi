# ==============================================================================
# OBJECTIVE 2: ACCURACY CHECK (CORRELATION ANALYSIS)
# Comparison: TROPOMI vs GOSAT vs GOSAT-2
# ==============================================================================

# 1. SETUP
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, sf, lubridate, knitr, gridExtra, readxl)

# 2. DEFINE EXACT PATHS
path_trop_19  <- "D:/TROPOMI data/2019_TROPOMI_Methane Concentration_excel/TROPOMI_2019_Whole_Malaysia.xlsx"
path_trop_24  <- "D:/TROPOMI data/2024_TROPOMI_Methane Concentration_excel/TROPOMI_2024_Whole_Malaysia.xlsx"
path_gosat_19 <- "D:/GOSAT data/2019_GOSAT_Methane Concentration_excel/GOSAT_2019_Whole_Malaysia.xlsx"
path_g2_19    <- "D:/GOSAT 2 data/2019_GOSAT2_Methane Concentration_excel/GOSAT2_2019_Malaysia_Whole.xlsx"
path_g2_24    <- "D:/GOSAT 2 data/2024_GOSAT2_Methane Concentration_excel/GOSAT2_2024_Malaysia_Whole.xlsx"

# 3. DATA LOADER FUNCTION
load_clean_data <- function(path, name) {
  if(!file.exists(path)) { message("❌ Missing: ", path); return(NULL) }
  
  if(grepl("xlsx$", path)) df <- read_excel(path) else df <- read_csv(path, show_col_types=F)
  names(df) <- tolower(names(df))
  
  # Map Columns
  if("ch4_ppb" %in% names(df)) {
    df <- df %>% rename(Lat=lat, Lon=lon, Date=date, Value=ch4_ppb)
  } else if("xch4" %in% names(df)) {
    if("time_readable" %in% names(df)) df <- df %>% rename(Date=time_readable) else df <- df %>% rename(Date=time)
    df <- df %>% rename(Lat=latitude, Lon=longitude, Value=xch4)
  } else if("methane_xch4" %in% names(df)) {
    df <- df %>% rename(Lat=latitude, Lon=longitude, Date=time, Value=methane_xch4)
  }
  
  if(!"Date" %in% names(df)) return(NULL)
  
  df %>%
    mutate(
      Date = as_datetime(Date), Day = as.Date(Date), Dataset = name,
      Value = ifelse(Value < 10, Value * 1000, Value) # Convert ppm to ppb
    ) %>%
    filter(!is.na(Value), Lat > 0, Lat < 8, Lon > 99, Lon < 120) %>%
    select(Day, Lat, Lon, Value)
}

# Load Datasets
message("Loading Datasets...")
t19  <- load_clean_data(path_trop_19, "TROPOMI 19")
g19  <- load_clean_data(path_gosat_19, "GOSAT 19")
g219 <- load_clean_data(path_g2_19, "GOSAT-2 19")
t24  <- load_clean_data(path_trop_24, "TROPOMI 24")
g224 <- load_clean_data(path_g2_24, "GOSAT-2 24")

# 4. COLLOCATION & STATS FUNCTION
analyze_correlation <- function(df1, df2, name1, name2, year_label) {
  
  # Find Overlaps (Same Day & <10km)
  st1 <- st_as_sf(df1, coords=c("Lon","Lat"), crs=4326)
  st2 <- st_as_sf(df2, coords=c("Lon","Lat"), crs=4326)
  
  common_days <- intersect(unique(df1$Day), unique(df2$Day))
  matched <- data.frame()
  
  for(d in common_days) {
    day_val <- as.Date(d, origin="1970-01-01")
    s1_d <- st1 %>% filter(Day == day_val)
    s2_d <- st2 %>% filter(Day == day_val)
    
    if(nrow(s1_d)>0 & nrow(s2_d)>0) {
      matches <- st_join(s2_d, s1_d, join=st_is_within_distance, dist=10000, left=FALSE)
      if(nrow(matches)>0) {
        clean <- matches %>% st_drop_geometry() %>% 
          select(Val_X = Value.x, Val_Y = Value.y) # X=Sat2(df2), Y=Sat1(df1)
        matched <- rbind(matched, clean)
      }
    }
  }
  
  if(nrow(matched) < 3) {
    message(paste("⚠️ Not enough overlaps for:", name1, "vs", name2))
    return(NULL)
  }
  
  # Stats
  correlation <- cor(matched$Val_X, matched$Val_Y)
  r_squared <- correlation^2
  bias <- mean(matched$Val_Y - matched$Val_X) # Y - X
  rmse <- sqrt(mean((matched$Val_Y - matched$Val_X)^2))
  
  # Scatter Plot
  p <- ggplot(matched, aes(x=Val_X, y=Val_Y)) +
    geom_point(alpha=0.6, color="darkblue") +
    geom_smooth(method="lm", color="red", se=TRUE) +
    geom_abline(slope=1, intercept=0, linetype="dashed", color="gray50") +
    labs(
      title = paste(year_label, ":", name1, "vs", name2),
      subtitle = paste0("N=", nrow(matched), " | R²=", round(r_squared,3), 
                        " | Bias=", round(bias,1)),
      x = paste(name2, "(ppb)"), 
      y = paste(name1, "(ppb)")
    ) +
    theme_bw()
  
  return(list(plot=p, stats=data.frame(
    Pair = paste(name1, "vs", name2),
    Year = year_label,
    N = nrow(matched),
    Correlation_R = round(correlation, 3),
    R_Squared = round(r_squared, 3),
    Mean_Bias_ppb = round(bias, 2),
    RMSE_ppb = round(rmse, 2)
  )))
}

# 5. RUN ANALYSES
results_list <- list()
plots_list <- list()

message("running Analysis 1: TROPOMI vs GOSAT (2019)...")
res1 <- analyze_correlation(t19, g19, "TROPOMI", "GOSAT", "2019")
if(!is.null(res1)) { results_list[[1]] <- res1$stats; plots_list[[1]] <- res1$plot }

message("running Analysis 2: TROPOMI vs GOSAT-2 (2019)...")
res2 <- analyze_correlation(t19, g219, "TROPOMI", "GOSAT-2", "2019")
if(!is.null(res2)) { results_list[[2]] <- res2$stats; plots_list[[2]] <- res2$plot }

message("running Analysis 3: GOSAT vs GOSAT-2 (2019 Sibling Check)...")
# Note: For Sibling Check, we treat GOSAT as 'Y' and GOSAT-2 as 'X'
res3 <- analyze_correlation(g19, g219, "GOSAT", "GOSAT-2", "2019")
if(!is.null(res3)) { results_list[[3]] <- res3$stats; plots_list[[3]] <- res3$plot }

message("running Analysis 4: TROPOMI vs GOSAT-2 (2024)...")
res4 <- analyze_correlation(t24, g224, "TROPOMI", "GOSAT-2", "2024")
if(!is.null(res4)) { results_list[[4]] <- res4$stats; plots_list[[4]] <- res4$plot }

message("running Analysis 5: TROPOMI vs GOSAT-2 (2019)...")
res5 <- analyze_correlation(t19, g19, g219, "TROPOMI", "GOSAT", "GOSAT-2", "2019")
if(!is.null(res5)) { results_list[[5]] <- res5$stats; plots_list[[5]] <- res5$plot }

# 6. OUTPUT
final_table <- bind_rows(results_list)

print("--- FINAL ACCURACY STATISTICS ---")
print(kable(final_table))

write_csv(final_table, "Final_Accuracy_Stats.csv")

if(length(plots_list) > 0) {
  g <- do.call("grid.arrange", c(plots_list, ncol=2))
  ggsave("Final_ScatterPlots.png", g, width=14, height=10)
  message("✅ DONE! Check 'Final_Accuracy_Stats.csv' and 'Final_ScatterPlots.png'")
}
what wrong of the codes and how to solve irt

# ==============================================================================
# ANALYSIS 5 ALTERNATIVE: OVERLAY SCATTER PLOT
# Comparison: TROPOMI (X) vs GOSAT & GOSAT-2 (Y)
# ==============================================================================

# 1. PREPARE DATA PAIRS
# We need to create two separate matched datasets and combine them
# Pair A: TROPOMI vs GOSAT
# Pair B: TROPOMI vs GOSAT-2

prepare_pairs <- function(df_ref, df_comp, ref_name, comp_name) {
  # Spatial Match (Same as before)
  st_ref <- st_as_sf(df_ref, coords=c("Lon","Lat"), crs=4326)
  st_comp <- st_as_sf(df_comp, coords=c("Lon","Lat"), crs=4326)
  
  common_days <- intersect(unique(df_ref$Day), unique(df_comp$Day))
  matched_data <- data.frame()
  
  for(d in common_days) {
    day_val <- as.Date(d, origin="1970-01-01")
    s_ref_d <- st_ref %>% filter(Day == day_val)
    s_comp_d <- st_comp %>% filter(Day == day_val)
    
    if(nrow(s_ref_d)>0 & nrow(s_comp_d)>0) {
      matches <- st_join(s_comp_d, s_ref_d, join=st_is_within_distance, dist=10000, left=FALSE)
      if(nrow(matches)>0) {
        clean <- matches %>% st_drop_geometry() %>% 
          select(TROPOMI_Val = Value.y, Compare_Val = Value.x) %>%
          mutate(Satellite = comp_name) # Label this row
        matched_data <- rbind(matched_data, clean)
      }
    }
  }
  return(matched_data)
}

message("Matching TROPOMI vs GOSAT...")
pair_1 <- prepare_pairs(t19, g19, "TROPOMI", "GOSAT")

message("Matching TROPOMI vs GOSAT-2...")
pair_2 <- prepare_pairs(t19, g219, "TROPOMI", "GOSAT-2")

# 2. COMBINE BOTH PAIRS
if(nrow(pair_1) > 0 & nrow(pair_2) > 0) {
  combined_pairs <- bind_rows(pair_1, pair_2)
  
  # 3. PLOT
  p_scatter_3way <- ggplot(combined_pairs, aes(x=TROPOMI_Val, y=Compare_Val, color=Satellite)) +
    # Add points
    geom_point(alpha=0.6, size=2) +
    # Add regression lines for each satellite
    geom_smooth(method="lm", se=FALSE, size=1) +
    # Add 1:1 Reference Line (Perfect Agreement)
    geom_abline(slope=1, intercept=0, linetype="dashed", color="black") +
    scale_color_manual(values=c("GOSAT"="red", "GOSAT-2"="blue")) +
    labs(
      title = "Multi-Satellite Correlation (2019)",
      subtitle = "Reference: TROPOMI (X-axis)",
      x = "TROPOMI Methane (ppb)",
      y = "GOSAT / GOSAT-2 Methane (ppb)"
    ) +
    theme_bw() +
    theme(legend.position = "bottom")
  
  print(p_scatter_3way)
  ggsave("Final_Scatter_Analysis5_Overlay.png", p_scatter_3way, width=8, height=6)
  message("✅ Scatter Plot Created: Final_Scatter_Analysis5_Overlay.png")
  
} else {
  message("❌ Not enough matching points found to create the overlay scatter plot.")
}
