# ==============================================================================
# OBJECTIVE 3: PERFORMANCE CHECK (Observation Density & Cloud Impact)
# ==============================================================================

# 1. SETUP
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, readxl, lubridate, knitr)

# 2. DEFINE EXACT PATHS
path_trop_19  <- "D:/TROPOMI data/2019_TROPOMI_Methane Concentration_excel/TROPOMI_2019_Whole_Malaysia.xlsx"
path_trop_24  <- "D:/TROPOMI data/2024_TROPOMI_Methane Concentration_excel/TROPOMI_2024_Whole_Malaysia.xlsx"
path_gosat_19 <- "D:/GOSAT data/2019_GOSAT_Methane Concentration_excel/GOSAT_2019_Whole_Malaysia.xlsx"
path_g2_19    <- "D:/GOSAT 2 data/2019_GOSAT2_Methane Concentration_excel/GOSAT2_2019_Malaysia_Whole.xlsx"
path_g2_24    <- "D:/GOSAT 2 data/2024_GOSAT2_Methane Concentration_excel/GOSAT2_2024_Malaysia_Whole.xlsx"

# 3. DATA COUNTER FUNCTION
count_observations <- function(path, sat_name) {
  if(!file.exists(path)) { message("❌ Missing: ", path); return(NULL) }
  
  if(grepl("xlsx$", path)) df <- read_excel(path) else df <- read_csv(path, show_col_types=F)
  names(df) <- tolower(names(df))
  
  # Normalize Date Column
  if("date" %in% names(df)) df <- df %>% rename(Date_Col = date)
  else if("time" %in% names(df)) df <- df %>% rename(Date_Col = time)
  else if("time_readable" %in% names(df)) df <- df %>% rename(Date_Col = time_readable)
  else return(NULL)
  
  df %>%
    mutate(
      Date = as_datetime(Date_Col),
      Month_Num = month(Date),
      Month_Name = month(Date, label = TRUE, abbr = TRUE)
    ) %>%
    group_by(Month_Num, Month_Name) %>%
    summarise(Valid_Points = n(), .groups = "drop") %>%
    mutate(Satellite = sat_name)
}

# 4. RUN CALCULATIONS
message("Calculating Data Density...")
res1 <- count_observations(path_trop_19, "TROPOMI (2019)")
res2 <- count_observations(path_trop_24, "TROPOMI (2024)")
res3 <- count_observations(path_gosat_19, "GOSAT (2019)")
res4 <- count_observations(path_g2_19, "GOSAT-2 (2019)")
res5 <- count_observations(path_g2_24, "GOSAT-2 (2024)")

all_counts <- bind_rows(res1, res2, res3, res4, res5)

# 5. PERFORM "GAP ANALYSIS" (Wet vs Dry Season)
# Dry Season = June, July, August (Typically clearest)
# Wet Season = Nov, Dec, Jan (Monsoon)

gap_analysis <- all_counts %>%
  mutate(Season = case_when(
    Month_Num %in% c(6, 7, 8) ~ "Dry_Season_Avg",
    Month_Num %in% c(11, 12, 1) ~ "Wet_Season_Avg",
    TRUE ~ "Transition"
  )) %>%
  filter(Season != "Transition") %>%
  group_by(Satellite, Season) %>%
  summarise(Avg_Points = mean(Valid_Points), .groups="drop") %>%
  pivot_wider(names_from = Season, values_from = Avg_Points) %>%
  mutate(
    Data_Loss_Percentage = round((1 - (Wet_Season_Avg / Dry_Season_Avg)) * 100, 1)
  )

# 6. OUTPUT RESULTS
print("--- MONTHLY OBSERVATION COUNTS ---")
print(kable(all_counts))

print("--- MONSOON IMPACT (DATA LOSS %) ---")
print(kable(gap_analysis))

# Save for Report
write_csv(all_counts, "Objective3_Monthly_Counts.csv")
write_csv(gap_analysis, "Objective3_Gap_Analysis.csv")

# 7. VISUALIZATION (Bar Chart)
ggplot(all_counts, aes(x = Month_Name, y = Valid_Points, fill = Satellite)) +
  geom_col() +
  facet_wrap(~Satellite, scales = "free_y", ncol = 2) +
  theme_bw() +
  labs(
    title = "Satellite Observation Density in Malaysia",
    subtitle = "Impact of Monsoon Season on Data Availability",
    y = "Number of Valid Data Points",
    x = "Month"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("Objective3_Data_Availability.png", width = 10, height = 8)
message("✅ Objective 3 Complete. Check 'Objective3_Gap_Analysis.csv' for your answers.")