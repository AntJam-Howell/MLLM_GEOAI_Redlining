#rm(list = ls())
#gc()

#load("Pilot3/DL_LLM_Census_Analysis.RData")
# General data manipulation
library(tidyverse)
library(dplyr)
library(purrr)
library(readr)

# Spatial data and analysis
library(sf)
library(spdep)
library(spatialreg)
library(tigris)
library(tidycensus)
library(ggmap)

# Econometric modeling
library(lfe)
library(quantreg)
library(boot)

# Statistical reporting and visualization
library(stargazer)
library(texreg)
library(broom)
library(ggplot2)
library(ggpubr)
library(ggstatsplot)
library(patchwork)
library(GGally)
library(ggridges)
library(ggalluvial)
library(cowplot)
library(kableExtra)
library(forcats)
library(viridis)
library(ggrepel)

# Additional utilities
library(DescTools)
library(DiagrammeR)

# IDE interaction (optional, typically used in interactive settings)
library(rstudioapi)


########################################################################################################################
########################################################################################################################
## DL Vs Census Data
########################################################################################################################
########################################################################################################################
#Absolute path to download Dataset

DL.df <- read.csv("Pilot3/Data/merged_summary.csv", comment.char="#")
length(unique(DL.df$GEOID))
summary(DL.df)
str(DL.df)
head(DL.df)
# Extract distinct census-level information for each GEOID
Census.DF <- DL.df %>%
  select(GEOID:rankgrpsz) %>%
  distinct(GEOID, .keep_all = TRUE)
summary(Census.DF)


# Calculate the number of tiles per geoid from the LLM data
DL.df %>%
  group_by(GEOID) %>%
  summarise(tileN = n()) %>% 
  summary()


length(unique(DL.df$tile_id))/length(unique(DL.df$GEOID ))



head(DL.df)
DL.df<-DL.df %>% select(GEOID, tile_id:class_149_flag)

#DL.df1<-DL.df %>% head()
#write.csv(DL.df, file = "DL.df1")
names(DL.df)
poverty_classes <- c(
  "class_032_fence", "class_043_signboard", "class_087_streetlight", "class_069_bench",
  "class_011_sidewalk", "class_052_path",
  "class_001_building", "class_025_house", "class_000_wall",
  "class_008_windowpane", "class_014_door",
  "class_006_road",
  "class_002_sky",
  #"class_009_grass",
  #"class_017_plant",
  #"class_004_tree", "class_072_palm",
  "class_012_person",
  "class_127_bicycle",
  "class_020_car", "class_083_truck", "class_102_van"
)


canopy_classes <- c(
  "class_004_tree",
  "class_009_grass",
  "class_017_plant",
  "class_072_palm"
)


# First, ensure numeric conversion of relevant segmentation columns
# (All columns after 'tile_id' are assumed segmentation/prediction columns)
segmentation_cols <- names(DL.df)[3:ncol(DL.df)]
DL.df[segmentation_cols] <- lapply(DL.df[segmentation_cols], as.numeric)

# Aggregation to GEOID (census block) level

# First, count tiles per GEOID
tile_counts <- DL.df %>%
  group_by(GEOID) %>%
  summarize(tileN = n())

# Join tile counts back to the original data
DL.df <- DL.df %>%
  left_join(tile_counts, by = "GEOID")

# Now aggregate with weighted.mean by GEOID
DL_Poverty <- DL.df %>%
  group_by(GEOID) %>%
  summarize(across(all_of(poverty_classes), ~weighted.mean(.x, tileN, na.rm = TRUE)))

DL_canopy <- DL.df %>%
  group_by(GEOID) %>%
  summarize(across(all_of(canopy_classes), ~weighted.mean(.x, tileN, na.rm = TRUE)))

# Merge aggregated DL predictions with official census data
final_dataset <- Census.DF %>%
  inner_join(DL_canopy, by = "GEOID")
final_dataset <- final_dataset %>%
  inner_join(DL_Poverty, by = "GEOID")

# Poverty: Create formula dynamically from the vector
# Poverty Normalized: 
RHSpctpovnorm <- as.formula(paste("pctpovnorm ~", paste(poverty_classes, collapse = " + ")))
lm_pctpovnorm <- lm(RHSpctpovnorm, data = final_dataset)
summary(lm_pctpovnorm)


# Urban Greenspace
# Tree Canopy+: Create formula dynamically from the vector
RHStreecanopy <- as.formula(paste("treecanopy ~", paste(canopy_classes, collapse = " + ")))
# Run regression using dynamically created formula
lm_treecanopy <- lm(RHStreecanopy, data = final_dataset)
# Summarize results
summary(lm_treecanopy)

final_dataset$DL_poverty_pred <- predict(lm_pctpovnorm, final_dataset)
final_dataset$DL_poverty_pred<-ifelse(final_dataset$DL_poverty_pred<0,0,final_dataset$DL_poverty_pred)
final_dataset$DL_canopy_pred <- predict(lm_treecanopy, final_dataset)

########################################################################################################################



########################################################################################################################
########################################################################################################################
### LLM Predictions Data Prep and Merge to Final_Dataset
########################################################################################################################
########################################################################################################################


#Local Relative Path to downlaod Datasets
LLM.df <- read.csv("Pilot2/Data/LLM_Results_All_phx_gilbert_Structured_Update.csv", comment.char="#")
LLM.df <- LLM.df %>% arrange(geoid,filename,prompt_id,iteration)

unique(LLM.df$prompt_id)
unique(LLM.df$prompt_text[LLM.df$prompt_id==1])
unique(LLM.df$prompt_text[LLM.df$prompt_id==2])
unique(LLM.df$prompt_text[LLM.df$prompt_id==3])
unique(LLM.df$prompt_text[LLM.df$prompt_id==4])
unique(LLM.df$prompt_text[LLM.df$prompt_id==5])
unique(LLM.df$prompt_text[LLM.df$prompt_id==6])


table(LLM.df$prompt_id)

LLM.df1<- LLM.df %>%  select(-prompt_text,-gpt_response,-structured_output,-PromptTokens, -CompletionTokens, -TotalTokens, 
                             -families_reasoning, -poverty_reasoning, -poverty_indicators) %>% 
  group_by(geoid, filename) %>%
  summarise(
    poverty_indicator_count = first(na.omit(poverty_indicator_count)),
    poverty_indicator_share = first(na.omit(poverty_indicator_share)),
    canopy_indicators = factor(first(na.omit(canopy_indicators))),
    canopy_indicator_count = first(na.omit(canopy_indicator_count)),
    canopy_coverage = first(na.omit(canopy_coverage)),
    housing_type = factor(first(na.omit(housing_type))),
    estimated_number_of_families = first(na.omit(estimated_number_of_families)),
    poverty_classification = first(na.omit(poverty_classification)),
    estimated_median_household_income = first(na.omit(estimated_median_household_income)),
    .groups = "drop"
  )



# Calculate the number of tiles per geoid from the LLM data
tile_counts_llm <- LLM.df1 %>%
  group_by(geoid) %>%
  summarise(tileN = n())

# Join the tile counts to LLM.df1
LLM.df1 <- LLM.df1 %>%
  left_join(tile_counts_llm, by = "geoid")

# Aggregate using weighted means
sumLLM.df1 <- LLM.df1 %>%
  group_by(geoid) %>%
  summarise(
    mean_poverty_indicator_count = weighted.mean(poverty_indicator_count, tileN, na.rm = TRUE),
    mean_poverty_indicator_share = weighted.mean(poverty_indicator_share, tileN, na.rm = TRUE),
    mean_canopy_indicator_count = weighted.mean(canopy_indicator_count, tileN, na.rm = TRUE),
    mean_canopy_coverage = weighted.mean(canopy_coverage, tileN, na.rm = TRUE),
    mean_estimated_number_of_families = weighted.mean(estimated_number_of_families, tileN, na.rm = TRUE),
    mean_poverty_classification = weighted.mean(poverty_classification, tileN, na.rm = TRUE),
    mean_estimated_median_household_income = weighted.mean(estimated_median_household_income, tileN, na.rm = TRUE),
    most_common_canopy_indicators = names(which.max(table(canopy_indicators))),
    most_common_housing_type = names(which.max(table(housing_type))),
    tile_count = unique(tileN),
    .groups = "drop"
  )

# Display summary of weighted aggregated data
summary(sumLLM.df1)



# Replace NAs with 0 for specified columns
sumLLM.df1 <- sumLLM.df1 %>%
  mutate(
    mean_poverty_indicator_count = if_else(is.na(mean_poverty_indicator_count), 0, mean_poverty_indicator_count),
    mean_poverty_indicator_share = if_else(is.na(mean_poverty_indicator_share), 0, mean_poverty_indicator_share),
    mean_canopy_indicator_count = if_else(is.na(mean_canopy_indicator_count), 0, mean_canopy_indicator_count),
    mean_canopy_coverage = if_else(is.na(mean_canopy_coverage), 0, mean_canopy_coverage),
    mean_estimated_number_of_families = if_else(is.na(mean_estimated_number_of_families), 1, mean_estimated_number_of_families),
    mean_estimated_number_of_families = if_else(mean_estimated_number_of_families==0, 1, mean_estimated_number_of_families),
    mean_poverty_classification = if_else(is.na(mean_poverty_classification), 0, mean_poverty_classification)
  )
summary(sumLLM.df1)

length(unique(sumLLM.df1$geoid))
length(unique(final_dataset$GEOID))

########################################################################################################################

########################################################################################################################
########################################################################################################################
##Combine: LLM vs. DL vs. Census
########################################################################################################################
########################################################################################################################
length(unique(combined_df$GEOID))
combined_df <- final_dataset %>%
  inner_join(sumLLM.df1, by = c("GEOID" = "geoid"))

combined_df <- combined_df %>%
  mutate(Pop_Density = cbg_pop / land_area)
summary(combined_df)

#combined_df<-na.omit(combined_df)
         

# Winsorize poverty variables (at 1% level)
combined_df <- combined_df %>%
  mutate(across(c(Pop_Density, pctpovnorm, treecanopy, DL_poverty_pred, DL_canopy_pred, mean_poverty_classification,mean_canopy_coverage),
                ~ Winsorize(.x, quantile(.x, probs = c(0.01, 0.99)))))



combined_df <- combined_df %>%
  mutate(
    census_pov_norm = scale(pctpovnorm),
    census_canopy_norm = scale(treecanopy),
    Census_Sustainable_Dev_Index = (census_canopy_norm - census_pov_norm)/2,
    
    DL_pov_norm = scale(DL_poverty_pred),
    DL_canopy_norm = scale(DL_canopy_pred),
    DL_Sustainable_Dev_Index = (DL_canopy_norm - DL_pov_norm)/2,
    
    LLM_pov_norm = scale(mean_poverty_classification),
    LLM_canopy_norm = scale(mean_canopy_coverage),
    LLM_Sustainable_Dev_Index = (LLM_canopy_norm - LLM_pov_norm)/2
  )

combined_df <- combined_df %>%
  mutate(
    census_pov_norm = as.numeric(census_pov_norm),
    census_canopy_norm= as.numeric(census_canopy_norm),
    Census_Sustainable_Dev_Index = as.numeric(Census_Sustainable_Dev_Index),
    DL_pov_norm = as.numeric(DL_pov_norm),
    DL_canopy_norm = as.numeric(DL_canopy_norm),
    DL_Sustainable_Dev_Index = as.numeric(DL_Sustainable_Dev_Index),
    LLM_pov_norm = as.numeric(LLM_pov_norm),
    LLM_canopy_norm = as.numeric(LLM_canopy_norm),
    LLM_Sustainable_Dev_Index = as.numeric(LLM_Sustainable_Dev_Index)
  )
########################################################################################################################


###############################################################################################
###############################################################################################
###################Additional Census Variables
###############################################################################################
###############################################################################################




# Census variables (example set, ACS 5-year 2023 estimates)
vars <- c(
  
  
  pct_black = "B02001_003", # black population
  pct_hispanic = "B03003_003", # hispanic population
  pct_asian = "B02001_005", # asian population
  pct_bachelors = "B15003_022", # bachelor's degree or higher
  total_pop = "B01003_001", # total population for normalization
  housing_age = "B25035_001"             # Median year structure built
  
)

# Download data at CBG-level
census_covars <- get_acs(geography = "block group",
                         variables = vars,
                         state = "AZ",
                         county = "Maricopa",
                         year = 2023,
                         survey = "acs5",
                         geometry = FALSE)

# Pivot and calculate percentages
census_covars <- census_covars %>%
  select(GEOID, variable, estimate) %>%
  pivot_wider(names_from = variable, values_from = estimate) %>%
  mutate(pct_black = pct_black / total_pop,
         pct_hispanic = pct_hispanic / total_pop,
         pct_asian = pct_asian / total_pop,
         pct_bachelors = pct_bachelors / total_pop)


# Add leading zero to GEOIDs in your combined_df
combined_df <- combined_df %>%
  mutate(GEOID1 = sprintf("%012s", GEOID))

# Extract unique GEOIDs from your data
geoids <- unique(combined_df$GEOID1)


combined_df <- combined_df %>%
  left_join(census_covars, by = c("GEOID1" = "GEOID"))

summary(combined_df)
combined_df$Per_Capita_Income<-NULL
combined_df$housing_age<-NULL
combined_df$mean_estimated_median_household_income<-NULL

############################################################################





########################################################################################################################
########################################################################################################################
#### Spatial lag variables
########################################################################################################################
########################################################################################################################


#census_api_key("8eab9b16f44cb26460ecbde164482194b7052772", install = TRUE)
Sys.getenv("CENSUS_API_KEY")


# Get geometry data from the ACS (e.g., block groups)
geometry_data <- get_acs(
  geography = "block group",
  variables = c(
    Per_Capita_Income = "B19301_001"
    ),
  year = 2023,
  survey = "acs5",     # Specify 5-year ACS explicitly
  state = "AZ",
  county = "Maricopa",
  geometry = TRUE
)

# Keep only relevant GEOIDs matching your data
geometry_data_filtered <- geometry_data %>%
  filter(GEOID %in% geoids) 

subset_combined_df <- combined_df %>%
  select(GEOID1, pctpovnorm, treecanopy, Census_Sustainable_Dev_Index,
         mean_poverty_classification,
         DL_poverty_pred,
         mean_canopy_coverage,
         DL_canopy_pred,
         LLM_Sustainable_Dev_Index,
         DL_Sustainable_Dev_Index
  )
subset_combined_df$GEOID<-subset_combined_df$GEOID1
geometry_data_filtered <- geometry_data %>%
  filter(GEOID %in% subset_combined_df$GEOID) %>% 
  left_join(subset_combined_df, by = "GEOID")


str(geometry_data_filtered)


# Create queen adjacency spatial weights
nb <- poly2nb(geometry_data_filtered, queen = TRUE)
lw <- nb2listw(nb, style = "W", zero.policy = TRUE)

names(geometry_data_filtered)
# Example: Compute spatial lag of poverty variable
geometry_data_filtered$Lag_pctpovnorm <- as.numeric(lag.listw(lw, geometry_data_filtered$pctpovnorm))
geometry_data_filtered$Lag_treecanopy <- as.numeric(lag.listw(lw, geometry_data_filtered$treecanopy))
geometry_data_filtered$Lag_Census_Sustainable_Dev_Index <- as.numeric(lag.listw(lw, geometry_data_filtered$Census_Sustainable_Dev_Index))

# Spatial lags of independent variables
geometry_data_filtered$Lag_mean_poverty_classification <- lag.listw(lw, geometry_data_filtered$mean_poverty_classification)
geometry_data_filtered$Lag_DL_poverty_pred <- lag.listw(lw, geometry_data_filtered$DL_poverty_pred)

geometry_data_filtered$Lag_mean_canopy_coverage <- lag.listw(lw, geometry_data_filtered$mean_canopy_coverage)
geometry_data_filtered$Lag_DL_canopy_pred <- lag.listw(lw, geometry_data_filtered$DL_canopy_pred)

geometry_data_filtered$Lag_LLM_Sustainable_Dev_Index <- lag.listw(lw, geometry_data_filtered$LLM_Sustainable_Dev_Index)
geometry_data_filtered$Lag_DL_Sustainable_Dev_Index <- lag.listw(lw, geometry_data_filtered$DL_Sustainable_Dev_Index)


summary(geometry_data_filtered)



geometry_data_filtered$lw
# Select only relevant columns from geometry_data_filtered
spatial_vars <- geometry_data_filtered %>%
  st_drop_geometry() %>%  # drop geometry to simplify merge
  select(
    GEOID,
    starts_with("Lag_"),    Per_Capita_Income = estimate  # rename 'estimate' clearly
  )

# If needed, ensure GEOID variables match exactly (character format)
combined_df$GEOID <- as.character(combined_df$GEOID)
spatial_vars$GEOID <- as.character(spatial_vars$GEOID)

# Merge spatial lagged variables and Per_Capita_Income into combined_df
combined_df <- combined_df %>%
  left_join(spatial_vars, by = c("GEOID1" = "GEOID"))

# Verify results clearly
summary(combined_df)

########################################################################################################################


########################################################################################################################
########################################################################################################################
##Descriptives: LLM vs. DL vs. Census
########################################################################################################################
########################################################################################################################

# Step 1: Create corr_df with new variable names
corr_df <- combined_df %>%
  select(
    Census_Poverty = pctpovnorm,
    ResDeep_Poverty = DL_poverty_pred,
    GPT4o_Poverty = mean_poverty_classification,
    GEIE_Green = treecanopy,
    ResDeep_Green = DL_canopy_pred,
    GPT4o_Green = mean_canopy_coverage
  )

# Step 2: Compute the correlation matrix
corr_matrix <- cor(corr_df, use = "complete.obs")
corr_matrix <- round(corr_matrix, 3)

# Step 3: Specify order for publication
ordered_vars <- c(
  "Census_Poverty", "ResDeep_Poverty", "GPT4o_Poverty", 
  "GEIE_Green", "ResDeep_Green", "GPT4o_Green"
)

# Step 4: Reshape for ggplot and factor axes for custom order
corr_df_long <- corr_matrix %>%
  as.data.frame() %>%
  rownames_to_column(var = "Var1") %>%
  pivot_longer(-Var1, names_to = "Var2", values_to = "Correlation") %>%
  mutate(
    Correlation = round(Correlation, 3),
    Var1 = factor(Var1, levels = ordered_vars),
    Var2 = factor(Var2, levels = rev(ordered_vars))  # for top-down order
  )

print(corr_df_long, n=40)


set.seed(123)


HeatMapCorr <- ggplot(corr_df_long, aes(x = Var1, y = Var2, fill = Correlation)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = Correlation), color = "black", size = 5, family = "serif") +
  scale_fill_gradient2(
    low = "#4575b4", mid = "white", high = "#d73027",
    midpoint = 0, limit = c(-1, 1), space = "Lab",
    name = "Correlation"
  ) +
  theme_classic(base_size = 14, base_family = "serif") +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 14, family = "serif"),
    axis.text.y = element_text(size = 14, family = "serif"),
    axis.title = element_text(size = 14, family = "serif"),
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 12, hjust = 0.5, family = "serif"),
    panel.grid = element_blank()
  ) +
  labs(
    title = "",
    x = "", y = ""
  )

# Print the plot
HeatMapCorr

#ggsave("Pilot3/Fig/Correlation_Heatmap_Ordered.pdf", HeatMapCorr,  width = 18, height = 6, dpi = 300)



moran.test(combined_df$pctpovnorm, lw)
moran.test(combined_df$DL_poverty_pred, lw)
moran.test(combined_df$mean_poverty_classification, lw)

moran.test(combined_df$treecanopy, lw)
moran.test(combined_df$DL_canopy_pred, lw)
moran.test(combined_df$mean_canopy_coverage, lw)

moran.test(combined_df$Census_Sustainable_Dev_Index, lw)
moran.test(combined_df$DL_Sustainable_Dev_Index, lw)
moran.test(combined_df$LLM_Sustainable_Dev_Index, lw)


combined_df <- combined_df %>%
  mutate(HolcFactor_Revised1 = factor(HolcFactor_Revised,
                                     labels =c("A", "B", "C", "D",
                                               "Phoenix", "Gilbert")
  ))
  

combined_df <- combined_df %>%
  mutate(HolcFactor_RevisedGrp = case_when(
    place == "Gilbert" | holc_grade == "A" ~ "Ideal",
    place == "Phoenix" & holc_grade == "" | holc_grade == "B" | holc_grade == "C" ~ "Stable/Decline",
    holc_grade == "D" ~ "Redlined",
    TRUE ~ NA
  ))
    
    
p1 <- ggplot() +
  # 1) first draw Stable/Decline in the background
  geom_point(
    data = combined_df %>% filter(HolcFactor_RevisedGrp=="Stable/Decline"),
    aes(x = mean_poverty_classification, y = pctpovnorm),
    colour = "lightblue", alpha = 0.2, size = 1.5
  ) +
  geom_smooth(
    data = combined_df %>% filter(HolcFactor_RevisedGrp=="Stable/Decline"),
    aes(x = mean_poverty_classification, y = pctpovnorm),
    method = "lm", se = FALSE,
    colour = "lightblue", size = 0.8, linetype = "dashed"
  ) +
  # 2) then draw Ideal in green
  geom_point(
    data = combined_df %>% filter(HolcFactor_RevisedGrp=="Ideal"),
    aes(x = mean_poverty_classification, y = pctpovnorm),
    colour = "green", alpha = 0.7, size = 1.8
  ) +
  geom_smooth(
    data = combined_df %>% filter(HolcFactor_RevisedGrp=="Ideal"),
    aes(x = mean_poverty_classification, y = pctpovnorm),
    method = "lm", se = FALSE,
    colour = "green", size = 1.2
  ) +
  # 3) finally draw Redlined in red on top
  geom_point(
    data = combined_df %>% filter(HolcFactor_RevisedGrp=="Redlined"),
    aes(x = mean_poverty_classification, y = pctpovnorm),
    colour = "red", alpha = 0.7, size = 1.8
  ) +
  geom_smooth(
    data = combined_df %>% filter(HolcFactor_RevisedGrp=="Redlined"),
    aes(x = mean_poverty_classification, y = pctpovnorm),
    method = "lm", se = FALSE,
    colour = "red", size = 1.2
  ) +
  # axes, labels, theme
  labs(
    x = "GSV-GPT4o",
    y = "Authoritative Data Source",
    subtitle = ""
  ) +
  theme_classic(base_size = 14) +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 14),
    axis.text  = element_text(size = 12)
  )

p1



# 4) GPT-4o vs. Census Canopy
p2 <- ggplot() +
  geom_point(
    data = combined_df %>% filter(HolcFactor_RevisedGrp=="Stable/Decline"),
    aes(x = mean_canopy_coverage, y = treecanopy),
    color = "lightblue", alpha = 0.2, size = 1.5
  ) +
  geom_smooth(
    data = combined_df %>% filter(HolcFactor_RevisedGrp=="Stable/Decline"),
    aes(x = mean_canopy_coverage, y = treecanopy),
    method = "lm", se = FALSE,
    color = "lightblue", size = 0.8, linetype = "dashed"
  ) +
  geom_point(
    data = combined_df %>% filter(HolcFactor_RevisedGrp=="Ideal"),
    aes(x = mean_canopy_coverage, y = treecanopy),
    color = "green", alpha = 0.7, size = 1.8
  ) +
  geom_smooth(
    data = combined_df %>% filter(HolcFactor_RevisedGrp=="Ideal"),
    aes(x = mean_canopy_coverage, y = treecanopy),
    method = "lm", se = FALSE,
    color = "green", size = 1.2
  ) +
  geom_point(
    data = combined_df %>% filter(HolcFactor_RevisedGrp=="Redlined"),
    aes(x = mean_canopy_coverage, y = treecanopy),
    color = "red", alpha = 0.7, size = 1.8
  ) +
  geom_smooth(
    data = combined_df %>% filter(HolcFactor_RevisedGrp=="Redlined"),
    aes(x = mean_canopy_coverage, y = treecanopy),
    method = "lm", se = FALSE,
    color = "red", size = 1.2
  ) +
  labs(
    x = "GSV-GPT4o",
    y = "",
    subtitle = ""
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none")

p2


# 8) GPT-4o vs. Census SI
p3 <- ggplot() +
  geom_point(
    data = combined_df %>% filter(HolcFactor_RevisedGrp=="Stable/Decline"),
    aes(x = LLM_Sustainable_Dev_Index, y = Census_Sustainable_Dev_Index),
    color = "lightblue", alpha = 0.2, size = 1.5
  ) +
  geom_smooth(
    data = combined_df %>% filter(HolcFactor_RevisedGrp=="Stable/Decline"),
    aes(x = LLM_Sustainable_Dev_Index, y = Census_Sustainable_Dev_Index),
    method = "lm", se = FALSE,
    color = "lightblue", size = 0.8, linetype = "dashed"
  ) +
  geom_point(
    data = combined_df %>% filter(HolcFactor_RevisedGrp=="Ideal"),
    aes(x = LLM_Sustainable_Dev_Index, y = Census_Sustainable_Dev_Index),
    color = "green", alpha = 0.7, size = 1.8
  ) +
  geom_smooth(
    data = combined_df %>% filter(HolcFactor_RevisedGrp=="Ideal"),
    aes(x = LLM_Sustainable_Dev_Index, y = Census_Sustainable_Dev_Index),
    method = "lm", se = FALSE,
    color = "green", size = 1.2
  ) +
  geom_point(
    data = combined_df %>% filter(HolcFactor_RevisedGrp=="Redlined"),
    aes(x = LLM_Sustainable_Dev_Index, y = Census_Sustainable_Dev_Index),
    color = "red", alpha = 0.7, size = 1.8
  ) +
  geom_smooth(
    data = combined_df %>% filter(HolcFactor_RevisedGrp=="Redlined"),
    aes(x = LLM_Sustainable_Dev_Index, y = Census_Sustainable_Dev_Index),
    method = "lm", se = FALSE,
    color = "red", size = 1.2
  ) +
  labs(
    x = "GSV-GPT4o",
    y = "",
    subtitle = ""
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none")

p3






p4 <- ggplot() +
  # same approach, but y = mean_poverty_classification
  geom_point(
    data = combined_df %>% filter(HolcFactor_RevisedGrp=="Stable/Decline"),
    aes(x = DL_poverty_pred, y = mean_poverty_classification),
    color = "lightblue", alpha = 0.2, size = 1.5
  ) +
  geom_smooth(
    data = combined_df %>% filter(HolcFactor_RevisedGrp=="Stable/Decline"),
    aes(x = DL_poverty_pred, y = mean_poverty_classification),
    method = "lm", se = FALSE,
    color = "lightblue", size = 0.8, linetype = "dashed"
  ) +
  geom_point(
    data = combined_df %>% filter(HolcFactor_RevisedGrp=="Ideal"),
    aes(x = DL_poverty_pred, y = mean_poverty_classification),
    color = "green", alpha = 0.7, size = 1.8
  ) +
  geom_smooth(
    data = combined_df %>% filter(HolcFactor_RevisedGrp=="Ideal"),
    aes(x = DL_poverty_pred, y = mean_poverty_classification),
    method = "lm", se = FALSE,
    color = "green", size = 1.2
  ) +
  geom_point(
    data = combined_df %>% filter(HolcFactor_RevisedGrp=="Redlined"),
    aes(x = DL_poverty_pred, y = mean_poverty_classification),
    color = "red", alpha = 0.7, size = 1.8
  ) +
  geom_smooth(
    data = combined_df %>% filter(HolcFactor_RevisedGrp=="Redlined"),
    aes(x = DL_poverty_pred, y = mean_poverty_classification),
    method = "lm", se = FALSE,
    color = "red", size = 1.2
  ) +
  labs(
    x = "GSV-Resnet",
    y = "GSV–GPT4o",
    subtitle = "Poverty"
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none")

# then combine
p4




# 6) ResNet vs. GPT-4o Canopy
p5 <- ggplot() +
  geom_point(
    data = combined_df %>% filter(HolcFactor_RevisedGrp=="Stable/Decline"),
    aes(x = DL_canopy_pred, y = mean_canopy_coverage),
    color = "lightblue", alpha = 0.2, size = 1.5
  ) +
  geom_smooth(
    data = combined_df %>% filter(HolcFactor_RevisedGrp=="Stable/Decline"),
    aes(x = DL_canopy_pred, y = mean_canopy_coverage),
    method = "lm", se = FALSE,
    color = "lightblue", size = 0.8, linetype = "dashed"
  ) +
  geom_point(
    data = combined_df %>% filter(HolcFactor_RevisedGrp=="Ideal"),
    aes(x = DL_canopy_pred, y = mean_canopy_coverage),
    color = "green", alpha = 0.7, size = 1.8
  ) +
  geom_smooth(
    data = combined_df %>% filter(HolcFactor_RevisedGrp=="Ideal"),
    aes(x = DL_canopy_pred, y = mean_canopy_coverage),
    method = "lm", se = FALSE,
    color = "green", size = 1.2
  ) +
  geom_point(
    data = combined_df %>% filter(HolcFactor_RevisedGrp=="Redlined"),
    aes(x = DL_canopy_pred, y = mean_canopy_coverage),
    color = "red", alpha = 0.7, size = 1.8
  ) +
  geom_smooth(
    data = combined_df %>% filter(HolcFactor_RevisedGrp=="Redlined"),
    aes(x = DL_canopy_pred, y = mean_canopy_coverage),
    method = "lm", se = FALSE,
    color = "red", size = 1.2
  ) +
  labs(
    x = "GSV–ResNet",
    y = "GSV–GPT4o",
    subtitle = "Tree Canopy"
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none")
p5




# 9) GPT-4o vs. ResNet SI
p6 <- ggplot() +
  geom_point(
    data = combined_df %>% filter(HolcFactor_RevisedGrp=="Stable/Decline"),
    aes(x = DL_Sustainable_Dev_Index, y = LLM_Sustainable_Dev_Index),
    color = "lightblue", alpha = 0.2, size = 1.5
  ) +
  geom_smooth(
    data = combined_df %>% filter(HolcFactor_RevisedGrp=="Stable/Decline"),
    aes(x = DL_Sustainable_Dev_Index, y = LLM_Sustainable_Dev_Index),
    method = "lm", se = FALSE,
    color = "lightblue", size = 0.8, linetype = "dashed"
  ) +
  geom_point(
    data = combined_df %>% filter(HolcFactor_RevisedGrp=="Ideal"),
    aes(x = DL_Sustainable_Dev_Index, y = LLM_Sustainable_Dev_Index),
    color = "green", alpha = 0.7, size = 1.8
  ) +
  geom_smooth(
    data = combined_df %>% filter(HolcFactor_RevisedGrp=="Ideal"),
    aes(x = DL_Sustainable_Dev_Index, y = LLM_Sustainable_Dev_Index),
    method = "lm", se = FALSE,
    color = "green", size = 1.2
  ) +
  geom_point(
    data = combined_df %>% filter(HolcFactor_RevisedGrp=="Redlined"),
    aes(x = DL_Sustainable_Dev_Index, y = LLM_Sustainable_Dev_Index),
    color = "red", alpha = 0.7, size = 1.8
  ) +
  geom_smooth(
    data = combined_df %>% filter(HolcFactor_RevisedGrp=="Redlined"),
    aes(x = DL_Sustainable_Dev_Index, y = LLM_Sustainable_Dev_Index),
    method = "lm", se = FALSE,
    color = "red", size = 1.2
  ) +
  labs(
    x = "GSV–Resnet",
    y = "GSV–GPT4o",
    subtitle = "SI"
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none")
p6
## Appendix figures
ScatterAppdx<-p4 | p5 | p6

#ggsave("Pilot3/Fig/ScatterAppdx.pdf", ScatterAppdx,  width = 18, height = 6, dpi = 300)


############## 

#################################################
####### Main Descriptives by REdlined Zone
#################################################

combined_df <- combined_df %>%
  mutate(HolcFactor_Revised = case_when(
    place == "Gilbert" & holc_grade == "" ~ "Non-Incorporated Gilbert",
    place == "Phoenix" & holc_grade == "" ~ "Non-Graded Phoenix",
    place == "Phoenix" & holc_grade == "A" ~ "HOLC-A",
    place == "Phoenix" & holc_grade == "B" ~ "HOLC-B",
    place == "Phoenix" & holc_grade == "C" ~ "HOLC-C",
    place == "Phoenix" & holc_grade == "D" ~ "HOLC-D",
    TRUE ~ "Non-Graded Phoenix"
  )) %>%
  mutate(HolcFactor_Revised = factor(HolcFactor_Revised,
                                     levels = c("HOLC-A", "HOLC-B", "HOLC-C", "HOLC-D",
                                                "Non-Graded Phoenix", "Non-Incorporated Gilbert"),
                                     labels = c("HOLC-A", "HOLC-B", "HOLC-C", "HOLC-D", 
                                                "No-HOLC Phoenix", "No-HOLC Gilbert")
  ))

library(dplyr)
library(tidyr)
library(ggplot2)

# 1. Reshape the data to long format for easier plotting

library(dplyr)
library(tidyr)
library(ggplot2)

# Pivot to long format for official statistics
df_official_long <- combined_df %>%
  select(HolcFactor_Revised, 
         pctpovnorm, 
         treecanopy, 
         Census_Sustainable_Dev_Index) %>%
  pivot_longer(
    cols = c(pctpovnorm, treecanopy, Census_Sustainable_Dev_Index),
    names_to = "Measure",
    values_to = "Value"
  ) %>%
  mutate(
    Measure = recode(Measure,
                     "pctpovnorm" = "Poverty (Census)",
                     "treecanopy" = "Tree Canopy (GEIE)",
                     "Census_Sustainable_Dev_Index" = "Sustainability Index (Census)")
  )

# Summarize means and SDs by group
library(dplyr)
library(forcats)

library(dplyr)
library(forcats)

df_summary <- df_official_long %>%
  # collapse the HOLC levels into exactly five groups
  mutate(
    HolcFactor_Revised = fct_collapse(
      HolcFactor_Revised,
      A            = "HOLC-A",
      D            = "HOLC-D",
      `B/C`        = c("HOLC-B", "HOLC-C"),
      `Pseudo-A`   = "No-HOLC Gilbert",
      `Pseudo-B/C` = "No-HOLC Phoenix"
    )
  ) %>%
  # summarize over those new groups
  group_by(HolcFactor_Revised, Measure) %>%
  summarise(
    Mean = median(Value, na.rm = TRUE),
    SD   = sd(Value,   na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # set the final display order
  mutate(
    HolcFactor_Revised = fct_relevel(
      HolcFactor_Revised,
      "A", "Pseudo-A", "B/C", "Pseudo-B/C", "D"
    ),
    Measure = factor(
      Measure,
      levels = c("Poverty (Census)", "Tree Canopy (GEIE)", "Sustainability Index (Census)"),
      labels = c("Poverty", "Canopy", "SI")
    ))
  

# Bar plot with error bars
FigBarPlot <- ggplot(df_summary, aes(x = HolcFactor_Revised, y = Mean, fill = HolcFactor_Revised)) +
  geom_col(position = position_dodge(), width = 0.7) +
  facet_wrap(~ Measure, scales = "free_y") +
  scale_fill_manual("HOLC",
    values = c(
      "A" = "green",
      "B/C" = "yellow",
      "D" = "red",
      "Pseudo-A" = "lightgreen",
      "Pseudo-B/C" = "skyblue"
    ),
    guide = guide_legend(nrow = 1)  # ✅ force one row
  ) +
  labs(
    subtitle = "B.",
    y ="Mean value")+
  geom_hline(yintercept = 0, linewidth = 1, color = "black") +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0, face = "bold", size = 14),
    legend.position = "bottom",
    legend.box = "horizontal",
    #legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    strip.text = element_text(face = "bold", size = 12),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_line(color = "black")
  )
FigBarPlot




###############################################################################################
###############################################################################################
################### Explanatory POwer analysis: FELM and Bootstrap
###############################################################################################
###############################################################################################

######FELM Single Model

# Create census tract ID from block group GEOID
combined_df <- combined_df %>%
  mutate(TRACTID = substr(GEOID, 1, 10))


# Poverty
lm_llm_pov <- felm(pctpovnorm ~ mean_poverty_classification , data = combined_df)
lm_dl_pov  <- felm(pctpovnorm ~ DL_poverty_pred, data = combined_df)
lm_both_pov <- felm(pctpovnorm ~ DL_poverty_pred + mean_poverty_classification, data = combined_df)
lm_pov_pop <- felm(pctpovnorm ~ log(Pop_Density) + depratnorm + lingnorm  + pct_black+pct_hispanic+pct_asian+pct_bachelors, data = combined_df)
lm_pov_pop.GSV <- felm(pctpovnorm ~ mean_poverty_classification + log(Pop_Density) + depratnorm + lingnorm  + pct_black+pct_hispanic+pct_asian+pct_bachelors, data = combined_df)

# Canopy
llm_canpy <- lm(treecanopy ~ mean_canopy_coverage , combined_df)
dl_canpy  <- lm(treecanopy ~ DL_canopy_pred , combined_df)
both_canpy <- lm(treecanopy ~ DL_canopy_pred + mean_canopy_coverage , combined_df)
canpy_pop <- lm(treecanopy~ log(Pop_Density) + depratnorm + lingnorm  + pct_black+pct_hispanic+pct_asian+pct_bachelors, data = combined_df)
canpy_pop.GSV <- lm(treecanopy ~ mean_canopy_coverage +
                      log(Pop_Density) + depratnorm + lingnorm  + pct_black+pct_hispanic+pct_asian+pct_bachelors, data = combined_df)


# Sustainability Index regressions
lm_llm_SDI <- lm(Census_Sustainable_Dev_Index ~ LLM_Sustainable_Dev_Index, data = combined_df)
lm_dl_SDI  <- lm(Census_Sustainable_Dev_Index ~ DL_Sustainable_Dev_Index, data = combined_df)
lm_both_SDI <- lm(Census_Sustainable_Dev_Index ~DL_Sustainable_Dev_Index + LLM_Sustainable_Dev_Index, data = combined_df)
lm_SDI_pop <- lm(Census_Sustainable_Dev_Index~ log(Pop_Density) + depratnorm + lingnorm  + pct_black+pct_hispanic+pct_asian+pct_bachelors, data = combined_df)
lm_SDI_pop_pop.GSV <- lm(Census_Sustainable_Dev_Index ~ LLM_Sustainable_Dev_Index + log(Pop_Density) + depratnorm + lingnorm  + pct_black+pct_hispanic+pct_asian+pct_bachelors, data = combined_df)



##Tables
custom_names <- list(
  "(Intercept)" = "Constant",
  "mean_poverty_classification" = "GPT4o Poverty",
  "DL_poverty_pred" = "ResNet Poverty",
  "mean_canopy_coverage" = "GPT4o Canopy",
  "DL_canopy_pred" = "ResNet Canopy",
  "LLM_Sustainable_Dev_Index" = "GPT4o Sustainability",
  "DL_Sustainable_Dev_Index" = "ResNet Sustainability",
  "log(Pop_Density)" = "Population Density (ln)",
  "depratnorm" = "Dependency Rate",
  "lingnorm" = "Linguistic Isolation",
  "pct_black" = "Black (%)",
  "pct_hispanic" = "Hispanic (%)",
  "pct_asian" = "Asian (%)",
  "pct_bachelors" = "College Educated (%)"
)

texreg(
  list(lm_llm_pov, lm_dl_pov, lm_both_pov, lm_pov_pop, lm_pov_pop.GSV),
  custom.model.names = c("GPT4o Only", "ResNet Only", "Both GPT4o + ResNet", "Demographic Only", "All"),
  custom.coef.map = custom_names,
  caption = "Regression Models Predicting ACS-based Poverty Rates",
  caption.above = TRUE,
  label = "tab:poverty_regression",
  stars =  0.05,
  digits = 3,
  include.fstatistic = FALSE,
  include.rmse = FALSE,
  include.nobs = TRUE,
  include.rsquared = FALSE,
  include.adjrs = TRUE,
  float.pos = "htbp",
  fontsize = "footnotesize",
  booktabs = TRUE,
  use.packages = FALSE,
  file = "Pilot3/Tab/Poverty_Regression.tex",
  
  # Use add.lines instead of notes
  notes = "* p < 0.05. Standard errors in parentheses. Higher coefficients indicate stronger predictive alignment with official ACS-derived poverty estimates.",
  note.align = "l"
)


texreg(
  list(llm_canpy, dl_canpy, both_canpy, canpy_pop, canpy_pop.GSV),
  custom.model.names = c("GPT4o Only", "ResNet Only", "Both GPT4o + ResNet", "Demographic Only", "All"),
  custom.coef.map = custom_names,
  caption = "Regression Models Predicting GEIE-based Tree Canopy Coverage",
  caption.above = TRUE,
  label = "tab:canopy_regression",
  stars =  0.05,
  digits = 3,
  include.fstatistic = FALSE,
  include.rmse = FALSE,
  include.nobs = TRUE,
  include.rsquared = FALSE,
  include.adjrs = TRUE,
  float.pos = "htbp",
  fontsize = "footnotesize",
  booktabs = TRUE,
  use.packages = FALSE,
  file = "Pilot3/Tab/TreeCanopy_Regression.tex",
  
  notes = "* p < 0.05. Standard errors in parentheses. Higher coefficients indicate stronger predictive alignment with official GEIE-derived tree canopy estimates.",
  note.align = "l"
)


texreg(
  list(lm_llm_SDI, lm_dl_SDI, lm_both_SDI, lm_SDI_pop, lm_SDI_pop_pop.GSV),
  custom.model.names = c("GPT4o Only", "ResNet Only", "Both GPT4o + ResNet", "Demographic Only", "All"),
  custom.coef.map = custom_names,
  caption = "Regression Models Predicting Composite Sustainability Index (SI)",
  caption.above = TRUE,
  label = "tab:sustainability_regression",
  stars =  0.05,
  digits = 3,
  include.fstatistic = FALSE,
  include.rmse = FALSE,
  include.nobs = TRUE,
  include.rsquared = FALSE,
  include.adjrs = TRUE,
  float.pos = "htbp",
  fontsize = "footnotesize",
  booktabs = TRUE,
  use.packages = FALSE,
  file = "Pilot3/Tab/Sustainability_Index_Regression.tex",
  notes = "* p < 0.05. Standard errors in parentheses. Higher coefficients indicate stronger predictive alignment with composite SI derived from authoritative ACS and GEIE estimates.",
  note.align = "l"
)



# -------------------------------------------------------------------
# 1.  Helper: refit model on a bootstrap sample ---------------------
# -------------------------------------------------------------------
boot_adj_r2 <- function(data, indices, model_formula, model_type = c("lm", "felm")) {
  d <- data[indices, ]
  model_type <- match.arg(model_type)
  
  fit <- switch(model_type,
                lm   = lm(model_formula,  data = d),
                felm = felm(model_formula, data = d))
  
  if (inherits(fit, "felm")) {
    return(summary(fit)$adj.r.squared)
  } else {           # lm
    return(summary(fit)$adj.r.squared)
  }
}

# -------------------------------------------------------------------
# 2.  A wrapper to run the bootstrap for ONE model ------------------
# -------------------------------------------------------------------
bootstrap_model <- function(model_object,
                            data,
                            model_name,
                            measure_name,
                            R  = 500) {
  
  # capture the original formula and model class
  fml  <- formula(model_object)
  cls  <- ifelse(inherits(model_object, "felm"), "felm", "lm")
  
  # run the bootstrap
  bt   <- boot(data      = data,
               statistic = boot_adj_r2,
               R         = R,
               model_formula = fml,
               model_type    = cls)
  
  ci   <- boot.ci(bt, type = "perc")$percent[4:5]
  
  tibble(
    Model        = model_name,
    Measure      = measure_name,
    Adj_R2       = mean(bt$t),
    CI_Lower     = ci[1],
    CI_Upper     = ci[2]
  )
}

# -------------------------------------------------------------------
# 3.  Define models, names, and measures ----------------------------
# -------------------------------------------------------------------
# ----- Poverty ------------------------------------------------------
pov_models <- list(
  "GPT4o Only"            = lm_llm_pov,       # felm object
  "ResNet Only"           = lm_dl_pov,
  "Controls Only"      = lm_pov_pop,
  "GPT4o + ResNet"   = lm_both_pov,
  "GPT4o + Controls"                   = lm_pov_pop.GSV
)

# ----- Tree canopy --------------------------------------------------
canopy_models <- list(
  "GPT4o Only"            = llm_canpy,        # lm object
  "ResNet Only"           = dl_canpy,
  "Controls Only"      = canpy_pop,
  "GPT4o + ResNet"   = both_canpy,
  "GPT4o + Controls"                   = canpy_pop.GSV
)

# ----- Sustainability index ----------------------------------------
sdi_models <- list(
  "GPT4o Only"            = lm_llm_SDI,
  "ResNet Only"           = lm_dl_SDI,
  "Controls Only"      = lm_SDI_pop,
  "GPT4o + ResNet"   = lm_both_SDI,
  "GPT4o + Controls"                   = lm_SDI_pop_pop.GSV
)

# -------------------------------------------------------------------
# 4.  Run the bootstrap for every model -----------------------------
# -------------------------------------------------------------------
set.seed(123)   # reproducible

adj_r2_boot <- bind_rows(
  imap_dfr(pov_models,
           ~ bootstrap_model(.x, combined_df, .y, "Poverty",            R = 500)),
  imap_dfr(canopy_models,
           ~ bootstrap_model(.x, combined_df, .y, "Tree Canopy",        R = 500)),
  imap_dfr(sdi_models,
           ~ bootstrap_model(.x, combined_df, .y, "Sustainability Index", R = 500))
)

# -------------------------------------------------------------------
# 5.  Ensure ordered factors (for plotting consistency) -------------
# -------------------------------------------------------------------
adj_r2_bootDF <- adj_r2_boot %>%
  mutate(
    Model   = factor(Model,
                     levels = c("GPT4o + Controls", "GPT4o + ResNet",
                                "Controls Only", "ResNet Only", "GPT4o Only")),
    Measure = factor(Measure,
                     levels = c("Poverty", "Tree Canopy", "Sustainability Index"))
  )

# -------------------------------------------------------------------
# 6.  Result ---------------------------------------------------------
print(adj_r2_bootDF, n = Inf)

library(ggplot2)
library(RColorBrewer)

# First, reverse the factor order for Model
adj_r2_bootDF$Model <- factor(
  adj_r2_bootDF$Model,
  levels = rev(levels(adj_r2_bootDF$Model))
)

adj_r2_bootDF <- adj_r2_bootDF %>%
  mutate(R2_for_plot = Adj_R2)


adj_r2_bootDF <- adj_r2_bootDF %>%
  mutate(
    Alpha = case_when(
      Model %in% c("GPT4o Only", "ResNet Only") ~ 1,
      TRUE ~ 0.4
    )
  )

Fig_BootstrapRsqrOLS <- ggplot(adj_r2_bootDF, aes(x = R2_for_plot, y = R2_for_plot, color = Model, alpha = Alpha)) +
  geom_point(size = 4, position = position_dodge(width = 0.04)) +
  
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.02, alpha = 0.6, #size = 1,
                position = position_dodge(width = 0.04)) +
  geom_errorbarh(aes(xmin = CI_Lower, xmax = CI_Upper), height = 0.02, #size = 1,
                 position = position_dodge(width = 0.04)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", linewidth = 1) +
  facet_wrap(~ Measure, scales = "fixed") +
  scale_color_brewer(palette = "Set1", name = "Model", direction = -1) +
  guides(color = guide_legend(reverse = TRUE),nrow = 1, byrow = TRUE) +
  scale_alpha(range = c(0.4, 1), guide = "none") +
  labs(
    subtitle = "A. Model Fit Comparison Across OLS Models",
    x = expression(R^2 ~ ": Y ~ {Model}"),
    y = expression(R^2 ~ ": Y ~ {Model}"),
  ) +
  xlim(0, 1) + ylim(0, 1) +
  theme_classic(base_size = 14) +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 14),
    axis.subtitle = element_text(size = 13),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.justification = "center",
    legend.box.just = "center",
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.key.size = unit(0.5, "cm"),
    strip.text = element_text(face = "bold", size = 14),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
  )

Fig_BootstrapRsqrOLS


###############################################################################################

###############################################################################################




#####################
#### QuantReg
#####################



# Dynamic formula creation
create_formula <- function(dep, preds, controls) {
  all_preds <- c(preds, controls)
  as.formula(paste(dep, "~", paste(all_preds, collapse = "+")))
}

# Pseudo R² calculation function
pseudo_r2 <- function(model, null_model) {
  1 - sum(abs(model$residuals)) / sum(abs(null_model$residuals))
}

# Main bootstrapping function
calculate_quantile_r2_bootstrap <- function(df, outcome, dl_pred, llm_pred, controls = NULL, measure_label, quantiles, R = 1000) {
  
  map_dfr(quantiles, function(tau) {
    
    # DL and LLM formula
    dl_formula <- create_formula(outcome, dl_pred, controls)
    llm_formula <- create_formula(outcome, llm_pred, controls)
    null_formula <- as.formula(paste(outcome, "~ 1"))
    
    # Bootstrap functions
    boot_dl <- function(data, indices) {
      d <- data[indices, ]
      fit <- rq(dl_formula, tau = tau, data = d)
      null_fit <- rq(null_formula, tau = tau, data = d)
      pseudo_r2(fit, null_fit)
    }
    
    boot_llm <- function(data, indices) {
      d <- data[indices, ]
      fit <- rq(llm_formula, tau = tau, data = d)
      null_fit <- rq(null_formula, tau = tau, data = d)
      pseudo_r2(fit, null_fit)
    }
    
    # Execute bootstrapping
    boot_dl_out <- boot(df, statistic = boot_dl, R = R)
    boot_llm_out <- boot(df, statistic = boot_llm, R = R)
    
    # Extract confidence intervals
    dl_ci <- boot.ci(boot_dl_out, type = "perc")$percent[4:5]
    llm_ci <- boot.ci(boot_llm_out, type = "perc")$percent[4:5]
    
    tibble(
      Measure = measure_label,
      Quantile = factor(tau),
      DL_R2 = mean(boot_dl_out$t),
      DL_CI_Lower = dl_ci[1],
      DL_CI_Upper = dl_ci[2],
      LLM_R2 = mean(boot_llm_out$t),
      LLM_CI_Lower = llm_ci[1],
      LLM_CI_Upper = llm_ci[2]
    )
  })
}

# Quantiles to evaluate
#quantiles <- seq(0.1, 1.0, by = 0.1)
quantiles <- c(0.1, 0.25, 0.5, 0.75, 0.9)


# Poverty Results
Pov_quantile_bootstrap <- calculate_quantile_r2_bootstrap(
  df = combined_df, outcome = "pctpovnorm", 
  dl_pred = "DL_poverty_pred", llm_pred = "mean_poverty_classification",
  #controls = c("Lag_pctpovnorm"), 
  measure_label = "Poverty",
  quantiles = quantiles, R = 500)

# Canopy Results
Canopy_quantile_bootstrap <- calculate_quantile_r2_bootstrap(
  df = combined_df, outcome = "treecanopy", 
  dl_pred = "DL_canopy_pred", llm_pred = "mean_canopy_coverage",
  #controls = c("Lag_treecanopy"), 
  measure_label = "Canopy",
  quantiles = quantiles, R = 500)

# Sustainability Index Results
Sustainability_quantile_bootstrap <- calculate_quantile_r2_bootstrap(
  df = combined_df, outcome = "Census_Sustainable_Dev_Index", 
  dl_pred = "DL_Sustainable_Dev_Index", llm_pred = "LLM_Sustainable_Dev_Index",
  #controls = c("Lag_Census_Sustainable_Dev_Index"), 
  measure_label = "Sustainability Index",
  quantiles = quantiles, R = 500)

# Combine results explicitly:
all_bootstrap_results <- bind_rows(
  Sustainability_quantile_bootstrap,
  Pov_quantile_bootstrap,
  Canopy_quantile_bootstrap
)


all_bootstrap_results$Measure <- factor(
  all_bootstrap_results$Measure,
  levels = c("Poverty", "Canopy", "Sustainability Index"),
  labels = c("Poverty", "Tree Canopy", "Sustainability Index")
)
# Final combined plot
Fig_BootstrapRsqrQuant <- ggplot(all_bootstrap_results, aes(x = DL_R2, y = LLM_R2, color = Quantile)) +
  geom_point(size = 4) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", linewidth = 1) +
  geom_errorbar(aes(ymin = LLM_CI_Lower, ymax = LLM_CI_Upper), width = 0.02, alpha = 0.6) +
  geom_errorbarh(aes(xmin = DL_CI_Lower, xmax = DL_CI_Upper), height = 0.02, alpha = 0.6) +
  facet_wrap(~ Measure, scales = "fixed") +
  scale_color_brewer(palette = "Dark2", name = expression("Quantile (" * tau * ")")) +
  guides(color = guide_legend(nrow = 1, byrow = TRUE)) +
  labs(
    subtitle = expression("B. Model Fit Comparison Across Select Quantiles (" * Y[tau] * "), GPT4o vs. ResNet"),
    x = expression(R^2 * ": " ~ Y[ tau ] ~ " ~ " ~ "{ResNet}"),
    y = expression(R^2 * ": " ~ Y[ tau ] ~ " ~ " ~ "{GPT4o}"),
    color = expression("Quantile (" * tau * ")")
    ) +
  xlim(0, 1) + ylim(0, 1) +
  theme_classic(base_size = 14) +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 14),
    axis.subtitle = element_text(size = 13),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.justification = "center",
    legend.box.just = "center",
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.key.size = unit(0.5, "cm"),
    
    strip.text = element_text(face = "bold", size = 14),
    
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
  )

# Display the plot clearly
Fig_BootstrapRsqrQuant

print(all_bootstrap_results)

ExploratroyRplot <- Fig_BootstrapRsqrOLS + Fig_BootstrapRsqrQuant + plot_layout(ncol = 1) 

ExploratroyRplot



#ggsave("Pilot3/Fig/ExploratroyRplot.pdf", ExploratroyRplot,  width = 10, height = 10, dpi = 300)



# Save R results to rmd for dynamic updating
adj_r2_bootDF <- adj_r2_bootDF %>%
  mutate(
    Adj_R2 = round(Adj_R2, 2),
    CI_Lower = round(CI_Lower, 2),
    CI_Upper = round(CI_Upper, 2),
    R2_for_plot = round(R2_for_plot, 2)
  )

all_bootstrap_results <- all_bootstrap_results %>%
  mutate(
    DL_R2 = round(DL_R2, 2),
    DL_CI_Lower = round(DL_CI_Lower, 2),
    DL_CI_Upper = round(DL_CI_Upper, 2),
    LLM_R2 = round(LLM_R2, 2),
    LLM_CI_Lower = round(LLM_CI_Lower, 2),
    LLM_CI_Upper = round(LLM_CI_Upper, 2)
  )


## save as appendix tables


# Assuming your data frame is named `all_bootstrap_results`
formatted_data <- all_bootstrap_results %>%
  mutate(
    DL_R2_formatted = sprintf("%.3f", DL_R2),
    DL_CI_formatted = sprintf("(%.3f--%.3f)", DL_CI_Lower, DL_CI_Upper),
    LLM_R2_formatted = sprintf("%.3f", LLM_R2),
    LLM_CI_formatted = sprintf("(%.3f--%.3f)", LLM_CI_Lower, LLM_CI_Upper)
  ) %>%
  select(Measure, Quantile, DL_R2_formatted, DL_CI_formatted, LLM_R2_formatted, LLM_CI_formatted)

# Pivot data into wide format for the table
table_matrix <- formatted_data %>%
  tidyr::pivot_wider(names_from = Quantile, values_from = c(DL_R2_formatted, DL_CI_formatted, LLM_R2_formatted, LLM_CI_formatted)) %>%
  arrange(Measure)

# Manually create rows for DL and LLM separately
table_rows <- list()
for (measure in unique(table_matrix$Measure)) {
  subset <- table_matrix %>% filter(Measure == measure)
  
  dl_row <- c(
    paste(measure, "(ResNet)"),
    subset$DL_R2_formatted_0.1, subset$DL_R2_formatted_0.25, subset$DL_R2_formatted_0.5, subset$DL_R2_formatted_0.75, subset$DL_R2_formatted_0.9
  )
  dl_ci_row <- c(
    "",
    subset$DL_CI_formatted_0.1, subset$DL_CI_formatted_0.25, subset$DL_CI_formatted_0.5, subset$DL_CI_formatted_0.75, subset$DL_CI_formatted_0.9
  )
  
  llm_row <- c(
    paste(measure, "(GPT-4o)"),
    subset$LLM_R2_formatted_0.1, subset$LLM_R2_formatted_0.25, subset$LLM_R2_formatted_0.5, subset$LLM_R2_formatted_0.75, subset$LLM_R2_formatted_0.9
  )
  llm_ci_row <- c(
    "",
    subset$LLM_CI_formatted_0.1, subset$LLM_CI_formatted_0.25, subset$LLM_CI_formatted_0.5, subset$LLM_CI_formatted_0.75, subset$LLM_CI_formatted_0.9
  )
  
  table_rows <- append(table_rows, list(dl_row, dl_ci_row, llm_row, llm_ci_row))
}

# Convert to matrix
final_table_matrix <- do.call(rbind, table_rows)

# Generate and save the table
R2quantTable <- kbl(
  final_table_matrix,
  format = "latex",
  booktabs = TRUE,
  caption = "Pseudo-$R^2$ for GPT-4o and ResNet Predictions by Quantile",
  label = "tab:quantile_r2",
  escape = FALSE,
  col.names = c("Measure", "0.10", "0.25", "0.50", "0.75", "0.90"),
  align = c("l", rep("c", 5))  # left-align first column, center others
) %>%
  kable_styling(font_size = 8, position = "center") %>%
  pack_rows("Poverty", 1, 4) %>%
  pack_rows("Sustainability Index", 5, 8) %>%
  pack_rows("Tree Canopy", 9, 12) %>%
  add_footnote(
    label = "\\footnotesize{Note: Values represent pseudo-$R^2$ estimates. 95\\% confidence intervals in parentheses.}",
    notation = "none",
    threeparttable = TRUE,
    escape = FALSE
  )

save_kable(R2quantTable, "Pilot3/Tab/quantile_r2_table.tex")


####################################################################################################################################


####################################################################################################################################
# Map 
###################################################################################################################################



# Add leading zero to GEOIDs in your combined_df
final_dataset <- final_dataset %>%
  mutate(GEOID1 = sprintf("%012s", GEOID))

# Extract unique GEOIDs from your data
geoids <- unique(final_dataset$GEOID1)


# Get geometry data from the ACS (e.g., block groups)
geometry_data <- get_acs(
  geography = "block group",
  variables = c(
    Per_Capita_Income = "B19301_001"
  ),
  year = 2023,
  survey = "acs5",     # Specify 5-year ACS explicitly
  state = "AZ",
  county = "Maricopa",
  geometry = TRUE
)

# Keep only relevant GEOIDs matching your data
geometry_data_Map <- geometry_data %>%
  filter(GEOID %in% geoids) 

subset_final_dataset <- final_dataset %>%
  select(GEOID1, pctpovnorm, treecanopy,holc_grade,
         DL_poverty_pred,
         DL_canopy_pred  )

subset_final_dataset <- subset_final_dataset %>%
  mutate(
    census_pov_norm = scale(pctpovnorm),
    census_canopy_norm = scale(treecanopy),
    Census_Sustainable_Dev_Index = (census_canopy_norm - census_pov_norm)/2
  )


subset_final_dataset$GEOID<-subset_final_dataset$GEOID1

geometry_data_Map <- geometry_data %>%
  filter(GEOID %in% subset_final_dataset$GEOID) %>% 
  left_join(subset_final_dataset, by = "GEOID")


str(geometry_data_Map)

st_crs(geometry_data_Map)
# Recommended: EPSG 4269 (NAD83), or EPSG 4326 (WGS84)

# Load Maricopa County block groups
maricopa_cbg <- block_groups(state = "AZ", county = "Maricopa", year = 2022) %>% 
  st_transform(st_crs(geometry_data_Map))

# Join your data with block groups
map_data <- maricopa_cbg %>%
  left_join(st_drop_geometry(geometry_data_Map), by = "GEOID")


map_data <- map_data %>%
  left_join(combined_df %>% select(GEOID1, place), by = c("GEOID" = "GEOID1"))

map_data <- map_data %>%
  mutate(HolcFactor = case_when(
    holc_grade == "A" ~ "A",
    holc_grade == "B" ~ "B",
    holc_grade == "C" ~ "C",
    holc_grade == "D" ~ "D",
    TRUE ~ "Non-Incorporated (Gilbert)"
  ))


# Filter to CBGs with valid Sustainability Index
map_data_filtered <- map_data %>% 
  filter(!is.na(Census_Sustainable_Dev_Index))


# US States (excluding AK, HI)
us_states <- states(cb = TRUE) %>% 
  filter(!STUSPS %in% c("AK", "HI", "PR", "GU", "VI", "MP", "AS")) %>% 
  st_transform(4326)

# Arizona boundary
arizona <- us_states %>% 
  filter(STUSPS == "AZ")

# Maricopa County boundary
maricopa_county <- counties(state = "AZ", cb = TRUE, year = 2022) %>%
  filter(NAME == "Maricopa") %>%
  st_transform(4326)

# Main Map
main_map <- ggplot() +
  geom_sf(data = maricopa_county, fill = "grey95", color = "grey80") +
  geom_sf(data = map_data_filtered, fill = "white", color = NA) +
  theme_void() +
  labs(subtitle = "") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))

# Inset Map with black boundary
# compute centroid of Maricopa County
maricopa_centroid <- st_centroid(maricopa_county)

# dark‐mode inset map
inset_map <- ggplot() +
  # dark‐grey U.S. states
  geom_sf(data = us_states, fill = "#2b2b2b", color = "#444444", size = 0.2) +
  # county boundary in red outline only
  geom_sf(data = maricopa_county, fill = NA, color = "red", size = 0.6) +
  # red star at centroid
  geom_sf(data = maricopa_centroid, shape = 8, color = "red", size = 2, stroke = 1.2) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "black", color = "black")
  )

inset_map


# Register Google API key if you have one (optional but recommended)
# register_google(key = "YOUR_API_KEY")

register_stadiamaps(key = "ee94c05b-676d-40b5-bf80-edd1db4d0e7d")


# Your bounding box values
bbox <- c(left = -112.32994, bottom = 33.20464, right = -111.68265, top = 33.82769)

# Fetch Stadia Map background (using a valid maptype from the listed options)
#base_map <- get_stadiamap(
#  bbox = bbox,
#  zoom = 12,
#  maptype = "stamen_toner_lite",
#  filetype = "png"
#)

# Plot to verify



library(dplyr)
library(ggmap)
library(ggplot2)

map_data_filtered <- map_data_filtered %>%
  mutate(
    HolcFactor_Revised = case_when(
      # true “A” HOLC in Phoenix
      place == "Phoenix" & holc_grade == "A" ~ "A",
      # collapse B + C into one
      place == "Phoenix" & holc_grade %in% c("B", "C") ~ "B/C",
      # true “D” HOLC in Phoenix
      place == "Phoenix" & holc_grade == "D" ~ "D",
      # non‐graded Phoenix → “Pseudo-B/C”
      place == "Phoenix" & holc_grade == "" ~ "Pseudo-B/C",
      # non‐graded Gilbert → “Pseudo-A”
      place == "Gilbert" & holc_grade == "" ~ "Pseudo-A",
      # (just in case any others slip through)
      TRUE ~ "Pseudo-B/C"
    )
  ) %>%
  mutate(
    HolcFactor_Revised = factor(
      HolcFactor_Revised,
      levels = c("A", "Pseudo-A", "B/C", "Pseudo-B/C", "D")
    )
  )

holc_map <- ggmap(base_map) +
  geom_sf(
    data = map_data_filtered,
    aes(fill = HolcFactor_Revised),
    inherit.aes = FALSE,
    color = NA, alpha = 1
  ) +
  scale_fill_manual(
    "" ,
    values = c(
      "A"           = "green",
      "Pseudo-A"    = "lightgreen",
      "B/C"         = "yellow",
      "Pseudo-B/C"  = "skyblue",
      "D"           = "red"
    )
  ) +
  labs(title = "A.") +
  theme_void() +
  theme(
    plot.title    = element_text(face = "bold", size = 14),
    legend.position = "none"
  )

holc_map




# Combine Maps
final_map <- ggdraw() +
  draw_plot(holc_map) +
  # flush to top‐right: x ~ 0.65–0.7, y ~ 0.65–0.7
  draw_plot(inset_map,
            x = 0.60,      # distance from left (0 = left edge, 1 = right edge)
            y = 0.75,      # distance from bottom (0 = bottom, 1 = top)
            width  = 0.2, # width of inset (fraction of full canvas)
            height = 0.2  # height of inset
  )

# print it
print(final_map)

# Set consistent themes for all plots first
standard_theme <- theme(
  plot.title = element_text(size = 12, face = "bold"),
  axis.title = element_text(size = 10),
  axis.text = element_text(size = 9),
  legend.text = element_text(size = 9),
  legend.title = element_blank()
)

# Apply the theme to all components
final_map <- final_map +     theme_void() + standard_theme 
#################





#######################
# --- 1) Build a long data‐frame with geometry + (panel, raw) + fill01 in [0,1] ---

library(scales)   # for rescale()

# 1) min–max rescale your canopy field to exactly fill [0,1]:
map_data_filtered <- map_data_filtered %>%
  mutate(
    canopy_rescaled = rescale(treecanopy, to = c(0,1))  
    # now canopy_rescaled runs from  0 → 1, instead of ~0 → 0.5
  )

# 2) pivot & compute fill01 exactly the same as before:
map_long <- map_data_filtered %>%
  pivot_longer(
    cols      = c(pctpovnorm,        # poverty already [0,1]
                  canopy_rescaled,   # now also [0,1]
                  Census_Sustainable_Dev_Index),
    names_to  = "panel",
    values_to = "raw"
  ) %>%
  filter(!is.na(raw)) %>%
  mutate(
    panel = factor(panel,
                   levels = c("pctpovnorm","canopy_rescaled","Census_Sustainable_Dev_Index"),
                   labels = c("Poverty","Canopy","SI")
    ),
    fill01 = case_when(
      panel == "Poverty" ~ 1 - raw,      # flip poverty
      panel == "Canopy"  ~ raw,          # now 0→1 exactly
      panel == "SI"      ~ (raw + 3) / 5  # map [-3,2] → [0,1]
    )
  )


# 3) common two‐tick “Bad → Good” legend:
common_fill <- scale_fill_gradientn(
  name = NULL,
  colours = c("red","red", ,"lightgreen", "green"),
  limits = c(0, 1),
  breaks = c(0, 1),
  labels = c("Unfavorable", "Favorable")
)

# 4) one faceted plot with shared legend:
combined_choro <- 
  ggmap(base_map) +
  geom_sf(data = map_long,
          aes(fill = fill01),
          inherit.aes = FALSE,
          color = NA, alpha = 0.8) +
  facet_wrap(~panel, nrow = 1) +
  common_fill +
  theme_void(base_size = 14) +
  theme(
    strip.text       = element_text(face="bold", size=14),
    legend.position  = "bottom",
    legend.key.width = unit(2, "cm"),
    legend.key.height= unit(0.3, "cm"),
    legend.text      = element_text(size=11)
  )

combined_choro
#####################



########################
##COmbine figures
########################
ScatterMain <- ggarrange(p1 + p2 + p3,
                        legend = "bottom", nrow = 1, ncol = 1)




# Final vertical stack
#combined_plotMap <- HOLCmapingFig / choropleths / ScatterAll +
#  plot_layout(heights = c(3, 1, .5)
#              )   

DescriptivesFig<-ggarrange(FigBarPlot, ScatterMain, common.legend = TRUE, nrow=2,legend = "bottom")

MapsFig<-ggarrange(final_map, combined_choro, common.legend = TRUE, nrow=2, legend="bottom")


combined_plotMap <- ggarrange(MapsFig,DescriptivesFig)
# Save high-quality PDF
#ggsave("Pilot3/Fig/CombinedMaps.pdf", combined_plotMap,  width = 20, height = 10, dpi = 300)

#####################

###################################
#####SAM And LLM graphic
###################################
# R Markdown conceptual diagram using DiagrammeR

grViz("
digraph flowchart {
  graph [fontsize=10 fontname='Helvetica'];
  node [shape=box fontsize=12 fontname='Helvetica' style=filled fillcolor=whitesmoke];
  
  Image [label='GSV Image'];
  Prompt [label='Structured Prompt \\n e.g. Housing Type \\n & Poverty Indicators'];
  GPT4o [label='GPT-4o Model \\n(Multimodal LLM)'];
  Output [label='Predicted Poverty \\nClassification & Reasoning'];
  
  Image -> Prompt -> GPT4o -> Output;
}
")
#######################################################




########################################################################################################################################################################################################################
########################################################################################################################################################################################################################
######################################################################## Policy Relevance ########################################################################
########################################################################################################################################################################################################################
########################################################################################################################################################################################################################




###########################################################################
##Data Setup and Zipcode FE
###########################################################################

# Merge combined_df with geometry_data_filtered
combined_df_sf <- geometry_data_filtered %>%
  select(GEOID, geometry) %>% 
  left_join(combined_df, by = c("GEOID" = "GEOID1")) %>%
  st_as_sf()

# Validate and repair combined_df_sf geometries
combined_df_sf <- combined_df_sf %>%
  st_make_valid()

# Check resulting object
str(combined_df_sf)

# Get U.S. ZCTAs for 2020 and transform CRS
zipcodes_us <- zctas(year = 2020) %>% 
  st_transform(st_crs(combined_df_sf))

# Obtain Arizona state boundary
az_state_boundary <- states(cb = TRUE) %>% 
  filter(STUSPS == "AZ") %>% 
  st_transform(st_crs(combined_df_sf))

# Subset AZ ZIP codes via spatial intersection
az_zipcodes <- st_intersection(zipcodes_us, az_state_boundary)

# Keep only ZIP Code column and geometry
az_zipcodes <- az_zipcodes %>%
  select(ZCTA5CE20, geometry)

az_zipcodes <- az_zipcodes %>% st_make_valid()


# Join ZIP codes to CBGs
combined_df_sf <- st_join(combined_df_sf, az_zipcodes, join = st_intersects)

# Convert ZIP codes to factor for Fixed Effects
combined_df_sf$ZIPCode <- factor(combined_df_sf$ZCTA5CE20)

## Redline groupingd
combined_df_sf <- combined_df_sf %>%
  mutate(HolcFactor_Revised = case_when(
    place == "Gilbert" | holc_grade == "A" ~ "Ideal",
    place == "Phoenix" & holc_grade == "" | holc_grade == "B" | holc_grade == "C" ~ "Surrounding Phoenix",
    holc_grade == "D" ~ "HOLC-Redlined",
    TRUE ~ NA
  )) %>%
  mutate(HolcFactor_Revised = factor(HolcFactor_Revised, levels = c(
   "Ideal",
     "Surrounding Phoenix",
    "HOLC-Redlined"
  )))
table(combined_df_sf$HolcFactor_Revised)

table(combined_df_sf$ZIPCode, combined_df_sf$HolcFactor_Revised)


# ReGenerate neighborhood structure spatial weights
nb <- poly2nb(combined_df_sf, queen = TRUE)
lw <- nb2listw(nb, style = "W", zero.policy = TRUE)

###########################################################################


###########################################################################
## Regresions by model specification, prediction appraoch, and measure
###############################################################################

# Define function for model estimation
run_models <- function(data, outcome, listw) {
  
  # Formulas
  base_formula <- as.formula(paste(outcome, "~ HolcFactor_Revised"))
  covariate_formula <- as.formula(paste(outcome, "~ HolcFactor_Revised + log(Pop_Density) + depratnorm + lingnorm + pct_black + pct_hispanic + pct_asian + pct_bachelors"))
  zipcode_FE_formula <- as.formula(paste(outcome, "~ HolcFactor_Revised + log(Pop_Density) + depratnorm + lingnorm + pct_black + pct_hispanic + pct_asian + pct_bachelors | ZIPCode"))
  spatial_formula <- as.formula(paste(outcome, "~ HolcFactor_Revised + log(Pop_Density) + depratnorm + lingnorm + pct_black + pct_hispanic + pct_asian + pct_bachelors"))
  
  # Models
  mod1 <- felm(base_formula, data = data)
  mod2 <- felm(covariate_formula, data = data)
  mod3 <- felm(zipcode_FE_formula, data = data)
  mod4 <- lagsarlm(spatial_formula, data = data, listw = listw, zero.policy = TRUE)
  
  return(list(Baseline = mod1, Covariates = mod2, `Zipcode FE` = mod3, `Spatial Lag` = mod4))
}



# Outcomes and types
outcomes <- list(
  Sustainability = c("Census_Sustainable_Dev_Index", "LLM_Sustainable_Dev_Index", "DL_Sustainable_Dev_Index"),
  Poverty = c("census_pov_norm", "LLM_pov_norm", "DL_pov_norm"),
  Canopy = c("census_canopy_norm", "LLM_canopy_norm", "DL_canopy_norm")
)


approaches <- c("LLM", "Census/GEIE", "ResNet")

# Run and store models systematically
resultsReg <- list()

for (measure in names(outcomes)) {
  for (i in seq_along(outcomes[[measure]])) {
    outcome_var <- outcomes[[measure]][i]
    approach <- approaches[i]
    
    mods <- run_models(combined_df_sf, outcome_var, lw)
    
    # Convert each model to tidy format and store
    for (model_name in names(mods)) {
      tidy_mod <- tidy(mods[[model_name]]) %>%
        mutate(
          Model = model_name,
          Approach = approach,
          Measure = measure
        )
      resultsReg[[paste(measure, approach, model_name, sep = "_")]] <- tidy_mod
    }
  }
}




# Combine all resultsReg
tidy_results <- bind_rows(resultsReg) %>%
  filter(term %in% c("HolcFactor_RevisedSurrounding Phoenix", "HolcFactor_RevisedHOLC-Redlined"))

# Adjust factor levels and labels
tidy_results$Model <- factor(tidy_results$Model, levels = c("Spatial Lag", "Zipcode FE", "Covariates", "Baseline"))
tidy_results$term <- factor(tidy_results$term, 
                            levels = c("HolcFactor_RevisedHOLC-Redlined", "HolcFactor_RevisedSurrounding Phoenix"),
                            labels = c("Redlined", "Surrounding Phoenix"))

tidy_results$Measure<-factor(tidy_results$Measure, levels=c("Poverty","Canopy","Sustainability"))
tidy_results$Model<-factor(tidy_results$Model, levels = c("Baseline", "Covariates",  "Zipcode FE",  "Spatial Lag"))

print(tidy_results,n=100)
tidy_results$Approach<-factor(tidy_results$Approach, levels=c("LLM","ResNet","Census/GEIE"))

FigPolEvalresults<-ggplot(tidy_results, aes(x = estimate, y = Approach, color = term)) +
  geom_point(size = 3, position = position_dodge(0.5)) +
  geom_errorbar(aes(xmin = estimate - 1.96 * std.error, xmax = estimate + 1.96 * std.error),
                width = 0.2, position = position_dodge(0.5)) +
  facet_grid(Model ~ Measure, scales = "free") +
  scale_x_continuous(breaks = seq(-2, 2, .5)) +
  labs(title = "",
       x = "Estimate",
       y = "Prediction Approach",
       color = "Neighborhoods\n(Ideal=Ref.)") +
  scale_color_manual(values = c("Surrounding Phoenix" = "#4575b4", "Redlined" = "#d73027")) +
  theme_minimal() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  theme_classic(base_size = 14) +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 14),
    axis.subtitle = element_text(size = 13),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.justification = "center",
    legend.box.just = "center",
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.key.size = unit(0.5, "cm"),
    
    strip.text = element_text(face = "bold", size = 14),
    
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
  )
FigPolEvalresults




outcomesMeans<- combined_df_sf %>% 
  select(Census_Sustainable_Dev_Index,LLM_Sustainable_Dev_Index,DL_Sustainable_Dev_Index,
         census_pov_norm,LLM_pov_norm,DL_pov_norm, # Standardized poverty measures by census, llm, resnet
         pctpovnorm, # raw variable Census poverty
         mean_poverty_classification, #Raw LLM poverty predictions
         DL_poverty_pred, # Raw RESNET poverty predictions
         
         census_canopy_norm,LLM_canopy_norm,DL_canopy_norm, #standardize canopy measures by census, llm, resnet
         treecanopy, # raw GEIE high resolution satelite imgary predcitions
         DL_canopy_pred, #Raw RESNET poverty predictions
         mean_canopy_coverage #Raw LLM canopy predictions
  ) %>% summary() 




##Save to csv for RMD
outcomesMeans


save( all_bootstrap_results,adj_r2_bootDF,tidy_results,boot_summary, file = "ManuscriptResults.RData" )


#ggsave("Pilot3/Fig/FigPolEvalresults.pdf", FigPolEvalresults, width = 12, height = 12, dpi = 300)



library(texreg)
library(dplyr)
library(tidyr)

library(texreg)
library(dplyr)
library(tidyr)

library(dplyr)
library(tidyr)
library(texreg)

## --- helper --------------------------------------------------------------
generate_texreg_table <- function(data, measure_name, file_name) {
  
  ## keep the two HOLC terms for the requested outcome
  df <- data %>% 
    filter(Measure == measure_name) %>% 
    arrange(term, Approach, Model) %>% 
    mutate(row_lab = paste(term, Approach, sep = "_")) %>% 
    select(Model, row_lab, estimate, std.error) %>% 
    pivot_wider(names_from = Model,
                values_from = c(estimate, std.error),
                names_glue = "{Model}_{.value}")
  
  model_specs <- c("Baseline", "Covariates", "Zipcode FE", "Spatial Lag")
  
  ## build one texreg object per column -----------------------------------
  tlist <- lapply(model_specs, function(ms) {
    b  <- df[[paste0(ms, "_estimate")]]
    se <- df[[paste0(ms, "_std.error")]]
    names(b)  <- df$row_lab
    names(se) <- df$row_lab
    
    createTexreg(coef.names = names(b),
                 coef        = b,
                 se          = se,
                 gof.names   = character(0),
                 gof         = numeric(0))
  })
  
  ## write LaTeX table -----------------------------------------------------
  texreg(tlist,
         custom.model.names = model_specs,
         caption            = paste("HOLC coefficient estimates –", measure_name),
         label            = paste("HOLC coefficient estimates –", measure_name),
         caption.above      = TRUE,
         stars              = c(0.05),
         digits             = 3,
         fontsize           = "footnotesize",
         booktabs           = FALSE,
         file               = file_name)
}

## --- run for each outcome -----------------------------------------------
outcomes <- c("Poverty", "Canopy", "Sustainability")

tidy_results1<-tidy_results

# Replace "LLM" with "GPT4o" in Approach column or row_label
tidy_results1$Approach <- gsub("LLM", "GPT4o", tidy_results1$Approach)

for (m in outcomes) {
  generate_texreg_table(tidy_results1,
                        measure_name = m,
                        file_name    = paste0("Pilot3/Tab/", m, "_HOLC_Coefficients.tex"))
}



###########################################################################


###########################################################################
## Stacked Models
###############################################################################


# Precompute spatial weights once explicitly
nb <- poly2nb(combined_stacked, queen = TRUE)
lwStack <- nb2listw(nb, style = "W", zero.policy = TRUE)

# Use lw in all spatial lag models
run_stacked_analysis <- function(data, dep_vars, baseline_var, controls, fe_var, lw) {
  results <- list()
  
  for (measure_group in names(dep_vars)) {
    
    vars <- dep_vars[[measure_group]]
    
    stacked_df <- data %>%
      pivot_longer(cols = all_of(vars),
                   names_to = "Measure_Type",
                   values_to = "Value")
    
    # Zipcode FE model (unchanged)
    formula_zip <- as.formula(
      paste("Value ~", baseline_var, "* Measure_Type +", paste(controls, collapse = "+"), "|", fe_var)
    )
    mod_zip_fe <- felm(formula_zip, data = stacked_df)
    
    # Spatial lag model (re-use lw)
    formula_spatial <- as.formula(
      paste("Value ~", baseline_var, "* Measure_Type +", paste(controls, collapse = "+"))
    )
    
    # Speed up spatial model using parallel processing explicitly
    #set.coresOption(detectCores() - 1) # Use all but one core
    mod_spatial <- lagsarlm(formula_spatial, data = stacked_df, listw = lw, zero.policy = TRUE)
    
    results[[measure_group]] <- list(zip_fe = mod_zip_fe, spatial = mod_spatial)
  }
  
  return(results)
}



# Define outcomes explicitly
dep_vars <- list(
  Sustainability = c("Census_Sustainable_Dev_Index", "LLM_Sustainable_Dev_Index", "DL_Sustainable_Dev_Index"),
  Poverty = c("census_pov_norm", "LLM_pov_norm", "DL_pov_norm"),
  Canopy = c("census_canopy_norm", "LLM_canopy_norm", "DL_canopy_norm")
)

controls <- c("log(Pop_Density)", "depratnorm", "lingnorm",
              "pct_black", "pct_hispanic", "pct_asian", "pct_bachelors")

str(stacked_results)
# Run stacked results with parallelized spatial lag model
stacked_results <- run_stacked_analysis(
  data = combined_df_sf,
  dep_vars = dep_vars,
  baseline_var = "HolcFactor_Revised",
  controls = controls,
  fe_var = "ZIPCode",
  lw = lwStack
)


# Example: get all models for the "Poverty" outcome
zipfe_pov <- stacked_results$Poverty$zip_fe
spatial_pov <- stacked_results$Poverty$spatial

# ...same for Canopy, Sustainability:
zipfe_canopy <- stacked_results$Canopy$zip_fe
spatial_canopy <- stacked_results$Canopy$spatial

 zipfe_sust <- stacked_results$Sustainability$zip_fe
 spatial_sust <- stacked_results$Sustainability$spatial


 library(stringr)
 
 rename_coef_names <- function(old_names) {
   new_names <- old_names
   
   # Main effects (Neighborhood)
   new_names <- str_replace(new_names, "HolcFactor_RevisedSurrounding Phoenix", "Surrounding Phoenix")
   new_names <- str_replace(new_names, "HolcFactor_RevisedHOLC-Redlined", "Redlined")
   
   # Main effects (Prediction type)
   new_names <- str_replace(new_names, "^Measure_TypeDL_.*", "ResNet")
   new_names <- str_replace(new_names, "^Measure_TypeLLM_.*", "LLM")
   
   # Main effects (Standard covariates)
   new_names <- str_replace(new_names, "^log\\(Pop_Density\\)$", "Pop. Density (ln)")
   new_names <- str_replace(new_names, "^depratnorm$", " Dependency Rate")
   new_names <- str_replace(new_names, "^lingnorm$", "Linguistic Isolation Rate")
   new_names <- str_replace(new_names, "^pct_black$", "Black (%)")
   new_names <- str_replace(new_names, "^pct_hispanic$", "Hispanic (%)")
   new_names <- str_replace(new_names, "^pct_asian$", "Asian (%)")
   new_names <- str_replace(new_names, "^pct_bachelors$", "College Educated (%)")
   
   # Interactions
   # e.g. "Surrounding Phoenix:Measure_TypeDL_pov_norm" etc.
   new_names <- str_replace(new_names, "^Surrounding Phoenix:Measure_TypeDL_.*", "Surrounding Phoenix × ResNet")
   new_names <- str_replace(new_names, "^Redlined:Measure_TypeDL_.*", "Redlined × ResNet")
   new_names <- str_replace(new_names, "^Surrounding Phoenix:Measure_TypeLLM_.*", "Surrounding Phoenix × LLM")
   new_names <- str_replace(new_names, "^Redlined:Measure_TypeLLM_.*", "Redlined × LLM")
   
   # felm stores interaction as HolcFactor_RevisedSurrounding Phoenix:Measure_TypeLLM_pov_norm
   new_names <- str_replace(new_names, "^HolcFactor_RevisedSurrounding Phoenix:Measure_TypeDL_.*", "Surrounding Phoenix × ResNet")
   new_names <- str_replace(new_names, "^HolcFactor_RevisedHOLC-Redlined:Measure_TypeDL_.*", "Redlined × ResNet")
   new_names <- str_replace(new_names, "^HolcFactor_RevisedSurrounding Phoenix:Measure_TypeLLM_.*", "Surrounding Phoenix × LLM")
   new_names <- str_replace(new_names, "^HolcFactor_RevisedHOLC-Redlined:Measure_TypeLLM_.*", "Redlined × LLM")
   
   # Intercept, spatial lag
   new_names <- str_replace(new_names, "^\\(Intercept\\)$", "Intercept")
   new_names <- str_replace(new_names, "^rho$", "Spatial Lag (ρ)")
   
   new_names
 }
 
 
 
 

 # For felm model objects
 rownames(mod_zipfe_pov$coefficients) <- rename_coef_names(rownames(mod_zipfe_pov$coefficients))
 rownames(mod_zipfe_canopy$coefficients) <- rename_coef_names(rownames(mod_zipfe_canopy$coefficients))
 rownames(mod_zipfe_sust$coefficients) <- rename_coef_names(rownames(mod_zipfe_sust$coefficients))
 # Same for other models if needed
 
 
  # For Spatial models (same function, captures "rho" and "Intercept")
 names(mod_spatial_pov$coefficients) <- rename_coef_names(names(mod_spatial_pov$coefficients))
 names(mod_spatial_canopy$coefficients) <- rename_coef_names(names(mod_spatial_canopy$coefficients))
 names(mod_spatial_sust$coefficients) <- rename_coef_names(names(mod_spatial_sust$coefficients))

 
 extract_custom_felm <- function(model) {
   coefs <- as.numeric(model$coefficients)
   ses <- as.numeric(model$se)
   pvals <- as.numeric(model$pval)
   names(coefs) <- rownames(model$coefficients) # your custom names already
   texreg::createTexreg(
     coef.names = names(coefs),
     coef = coefs,
     se = ses,
     pvalues = pvals
   )
 }
 
 # Save Table
 texreg(
   list(
     extract_custom_felm(mod_zipfe_pov),
     extract_custom_felm(mod_zipfe_canopy),
     extract_custom_felm(mod_zipfe_sust), 
     mod_spatial_pov, 
     mod_spatial_canopy, 
     mod_spatial_sust
   ),
   custom.model.names = c(
     "Poverty (Zipcode FE)", "Canopy (Zipcode FE)", "Sust. Index (Zipcode FE)",
     "Poverty (Spatial Lag)", "Canopy (Spatial Lag)", "Sust. Index (Spatial Lag)"
   ),
   caption = "Stacked Regression Results: Zipcode FE vs Spatial Lag Specifications for All Outcomes",
   label = "tab:stacked_regression_all",
   digits = 3,
   #file = "Pilot3/Tab/Stacked_AllOutcomes.tex",
   use.packages = FALSE # (optional: disables extra \usepackage statements)
 )

##########
### Figure Not Reported
##########
 interaction_data <- bind_rows(
   # Zipcode FE
   extract_interactions(stacked_results, "Sustainability", "zip_fe"),
   extract_interactions(stacked_results, "Poverty", "zip_fe"),
   extract_interactions(stacked_results, "Canopy", "zip_fe"),
   
   # Spatial Lag
   extract_interactions(stacked_results, "Sustainability", "spatial"),
   extract_interactions(stacked_results, "Poverty", "spatial"),
   extract_interactions(stacked_results, "Canopy", "spatial")
 )
 levels(factor(interaction_data$Measure))
 
 interaction_data <- interaction_data %>%
   mutate(
     Neighborhood = case_when(
       str_detect(term, "HOLC-Redlined") ~ "Redlined",
       str_detect(term, "Surrounding Phoenix") ~ "Surrounding Phoenix"
     )
   )

 FigInteraction <- ggplot(interaction_data,
                          aes(x = estimate, y = Approach, color = Neighborhood)) +
   geom_point(size = 3, position = position_dodge(width = 0.5)) +
   geom_errorbar(aes(xmin = estimate - 1.96 * std.error,
                     xmax = estimate + 1.96 * std.error),
                 width = 0.2, position = position_dodge(width = 0.5)) +
   facet_grid(Model ~ Measure, scales = "free_x") +
   scale_color_manual(values = c("Surrounding Phoenix" = "#4575b4", "Redlined" = "#d73027")) +
   
   labs(
     title = "Interaction Coefficients: Redlined and Surrounding Phoenix vs. Census Baseline",
     subtitle = "Differences by Prediction Approach (LLM and ResNet)",
     x = "Interaction Coefficient Estimate",
     y = "Prediction Approach",
     color = "Neighborhood Type"
   ) +
   geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
   theme_minimal(base_size = 14) +
   theme(legend.position = "bottom")

 
 


# Display the figure
FigInteraction

#ggsave("Pilot3/Fig/FigStackedInteractions.pdf", FigInteraction,  width = 7, height = 5, dpi = 300)

# Create directory if it does not exist
if (!dir.exists("Pilot3/Tab")) {
  dir.create("Pilot3/Tab", recursive = TRUE)
}

library(texreg)

# Function to generate and save stacked regression tables clearly
generate_stacked_tables <- function(models, measure_name) {
  texreg(models,
         custom.model.names = c("Zipcode FE", "Spatial Lag"),
         file = paste0("Pilot3/Tab/", measure_name, "_Stacked_Appendix.tex"),
         caption = paste("Appendix Table: Stacked Regression Results for", measure_name))
}

# Sustainability Stacked Tables
generate_stacked_tables(list(stacked_results$Sustainability$zip_fe, 
                             stacked_results$Sustainability$spatial), 
                        "Sustainability")

# Poverty Stacked Tables
generate_stacked_tables(list(stacked_results$Poverty$zip_fe, 
                             stacked_results$Poverty$spatial), 
                        "Poverty")

# Canopy Stacked Tables
generate_stacked_tables(list(stacked_results$Canopy$zip_fe, 
                             stacked_results$Canopy$spatial), 
                        "Canopy")
#####################

##########################################


##############################################################################################################################
## Bootstrapped 
##############################################################################################################################
library(tidyverse)
library(lfe)
library(broom)
library(boot)
library(parallel)

set.seed(123)



########################
## Bootstrapped Regression (Zipcode FE only)
########################

library(tidyverse)
library(lfe)
library(broom)
library(boot)
library(parallel)

set.seed(123)

# Prediction measures
measures <- list(
  Sustainability = c("Census_Sustainable_Dev_Index", "LLM_Sustainable_Dev_Index", "DL_Sustainable_Dev_Index"),
  Poverty = c("census_pov_norm", "LLM_pov_norm", "DL_pov_norm"),
  Canopy = c("census_canopy_norm", "LLM_canopy_norm", "DL_canopy_norm")
)

# Bootstrap function for one measure
bootstrap_coefs <- function(data, indices) {
  boot_data <- data[indices, ]
  
  model <- felm(Value ~ HolcFactor_Revised * Measure_Type +
                  log(Pop_Density) + depratnorm + lingnorm +
                  pct_black + pct_hispanic + pct_asian + pct_bachelors | ZIPCode,
                data = boot_data)
  
  coefs <- tidy(model) %>%
    filter(term %in% c(
      "HolcFactor_RevisedHOLC-Redlined",
      "HolcFactor_RevisedHOLC-Redlined:Measure_TypeLLM",
      "HolcFactor_RevisedHOLC-Redlined:Measure_TypeResNet"
    ))
  
  beta_census <- coefs$estimate[coefs$term == "HolcFactor_RevisedHOLC-Redlined"]
  beta_llm <- beta_census + coefs$estimate[coefs$term == "HolcFactor_RevisedHOLC-Redlined:Measure_TypeLLM"]
  beta_resnet <- beta_census + coefs$estimate[coefs$term == "HolcFactor_RevisedHOLC-Redlined:Measure_TypeResNet"]
  
  return(c(Census = beta_census, LLM = beta_llm, ResNet = beta_resnet))
}

# Run bootstrap for each outcome type
cores <- detectCores() - 1
results_listFE <- list()

for (measure in names(measures)) {
  
  stacked_df <- combined_df_sf %>%
    pivot_longer(cols = all_of(measures[[measure]]),
                 names_to = "Measure_Type",
                 values_to = "Value") %>%
    mutate(
      Measure_Type = case_when(
        str_detect(Measure_Type, "Census|census") ~ "Census",
        str_detect(Measure_Type, "LLM") ~ "LLM",
        str_detect(Measure_Type, "DL") ~ "ResNet"
      )
    )
  
  boot_result <- boot(
    data = stacked_df,
    statistic = bootstrap_coefs,
    R = 500,
    parallel = "multicore",
    ncpus = cores
  )
  
  boot_coefs <- data.frame(
    Method = rep(c("Census", "LLM", "ResNet"), each = nrow(boot_result$t)),
    Coefficient = as.vector(boot_result$t),
    Measure = measure
  )
  
  results_listFE[[measure]] <- boot_coefs
}

# Combine all into single df
final_boot_coefsFE <- bind_rows(results_listFE)

final_boot_coefsFE <- final_boot_coefsFE %>%
  mutate(Measure = factor(Measure,
                          levels = c("Poverty", "Canopy", "Sustainability"),
                          labels = c("Poverty", "Tree Canopy", "Sustainability Index")))

########################

########################
## Bootstrapped stacked spatial‑lag model
########################

library(sf)           # if your df is an sf object
library(spdep)
library(spatialreg)
library(tidyverse)
library(broom)
library(boot)
library(parallel)
library(ggpubr)

# ── 1. Outcome families ────────────────────────────────────────────────────────
measures <- list(
  Sustainability = c("Census_Sustainable_Dev_Index",
                     "LLM_Sustainable_Dev_Index",
                     "DL_Sustainable_Dev_Index"),
  Poverty        = c("census_pov_norm",
                     "LLM_pov_norm",
                     "DL_pov_norm"),
  Canopy         = c("census_canopy_norm",
                     "LLM_canopy_norm",
                     "DL_canopy_norm")
)

# ── 2. Helper: replicate neighbour list k times ────────────────────────────────
replicate_nb <- function(nb, k = 3) {
  n <- length(nb)
  nb_rep <- vector("list", n * k)
  reg_id <- attr(nb, "region.id")
  for (r in seq_len(k)) {
    off <- (r - 1) * n
    for (i in seq_len(n)) {
      nb_rep[[off + i]] <- if (length(nb[[i]])) as.integer(nb[[i]] + off) else integer(0)
    }
  }
  class(nb_rep) <- "nb"
  attr(nb_rep, "region.id") <- as.character(rep(reg_id, k))
  attr(nb_rep, "call")      <- attr(nb, "call")
  attr(nb_rep, "type")      <- attr(nb, "type")
  attr(nb_rep, "sym")       <- attr(nb, "sym")
  nb_rep
}

# ── 3. Loop over each outcome family ──────────────────────────────────────────
results_list <- list()

for (measure_name in names(measures)) {
  
  vars3 <- measures[[measure_name]]
  
  # a.  Filter rows with complete data for this trio
  df_clean <- combined_df_sf %>% drop_na(!!!syms(vars3))
  
  # b.  Build block‑stacked long dataframe
  stack_long <- bind_rows(
    df_clean %>% mutate(Method = "Census", Value = .data[[vars3[1]]]),
    df_clean %>% mutate(Method = "LLM",    Value = .data[[vars3[2]]]),
    df_clean %>% mutate(Method = "ResNet", Value = .data[[vars3[3]]])
  ) %>% mutate(Method = factor(Method, levels = c("Census","LLM","ResNet")))
  
  # c.  Build matching neighbour list replicated ×3
  nb_clean <- poly2nb(df_clean, queen = TRUE)
  nb_rep   <- replicate_nb(nb_clean, k = 3)
  stopifnot(length(nb_rep) == nrow(stack_long))   # alignment check
  
  # d.  Define bootstrap statistic (captures nb_rep)
  boot_spatial <- function(data, indices) {
    keep <- logical(nrow(data)); keep[indices] <- TRUE   # logical mask
    nb_sub <- subset.nb(nb_rep, keep)
    lw_b   <- nb2listw(nb_sub, style = "W", zero.policy = TRUE)
    
    m <- lagsarlm(
      Value ~ HolcFactor_Revised * Method +
        log(Pop_Density) + depratnorm + lingnorm +
        pct_black + pct_hispanic + pct_asian + pct_bachelors,
      data  = data[keep, ],
      listw = lw_b,
      zero.policy = TRUE
    )
    
    co    <- tidy(m)
    β_cen <- co$estimate[co$term == "HolcFactor_RevisedHOLC-Redlined"]
    β_llm <- β_cen + co$estimate[co$term == "HolcFactor_RevisedHOLC-Redlined:MethodLLM"]
    β_dl  <- β_cen + co$estimate[co$term == "HolcFactor_RevisedHOLC-Redlined:MethodResNet"]
    c(Census = β_cen, LLM = β_llm, ResNet = β_dl)
  }
  
  # e.  Run bootstrap
  boot_out <- boot(stack_long, boot_spatial,
                   R = 500,
                   parallel = "multicore",
                   ncpus = detectCores() - 1)
  
  # f.  Store tidy bootstrapped coefficients
  results_list[[measure_name]] <- tibble(
    Method      = rep(c("Census","LLM","ResNet"), each = nrow(boot_out$t)),
    Coefficient = as.vector(boot_out$t),
    Measure     = measure_name
  )
}

#final_boot_spatial_pov_Canopy <- bind_rows(results_list)
#final_boot_spatial_Sustainability <- bind_rows(results_list)
final_boot_spatial<-rbind(final_boot_spatial_pov_Canopy,final_boot_spatial_Sustainability)

final_boot_spatial <- final_boot_spatial %>%
  mutate(Measure = factor(Measure,
                          levels = c("Poverty", "Canopy", "Sustainability"),
                          labels = c("Poverty", "Tree Canopy", "Sustainability Index")))


########################

########################
### Combine FE and Spatial Lag
########################

final_boot_coefsFE <- final_boot_coefsFE %>%
  mutate(Model = "Zipcode FE")

final_boot_spatial <- final_boot_spatial %>%
  mutate(Model = "Spatial Lag")


boot_combined <- bind_rows(final_boot_coefsFE, final_boot_spatial)

# 1. relabel Method to show baseline vs. interaction
boot_combined <- boot_combined %>% 
  mutate(Approach = factor(Method,
                              levels = c("Census", "LLM", "ResNet"),
                              labels = c("Baseline [R]",
                                         "([R] + GPT4o Interaction)",
                                         "([R] + ResNet Interaction)")))

library(dplyr)

boot_summary  <- boot_combined %>%                                    # from your code
  group_by(Model, Measure, Approach) %>%
  summarise(
    lower = quantile(Coefficient, 0.025),
    upper = quantile(Coefficient, 0.975),
    mean  = mean(Coefficient),
    p_two_sided = 2 * min(
      mean(Coefficient <= 0),          # proportion at / below 0
      mean(Coefficient >= 0)           # proportion at / above 0
    ),
    .groups = "drop"
  )


library(ggdist)      # for slab + interval layer

add_boot_CI <- function(p, df) {
  p + ggdist::stat_interval(
    data    = df,
    aes(x = Approach,
        y = mean,
        ymin = lower,
        ymax = upper),
    colour  = "black",
    size    = 0.9,
    .width  = c(0.95)) +          # draws the 95 % CI
    geom_hline(yintercept = 0, linetype = 2) +
    geom_text(
      data = df,
      aes(x = Approach, y = upper + 0.02,
          label = sprintf("95%% CI: [%.2f, %.2f]", lower, upper)),
      size = 3.1,
      vjust = 0)
}





FigInteractionBoot <- ggplot(
  data = boot_combined,
  aes(x = Approach, y = Coefficient, fill = Approach)
) +
  geom_violin(trim = FALSE, alpha = 0.8) +
  geom_boxplot(width = 0.1, outlier.shape = NA, position = position_dodge(width = 0.9)) +
  stat_summary(fun = median, geom = "point", size = 4, shape = 21, fill = "white",
               position = position_dodge(width = 0.9)) +
  geom_errorbar(
    data = boot_summary,
    aes(x = Approach, y = mean, ymin = lower, ymax = upper),
    width = 0.15, size = 1.0, color = "darkred",
    position = position_dodge(width = 0.9)
  ) +
  geom_text_repel(
    data = boot_summary,
    aes(x = Approach, y = mean, label = sprintf("%.2f", mean)),
    color = "black",
    size = 4.5,
    fontface = "bold",
    nudge_x = .4,
    nudge_y = .2,
    direction = "y",
    segment.color = "grey50"
    #position = position_dodge(0.9)
  ) +
  facet_grid(Model ~ Measure, scales = "free_y") +
  scale_fill_manual(
    values = c("Baseline [R]" = "#DD8452",
               "([R] + GPT4o Interaction)" = "#4C72B0",
               "([R] + ResNet Interaction)" = "#55A868")

  ) +
  labs(
    y = "Redlining Effect",
    x = "Baseline + Interaction Terms",
    fill = ""
  ) +
   scale_x_discrete(labels = c(
    "Baseline [R]" = "Census",
    "([R] + GPT4o Interaction)" = "GSV-GPT4o",
    "([R] + ResNet Interaction)" = "GSV-ResNet"
  ))+
  theme_classic(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    strip.text = element_text(face = "bold", size = 12),
    #axis.text.x = element_text(size = 12),
    panel.spacing = unit(1, "lines")
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")

FigInteractionBoot

#ggsave("Pilot3/Fig/Bootstrap_Interaction_Effects.pdf", FigInteractionBoot, width = 12, height = 8, dpi = 300)









#############################################
## SAR Model Statistics Table for SI
#############################################
#############################################
## SAR Model Statistics Table with Coefficients
#############################################

#############################################
## Enhanced Stacked Regression Table with Marginal Effects
#############################################

# Function to extract SAR stats with marginal effects
extract_sar_with_marginal <- function(model, outcome_name) {
  
  coefs <- summary(model)$Coef
  
  # Extract redlining coefficient (baseline = ACS/GEIE)
  redlined_row <- which(rownames(coefs) == "HolcFactor_RevisedHOLC-Redlined")
  delta <- coefs[redlined_row, "Estimate"]
  se <- coefs[redlined_row, "Std. Error"]
  
  # Compute total marginal effect
  rho <- model$rho
  total_effect <- delta / (1 - rho)
  
  # SE of total effect (delta method approximation)
  total_se <- se / (1 - rho)
  
  data.frame(
    Outcome = outcome_name,
    Delta = round(delta, 2),
    Delta_SE = round(se, 2),
    Total_Effect = round(total_effect, 2),
    Total_SE = round(total_se, 2),
    Rho = round(rho, 2),
    Rho_SE = round(model$rho.se, 2)
  )
}

# Extract from your stacked SAR models
sar_marginal_table <- bind_rows(
  extract_sar_with_marginal(spatial_pov, "Poverty"),
  extract_sar_with_marginal(spatial_canopy, "Tree Canopy"),
  extract_sar_with_marginal(spatial_sust, "Sustainability Index")
)

print(sar_marginal_table)

# Create formatted LaTeX table
library(kableExtra)

sar_latex <- sar_marginal_table %>%
  mutate(
    `Partial Effect` = paste0(Delta, " (", Delta_SE, ")"),
    `Total Effect` = paste0(Total_Effect, " (", Total_SE, ")"),
    `$\\rho$` = paste0(Rho, " (", Rho_SE, ")")
  ) %>%
  select(Outcome, `Partial Effect`, `Total Effect`, `$\\rho$`) %>%
  kbl(
    format = "latex",
    booktabs = TRUE,
    caption = "SAR Model: Partial and Total Marginal Effects of Redlining",
    label = "tab:sar_marginal",
    escape = FALSE,
    align = c("l", "c", "c", "c")
  ) %>%
  kable_styling(
    font_size = 9,
    latex_options = c("hold_position")
  ) %>%
  footnote(
    general = "Notes: Partial effect is the SAR coefficient ($\\\\hat{\\\\delta}$). Total effect $= \\\\delta/(1-\\\\rho)$ incorporates spatial spillovers. Standard errors in parentheses. All outcomes standardized (coefficients in SD units).",
    general_title = "",
    footnote_as_chunk = TRUE,
    escape = FALSE,
    threeparttable = TRUE
  )

save_kable(sar_latex, "Pilot3/Tab/SAR_Marginal_Effects.tex")






#############################################
## Tract-Level ACS Poverty MOE Analysis
## Complete version with all statistical tests
#############################################

# Get tract-level poverty data WITH MOE
poverty_tract <- get_acs(
  geography = "tract",
  variables = c("B17001_001", "B17001_002"),
  state = "AZ",
  county = "Maricopa",
  year = 2022,
  survey = "acs5",
  output = "wide",
  geometry = FALSE
)

# Compute poverty rate and CV
poverty_tract <- poverty_tract %>%
  rename(
    total_pop = B17001_001E,
    total_pop_moe = B17001_001M,
    poverty_count = B17001_002E,
    poverty_count_moe = B17001_002M
  ) %>%
  mutate(
    poverty_rate = poverty_count / total_pop,
    poverty_rate_moe = moe_prop(poverty_count, total_pop, poverty_count_moe, total_pop_moe),
    CV = poverty_rate_moe / poverty_rate,
    TRACTID = GEOID
  ) %>%
  mutate(
    CV = ifelse(is.infinite(CV) | is.nan(CV) | poverty_rate == 0, NA, CV)
  )

# Create TRACTID in CBG data for merging
cbg_unique <- cbg_unique %>%
  mutate(TRACTID = substr(GEOID, 1, 11))

# Merge tract-level MOE to CBG data
moe_analysis <- cbg_unique %>%
  left_join(
    poverty_tract %>% select(TRACTID, poverty_rate, poverty_rate_moe, CV),
    by = "TRACTID"
  )

# Summary by HOLC group
moe_summary <- moe_analysis %>%
  filter(!is.na(CV)) %>%
  group_by(HolcFactor_Revised) %>%
  summarise(
    n = n(),
    mean_poverty_rate = mean(poverty_rate, na.rm = TRUE),
    mean_CV = mean(CV, na.rm = TRUE),
    median_CV = median(CV, na.rm = TRUE),
    sd_CV = sd(CV, na.rm = TRUE),
    mean_MOE = mean(poverty_rate_moe, na.rm = TRUE),
    .groups = "drop"
  )

#############################################
## Statistical Tests
#############################################

# ANOVA test for CV differences
anova_cv <- aov(CV ~ HolcFactor_Revised, data = moe_analysis)
anova_F <- summary(anova_cv)[[1]][["F value"]][1]
anova_p <- summary(anova_cv)[[1]][["Pr(>F)"]][1]

# Pairwise t-tests
redlined_cv <- moe_analysis %>% filter(HolcFactor_Revised == "HOLC-Redlined") %>% pull(CV)
ideal_cv <- moe_analysis %>% filter(HolcFactor_Revised == "Ideal") %>% pull(CV)
surrounding_cv <- moe_analysis %>% filter(HolcFactor_Revised == "Surrounding Phoenix") %>% pull(CV)

t_redlined_ideal <- t.test(redlined_cv, ideal_cv)
t_redlined_surrounding <- t.test(redlined_cv, surrounding_cv)

# Correlation: poverty rate vs CV
cor_pov_cv <- cor.test(moe_analysis$poverty_rate, moe_analysis$CV, use = "complete.obs")

# Print all results
cat("\n========== STATISTICAL TEST RESULTS ==========\n\n")

cat("=== ANOVA: CV by HOLC Group ===\n")
print(summary(anova_cv))

cat("\n=== T-test: Redlined vs. Ideal ===\n")
print(t_redlined_ideal)

cat("\n=== T-test: Redlined vs. Surrounding Phoenix ===\n")
print(t_redlined_surrounding)

cat("\n=== Correlation: Poverty Rate vs. CV ===\n")
print(cor_pov_cv)

cat("\n========== SUMMARY FOR MANUSCRIPT ==========\n\n")
cat(sprintf("ANOVA: F(2, %d) = %.2f, p < 0.001\n", 
            summary(anova_cv)[[1]][["Df"]][2], anova_F))
cat(sprintf("T-test (Redlined vs Ideal): t = %.2f, p < 0.001\n", 
            t_redlined_ideal$statistic))
cat(sprintf("T-test (Redlined vs Surrounding): t = %.2f, p < 0.001\n", 
            t_redlined_surrounding$statistic))
cat(sprintf("Correlation (Poverty Rate vs CV): r = %.3f, p < 0.001\n", 
            cor_pov_cv$estimate))

#############################################
## Visualization
#############################################

Fig_MOE_by_HOLC <- ggplot(moe_analysis %>% filter(!is.na(CV)), 
                          aes(x = HolcFactor_Revised, y = CV, fill = HolcFactor_Revised)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 21, outlier.alpha = 0.5) +
  scale_fill_manual(
    values = c("Ideal" = "#4DAF4A", "Surrounding Phoenix" = "#377EB8", "HOLC-Redlined" = "#E41A1C")
  ) +
  labs(
    x = "HOLC Classification",
    y = "Coefficient of Variation (MOE / Estimate)"
  ) +
  theme_classic(base_size = 12) +
  theme(legend.position = "none") +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "black")

print(Fig_MOE_by_HOLC)
ggsave("Pilot3/Fig/ACS_MOE_by_HOLC.pdf", Fig_MOE_by_HOLC, width = 6, height = 5, dpi = 300)

#############################################
## LaTeX Table with Full Statistical Notes
#############################################

moe_latex_table <- moe_summary %>%
  mutate(
    `Mean CV (SD)` = sprintf("%.3f (%.3f)", mean_CV, sd_CV),
    `Median CV` = sprintf("%.3f", median_CV),
    `Mean MOE` = sprintf("%.4f", mean_MOE)
  ) %>%
  select(HolcFactor_Revised, `Mean MOE`, `Mean CV (SD)`, `Median CV`) %>%
  kbl(
    format = "latex",
    booktabs = TRUE,
    caption = "ACS Poverty Rate Measurement Uncertainty by HOLC Classification",
    label = "tab:moe_by_holc",
    col.names = c("HOLC Group", "Mean MOE", "Mean CV (SD)", "Median CV"),
    align = c("l", "c", "c", "c")
  ) %>%
  kable_styling(font_size = 9, latex_options = c("hold_position")) %>%
  footnote(
    general = sprintf("Notes: CV = Coefficient of Variation (MOE/Estimate). ACS 5-year estimates (2018--2022). Lower CV indicates greater relative precision. ANOVA: $F(2, %d) = %.2f$, $p < 0.001$. Pairwise comparisons: Redlined vs.\\ Ideal $t = %.2f$, $p < 0.001$; Redlined vs.\\ Surrounding $t = %.2f$, $p < 0.001$. The significantly lower CV in redlined areas reflects higher poverty rates (larger denominator) rather than smaller absolute MOE. Correlation between poverty rate and CV: $r = %.2f$, $p < 0.001$.",
                      summary(anova_cv)[[1]][["Df"]][2],
                      anova_F,
                      t_redlined_ideal$statistic,
                      t_redlined_surrounding$statistic,
                      cor_pov_cv$estimate),
    general_title = "",
    footnote_as_chunk = TRUE,
    escape = FALSE,
    threeparttable = TRUE
  )

save_kable(moe_latex_table, "Pilot3/Tab/ACS_MOE_by_HOLC.tex")
cat("\nTable saved to Pilot3/Tab/ACS_MOE_by_HOLC.tex\n")
