###########################################################
## Read in Data, Shapefiles and Mapped Objects
###########################################################

# Load required packages for data manipulation and visualization
#source("variables.R")  # Source global variables defined in external file for consistency
suppressPackageStartupMessages({
  # Load a series of libraries without showing any package startup messages
  library(feasts)  # For time series features
  library(tsibble)  # For handling time series data in a tidy format
  library(lubridate)  # For date-time manipulation
  library(ggplot2)  # For creating static visualizations
  library(viridis)  # For color scales that are perceptually uniform in both colour and greyscale
  library(ggExtra)  # For adding marginal histograms to ggplot2, and more ggplot2 enhancements
  library(tidyr)  # For data tidying
  library(dygraphs)  # For interactive time series charting
  library(xts)  # For handling time-series data
  library(tidyverse)  # For easy data manipulation and visualization
  library(readxl)  # For reading Excel files
  library(writexl)  # For writing Excel files
  library(janitor)  # For cleaning up messy data
  library(ggpubr)  # For 'ggplot2' based publication ready plots
  library(readr)  # For reading rectangular data
  library(stringi)  # For fast, correct string manipulation
  library(tidycensus)  # For loading US Census data
  library(tigris)  # For loading geographic data from the US Census Bureau
  library(sf)  # For simple features, a standardized way to encode spatial vector data
  library(rprojroot)  # For finding the root of a project directory
  library(data.table)  # For enhanced data manipulation
  library(wesanderson)  # For Wes Anderson color palettes
  library(Rtsne)  # For T-distributed Stochastic Neighbor Embedding (t-SNE)
  library(RColorBrewer)  # For color palettes
  library(gganimate)  # For animating ggplot2 visualizations
  library(gifski)  # For rendering animations to GIF
  library(shiny)  # For building interactive web apps straight from R
  library(shinydashboard)  # For creating dashboards with 'Shiny'
  library(shinyWidgets)  # For custom widgets like buttons, sliders, etc.
  library(leaflet)  # For interactive maps
  library(dplyr)  # For data manipulation
  library(plotly)  # For creating interactive web graphics via 'plotly.js'
  library(xtable)  # For exporting tables to LaTeX or HTML
  library(reactable)  # For creating interactive data tables
  library(DT)  # For rendering data tables
  library(shinythemes)  # For additional themes for 'Shiny' apps
  library(htmltools)  # For HTML rendering tools
  library(maps)  # For drawing geographical maps
  library(reactablefmtr)  # For formatting options for reactable
  library(sparkline)  # For generating sparklines in 'Shiny' applications
  library(devtools)
  #devtools::install_github("timelyportfolio/dataui") use this to install devtools
  library(dataui)
  library(dataui)  # For interactive web data components
  library(devtools)  # For development tools
  library(scales)  # For graphical scales mapping data to aesthetics
  library(cdlTools)  # For converting state FIPS codes to state names
  options(tigris_use_cache = TRUE)  # Use local caching for 'tigris' data
})

# Function to calculate moving averages
ma <- function(x, n = 3) {
  # Calculate moving averages using a simple mean over n periods
  stats::filter(x, rep(1 / n, n), sides = 1)
}

# Load data related to Wastewater Treatment Plant (WWTP) locations
# This section reads multiple Excel files containing site coding, merges them, and standardizes column names
WWTP_files <- list.files(sprintf(
  "%s/Data/site_coding", 
  find_rstudio_root_file()), 
  pattern = "location.*\\.xlsx$", full.names = TRUE)
WWTP <- rbindlist(lapply(WWTP_files, read_excel), fill = TRUE)
WWTP <- as.data.frame(WWTP)
colnames(WWTP) = c("State", "County", "City", "WWTP", "lat", "lon", "county_centroid_lat", "county_centroid_lon", "city_centroid_lat",
                   "city_centroid_lon", "Radius", "Color", "Weight", "FillOpacity")

# Read and replace real site names with codes for anonymization
code_dt <- read_excel(sprintf("%s/Data/site_coding/WWTP_codes1.xlsx", find_rstudio_root_file()))
abbr_dt <- read_excel(sprintf("%s/Data/site_coding/Sites_and_abbreviations.xlsx", find_rstudio_root_file()))

# Load US state boundaries and filter for Texas
us_states <- states()
texas_boundary <- us_states[us_states$STUSPS == "TX", ]

# Aggregate data by county within Texas, merging with shapefiles for mapping
CountyWWTP <- WWTP %>% 
  group_by(County, county_centroid_lon, county_centroid_lat, Color, Radius, Weight, FillOpacity) %>% 
  summarize(totalWWTP = n_distinct(WWTP)) 
names(CountyWWTP)[1] = "NAMELSAD"
county_data_shp <- tigris::counties(state = "TX", year = 2021)
merged_CountyWWTP <- left_join(county_data_shp, CountyWWTP, by = "NAMELSAD")
merged_CountyWWTP$Weight <- replace(merged_CountyWWTP$Weight, is.na(merged_CountyWWTP$Weight), 0)
merged_CountyWWTP$Radius <- replace(merged_CountyWWTP$Radius, is.na(merged_CountyWWTP$Radius), 0)
merged_CountyWWTP$FillOpacity <- replace(merged_CountyWWTP$FillOpacity, is.na(merged_CountyWWTP$FillOpacity), 0)
merged_CountyWWTP$Color <- replace(merged_CountyWWTP$Color, is.na(merged_CountyWWTP$Color), "#F5DEB3")
merged_CountyWWTP$totalWWTP <- replace(merged_CountyWWTP$totalWWTP, is.na(merged_CountyWWTP$totalWWTP), 0)

# Read and process metadata for taxonomical and genomic data
tax_files <- list.files(sprintf(
  "%s/Data/taxonomical_profiles", 
  find_rstudio_root_file()), 
  pattern = "*.tax.tsv", full.names = TRUE)
comb_tax_table <- rbindlist(lapply(tax_files, fread))
comb_tax_table <- as.data.frame(comb_tax_table)
metadata_files <- list.files(sprintf(
  "%s/Data/metadata", 
  find_rstudio_root_file()), 
  pattern = "*.xlsx", full.names = TRUE)
comb_metadata_table <- rbindlist(lapply(metadata_files, read_excel))
comb_metadata_table <- as.data.frame(comb_metadata_table)
colnames(comb_metadata_table) = c("sample_ID", "Site", "City", "Date", "Flow", "PoolID")
comb_metadata_table$Date <- as.Date(comb_metadata_table$Date)

# Combine metadata with site codes and reformat table for further analysis
comb_metadata_table <- merge(comb_metadata_table, code_dt, by.x = "Site", by.y = "Name", all.x = TRUE)
comb_metadata_table$Site <- comb_metadata_table$Code
comb_metadata_table <- comb_metadata_table %>% 
  mutate(sample_ID = paste(sample_ID, PoolID, sep = ".")) %>%
  select(-c(Code, PoolID))

# Read genome coverage data and merge with taxonomical profiles
coverage_files <- list.files(sprintf(
  "%s/Data/genome_coverage", 
  find_rstudio_root_file()), 
  pattern = "*.mean_cov.tsv", full.names = TRUE)
comb_coverage_table <- rbindlist(
  lapply(coverage_files, 
         fread, header = FALSE, 
         col.names = c("sample_ID", "accession", "start_base", "end_base", "mean_depth")))
comb_coverage_table <- as.data.frame(comb_coverage_table)

# Load qPCR data, format, and prepare for analysis
qPCR_files <- list.files(sprintf(
  "%s/Data/qPCR", 
  find_rstudio_root_file()), 
  pattern = "*.csv", full.names = TRUE)
comb_qPCR_table <- rbindlist(lapply(qPCR_files, fread, colClasses = "character"))
comb_qPCR_table <- as.data.frame(comb_qPCR_table)
comb_qPCR_table$date_of_collection <- as.POSIXct(comb_qPCR_table$date_of_collection)
comb_qPCR_table <- merge(comb_qPCR_table, abbr_dt, by = "LocationAbbr") %>%
  mutate(Week = floor_date(as.Date(date_of_collection), "weeks", week_start = 1),
         copiesperml = as.numeric(copiesperml)) %>%
  group_by(LocationAbbr, CMMR_Barcode, Target, SampleName, Week, City) %>%
  summarize(average_genome_copies_L = mean(copiesperml)) %>%
  ungroup()



## calculate unique cities
virome_cities <- comb_metadata_table %>% 
  ungroup() %>%
  filter(City != "other") %>% 
  select(City) %>%
  unique()

qPCR_cities_dt <- comb_qPCR_table %>% 
  ungroup() %>%
  filter(City != "other") %>% 
  select(City) %>%
  unique()


total_cities <- merge(virome_cities, qPCR_cities_dt, by = "City", all = T)

WWTP_cities <- list(unique(total_cities$City))
WWTP_cities <- gsub(", TX", "", WWTP_cities[[1]])

WWTP_citieslength <- length(unique(total_cities$City))

### Coordinates, Texas Cities with WWTPs in TEPHI program

invisible(capture.output(
  cities <- st_read(sprintf(
    "%s/Data/geographical_files/Texas_Cities/City.shp", 
    find_rstudio_root_file())) %>% 
    filter(CITY_NM %in% WWTP_cities)%>% 
    st_cast("POINT") %>% as("Spatial") 
))

# Calculate unique cities for virome and qPCR analysis
# This section creates distinct lists of cities based on metadata and qPCR data, ensuring data consistency across analyses
virome_cities <- comb_metadata_table %>%
  ungroup() %>%
  filter(City != "other") %>%
  select(City) %>%
  unique()

qPCR_cities_dt <- comb_qPCR_table %>%
  ungroup() %>%
  filter(City != "other") %>%
  select(City) %>%
  unique()

# Combine the city lists into a single dataset and remove duplicates
total_cities <- merge(virome_cities, qPCR_cities_dt, by = "City", all = TRUE)
WWTP_cities <- list(unique(total_cities$City))
WWTP_cities <- gsub(", TX", "", WWTP_cities[[1]])  # Remove state suffix for cleaner city names

# Coordinate handling for Texas cities involved in the TEPHI program
# This part reads shapefiles for Texas cities, filters them by cities with WWTPs, and converts them to spatial points for mapping
invisible(capture.output(
  cities <- st_read(sprintf(
    "%s/Data/geographical_files/Texas_Cities/City.shp", 
    find_rstudio_root_file())) %>% 
    filter(CITY_NM %in% WWTP_cities) %>% 
    st_cast("POINT") %>% as("Spatial")  # Convert shape data to simple POINT type for easier handling
))

# Prepare the data tables for pathogen analysis based on sequencing and qPCR data
# This includes mapping, averaging, and normalizing the data for further analysis
major_path_met_dt <- merge(comb_tax_table, comb_metadata_table, 
                           by = "sample_ID") %>%
  mutate(Week = floor_date(Date, "weeks", week_start = 1)) %>%
  group_by(City) %>%
  filter(n_distinct(Week) >= 3) %>%
  ungroup() %>%
  filter(species %in% c("Norwalk virus", "Enterovirus D", "Rotavirus A", 
                        "Influenza A virus", 
                        "Severe acute respiratory syndrome-related coronavirus", 
                        "Monkeypox virus", "Respiratory syncytial virus",
                        "Human orthopneumovirus",
                        "Human mastadenovirus B", "Hepatovirus A", 
                        "Human respirovirus 1", "Human respirovirus 3")) %>%
  group_by(sample_ID, City, Week, species) %>%
  summarize(RPKMF = sum(RPKMF)) %>%
  ungroup() %>%
  group_by(species, City) %>%
  mutate(rel_ab = RPKMF/sum(RPKMF)) %>%
  ungroup()

# Expand pathogen data to include all weeks sampled, filling gaps where no data exists
city_dates <- comb_metadata_table %>%
  mutate(Week = floor_date(Date, "weeks", week_start = 1)) %>%
  select(c(City, Week)) %>%
  distinct()
major_path_expand_dt <- major_path_met_dt %>%
  group_by(Week, City, species) %>%
  summarize(rel_ab = sum(rel_ab),
            RPKMF = mean(RPKMF)) %>%
  ungroup() %>%
  complete(Week, City, species, fill = list(rel_ab = 0, RPKMF = 0)) %>%
  group_by(City, species) %>%
  arrange(City, species, Week) %>%
  mutate(moving_average = ma(RPKMF)) %>%
  ungroup() %>%
  mutate(species = gsub("Enterovirus D", "Enterovirus D68", species),
         species = gsub("Norwalk virus", "Noroviruses", species),
         species = gsub("Severe acute respiratory syndrome-related coronavirus",
                        "SARS-CoV-2", species),
         species = gsub("Hepatovirus A", "Hepatitis A Virus", species),
         species = gsub("Human respirovirus 3", "Parainfluenza Virus 3", species),
         species = gsub("Human respirovirus 1", "Parainfluenza Virus 1", species),
         species = gsub("Human mastadenovirus B", "Human Adenovirus B", species),
         species = gsub("Respiratory syncytial virus", "Respiratory syncytial virus A", species),
         species = gsub("Human orthopneumovirus", "Respiratory syncytial virus B", species))

major_path_expand_dt <- merge(major_path_expand_dt, city_dates, by = c("City", "Week"))

# Date range calculations for interactive displays and filtering
# These calculations find the minimum and maximum dates for comprehensive deep sequencing and qPCR datasets
minDate_cds <- as.Date(min(major_path_expand_dt$Week, na.rm = TRUE))
maxDate_cds <- as.Date(max(major_path_expand_dt$Week, na.rm = TRUE))


# qPCR (Targeted): Quantification of Specific Pathogens in Wastewater

qPCR_ma_p <- comb_qPCR_table %>%
  filter(City != "other",
         Target %in% c("SARSCOV2N1", "INFLUENZAA", "INFLUENZAB", 
                       "NOROVIRUS", "MONKEYPOX")) %>%
  mutate(Target = gsub("SARSCOV2N1", "SARS-CoV-2", Target),
         Target = gsub("INFLUENZAA", "Influenza A virus", Target),
         Target = gsub("INFLUENZAB", "Influenza B virus", Target),
         Target = gsub("NOROVIRUS", "Norovirus GII", Target),
         Target = gsub("MONKEYPOX", "Monkeypox virus", Target)) %>%
  group_by(Week, City, Target) %>%
  summarize(average_genome_copies_L = mean(average_genome_copies_L)) %>%
  ungroup() %>%
  group_by(City, Target) %>%
  filter(n_distinct(Week) >= 3) %>%
  arrange(City, Target, Week) %>%
  mutate(moving_average = ma(average_genome_copies_L)) %>%
  ungroup()

print(qPCR_ma_p)

minDate_qpcr <- as.Date(min(qPCR_ma_p$Week, na.rm = TRUE))
maxDate_qpcr <- as.Date(max(qPCR_ma_p$Week, na.rm = TRUE))


# DATA PREPARATION FOR INTERACTIVE TABLE
# Format the coverage data for reactable displays
comb_coverage_table$mean_depth <- round(comb_coverage_table$mean_depth)  # Round mean depth values for cleaner display

# Aggregate coverage data by sample and accession to create a list of mean depth values
sum_coverage <- comb_coverage_table %>%
  group_by(sample_ID, accession) %>%
  summarize(coverage = list(mean_depth)) 

sum_coverage <- setDT(sum_coverage)  # Convert to data.table for better performance on large data sets

# Merge taxonomic profile, metadata, and coverage into a single dataset
genome_data <- merge(comb_tax_table, comb_metadata_table, by = "sample_ID") %>%
  filter(species %in% c("Enterovirus D", "Influenza A virus", 
                        "Severe acute respiratory syndrome-related coronavirus", 
                        "Monkeypox virus", "Respiratory syncytial virus", 
                        "Human orthopneumovirus",
                        "Human mastadenovirus B", "Hepatovirus A", 
                        "Human respirovirus 1", "Human respirovirus 3"))

combined_react_data <- merge(genome_data, sum_coverage, by = c("sample_ID", "accession"))

# Select and reformat the data for better readability in the interactive table
combined_react_data <- subset(combined_react_data, select = c("sample_ID", "Site", "City", "Date", "accession", "sequence_name", "reference_length", "RPKMF", "covered_bases", "coverage"))
combined_react_data$Percent_covered <- combined_react_data$covered_bases / combined_react_data$reference_length  # Calculate percentage coverage
combined_react_data <- subset(combined_react_data, select = c("sample_ID", "Site", "City", "Date", "sequence_name", "accession", "reference_length", "Percent_covered", "RPKMF", "coverage"))

# INTERACTIVE TABLE PREPARATION
# Analyze pathogen trends and generate data for interactive display
trend_major_path_dt <- major_path_expand_dt %>%
  group_by(City, species) %>%
  mutate(most_recent_week = last(Week),  # Identify the most recent week of data
         four_weeks_before = (most_recent_week - 28)) %>% # Calculate the date four weeks prior for comparison
  filter(Week == most_recent_week | Week == four_weeks_before) %>% # Filter to only these two points
  filter(n() == 2)  %>% # Ensure both time points are present for each city/species pair
  mutate(observation_label = case_when(
    Week == most_recent_week ~ "Most Recent Week",
    Week == four_weeks_before ~ "Four Weeks Earlier",
    TRUE ~ "other"),
    difference = (moving_average[Week == most_recent_week] / moving_average[Week == four_weeks_before]) - 1) %>% # Calculate the percentage difference between the two time points
  mutate(Interpretation = case_when(
    moving_average[Week == most_recent_week] > 0 & moving_average[Week == four_weeks_before] > 0 & difference > 0.25 ~ "Increase from Baseline",
    moving_average[Week == most_recent_week] > 0 & moving_average[Week == four_weeks_before] > 0 & difference < -0.25 ~ "Decrease from Baseline",
    moving_average[Week == most_recent_week] > 0 & moving_average[Week == four_weeks_before] > 0 & difference >= -0.25 & difference <= 0.25 ~ "Little Change",
    moving_average[Week == most_recent_week] == 0 & moving_average[Week == four_weeks_before] == 0  ~ "Constant at 0",
    moving_average[Week == most_recent_week] > 0 & moving_average[Week == four_weeks_before] == 0  ~ "(Re)-emerging from 0",
    moving_average[Week == most_recent_week] == 0 & moving_average[Week == four_weeks_before] > 0  ~ "Going to 0",
    TRUE ~ "other"
  ))  # Interpret the changes for reporting in the table

trend_major_path_dt %>%
  select(c(City, Week, most_recent_week, species, difference, Interpretation, moving_average)) %>%
  filter(Week == most_recent_week) %>%
  distinct() %>%
  mutate(interpretation_color = case_when(  # Assign colors based on the interpretation for visual cues in the table
    Interpretation == "Increase from Baseline" ~ "orangered",
    Interpretation == "Decrease from Baseline" ~ "cadetblue",
    Interpretation == "Little Change" ~ "grey60",
    Interpretation == "Constant at 0" ~ "white",
    Interpretation == "(Re)-emerging from 0" ~ "purple",
    Interpretation == "Going to 0" ~ "blue",
    TRUE ~ "black"
  ),
  difference = case_when(
    difference == "NaN" ~ 0,  # Handle NaN values explicitly
    Interpretation == "Going to 0" ~ -1,
    Interpretation == "(Re)-emerging from 0" ~ 1,
    TRUE ~ difference
  ),
  difference = difference * 100)  %>% # Convert difference to a percentage for clarity
  arrange(desc(difference))  # Sort by difference to highlight significant changes


# Calculate tSNE for visualizing data complexity and relationships
# t-Distributed Stochastic Neighbor Embedding (t-SNE) is a machine learning algorithm for dimensionality reduction, useful for visualizing high-dimensional data
seqname_sID_wide_dt <-  comb_tax_table %>%
  subset(select = c("sequence_name", "sample_ID", "RPKMF")) %>%
  distinct(sequence_name, sample_ID, RPKMF) %>%
  group_by(sequence_name, sample_ID) %>%
  summarize(RPKMF = mean(RPKMF)) %>%
  pivot_wider(names_from = sequence_name, values_from = RPKMF, values_fill = 0)  # Convert the data to a wide format suitable for t-SNE

sampleID_l <- seqname_sID_wide_dt$sample_ID  # Store sample IDs for later use
seqname_sID_wide_dt <- seqname_sID_wide_dt %>% subset(select = -sample_ID)  # Remove sample IDs from data to prepare for t-SNE

# Perform principal component analysis (PCA) before t-SNE to reduce dimensionality and improve t-SNE performance
tephi_prcomp1 <- prcomp(log10(seqname_sID_wide_dt + 1))  # Apply log transformation to reduce skewness and stabilize variance

# Execute t-SNE on PCA results
emb <- Rtsne::Rtsne(tephi_prcomp1$x[,1:10])  # Use the first 10 principal components for t-SNE
embb <- emb$Y  # Extract t-SNE embeddings
rownames(embb) <- sampleID_l  # Assign row names to the embeddings for identification

embb_df <- as.data.frame(embb)  # Convert matrix to data frame for easier handling
embb_dt <- setDT(embb_df, keep.rownames = "sample_ID")  # Convert to data.table with sample IDs
embb_dt <- merge(embb_dt, comb_metadata_table, by ="sample_ID")  # Merge t-SNE results with metadata for contextual information
embb_dt$Date <- as.Date(embb_dt$Date, "%m-%d-%Y")  # Convert date strings to Date objects

# Create an animated plot of t-SNE results over time using ggplot2 and gganimate
anim_date_tsnep <- embb_dt %>%
  arrange(Date) %>%
  ggplot(aes(x = V1, y = V2, color = as.integer(Date), group = Site)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_path(linewidth = 1.1, alpha = 0.8) +
  scale_color_gradient(low = "#FDD262", high = "#3F3F7B") +  # Color gradient for visual appeal
  theme_bw() +  # Use a minimal theme for clarity
  facet_wrap(~City) +  # Facet by city to compare different locations
  labs(x = "t-SNE 1", y = "t-SNE 2") +  # Label axes
  transition_reveal(along = as.integer(Date))  # Animate transitions along the Date

# Prepare data for species abundance analysis over time in various cities
# This block calculates the abundance of different virus species and visualizes changes over time
species_abund_dt <- merge(comb_tax_table, comb_metadata_table, by = "sample_ID") %>%
  mutate(Week = floor_date(Date, "weeks", week_start = 1)) %>%
  group_by(City) %>%
  filter(n_distinct(Week) >= 3) %>%
  ungroup() %>%
  group_by(Site, species, Week) %>%
  summarize(RPKMS = sum(RPKMF), City = first(City)) %>%
  ungroup() %>%
  group_by(City, species, Week) %>%
  summarize(RPKMFS = mean(RPKMS)) %>%
  ungroup() %>%
  complete(nesting(Week, City), species, fill = list(RPKMFS = 0)) %>%
  group_by(City, species) %>%
  arrange(City, species, Week) %>%
  mutate(moving_average = ma(RPKMFS)) %>%
  ungroup()

# Final processing and visualization of prevalent species data
prevalent_sp_dt <- merge(species_abund_dt, city_dates, by = c("City", "Week")) %>%
  group_by(species) %>%
  mutate(nonzero = sum(RPKMFS != 0)) %>%
  filter(nonzero >= 10) %>%  # Filter to include only species with significant presence
  select(-nonzero) %>%
  ungroup() %>%
  arrange(City, species, Week)




#average dates with multiple observations of percentage coverage (same date, city, and variant) from combined_react_data for plotting
combined_react_data_plot <- combined_react_data %>%
  group_by(sequence_name, City, Date) %>%
  summarize(Percent_covered = mean(Percent_covered, na.rm = TRUE)) %>%
  ungroup()

unique(combined_react_data_plot$sequence_name)



#Make the variant names more user friendly on the plot
name_mapping <- c(
  "Enterovirus D68 isolate 2011-21186, complete genome" = "Enterovirus",                                                  
  "HPIV-1 strain Washington/1964, complete genome" = "HPIV-1",                                                       
  "Hepatitis A virus isolate 18f, complete genome" = "Hepatitis A",                                                       
  "Human adenovirus 11 strain HAdV-B2/USA_GA/8348/2019/P11H11F12, complete genome" = "Human adenovirus 11",                       
  "Human adenovirus 14 strain HAdV14-BJ430/CHN2010, complete genome" = "Human adenovirus 14",                                     
  "Human adenovirus 16 strain E26, complete genome" = "Human adenovirus 16",                                                      
  "Human adenovirus 21 isolate OHT-006, complete genome" = "Human adenovirus 21",                                                 
  "Human adenovirus 3+11p strain OvAd1, complete genome" = "Human adenovirus 3+11p",                                                 
  "Human adenovirus 7 strain BJ/CHN/2018, complete genome" = "Human adenovirus 7",                                               
  "Human adenovirus B3 strain T382/Ft Jackson South Carolina USA/2002, complete genome" = "Human adenovirus B3",                  
  "Human adenovirus type 34 strain Compton, complete genome" = "Human adenovirus 34",                                             
  "Human adenovirus type 35, complete genome" = "Human adenovirus 35",                                                            
  "Human hepatitis A virus HA16-1496 genomic RNA, complete genome" = "Human hepatitis A virus HA16-1496 genomic RNA",                                       
  "Human orthopneumovirus Subgroup B, complete genome" = "Human orthopneumovirus Subgroup B",                                                   
  "Human parainfluenza virus 3 strain 14702, complete genome" = "Human parainfluenza virus 3 strain 14702",                                            
  "Human parainfluenza virus 3 strain ZHYMgz01, complete genome" = "Human parainfluenza virus 3 strain ZHYMgz01",                                         
  "Human respiratory syncytial virus MinB, complete genome" = "Human respiratory syncytial virus MinB",                                              
  "Influenza A virus (A/swine/Mexico/AVX49/2013(H1N2)) segment 1 polymerase PB2 (PB2) gene, complete cds" = "Influenza A virus (A/swine/Mexico/AVX49/2013(H1N2)) segment 1 polymerase PB2 (PB2) gene, complete cds",
  "Monkeypox virus Zaire-96-I-16, complete genome" = "Monkeypox virus Zaire-96-I-16",                                                       
  "Respiratory syncytial virus, complete genome" = "Respiratory syncytial virus",                                                         
  "Severe acute respiratory syndrome coronavirus 2 isolate Wuhan-Hu-1, complete genome" = "SARS-CoV-2",                  
  "Simian adenovirus 33, complete genome" = "Simian adenovirus 33",                                                                
  "influenza A H1N1 all segments" = "Influenza A H1N1 all segments",                                                                        
  "influenza A H3N2 all segments" = "Influenza A H3N2 all segments"
)

# Rename the sequence_name column using the named vector
combined_react_data_plot <- combined_react_data_plot %>%
  mutate(sequence_name = name_mapping[sequence_name])







source("server.R")
source("ui.R")
source("variables.R")
options(shiny.autoreload = TRUE)

### POTENTIAL IMPROVEMENT: DEBOUNCE()
# helper_files = list.files(path = "helper", full.names = TRUE, pattern = "*.R")
# sapply(helper_files, source, encoding = "UTF-8")
# 
# firstArg = NULL
# 
# args = commandArgs(trailingOnly=TRUE)
# 
# # test if there is at least one argument: if not, return an error
# if (length(args)==0) {
#   print("No arguments were supplied ... moving on")
# } else if (length(args)==1) {
#   #
#   firstArg = args[1]
# }



minDate_cds <- as.Date(min(major_path_expand_dt$Week, na.rm = TRUE))
maxDate_cds <- as.Date(max(major_path_expand_dt$Week, na.rm = TRUE))

minDate_qpcr<- as.Date(min(qPCR_ma_p$Week, na.rm = TRUE))
maxDate_qpcr <- as.Date(max(qPCR_ma_p$Week, na.rm = TRUE))

options(shiny.port = 8100)
shinyApp(ui = ui, server = server)
