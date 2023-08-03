###########################################################
## Read in Data, Shapefiles and Mapped Objects
###########################################################

# Required Packages
suppressPackageStartupMessages({
  library(feasts)
  library(tsibble)
  library(lubridate)
  library(ggplot2)
  library(viridis)
  library(lubridate)
  library(ggExtra)
  library(tidyr)
  library(dygraphs)
  library(xts)
  library(tidyverse)
  library(readxl)
  library(writexl)
  library(janitor)
  library(ggpubr)
  library(rgdal)
  library(readr)
  library(stringi)
  library(tidycensus)
  library(tigris)
  library(sf)
  library(rprojroot)
  library(data.table)
  library(wesanderson)
  library(Rtsne)
  library(RColorBrewer)
  library(gganimate)
  library(gifski)
  library(shiny)
  library(shinydashboard)
  library(shinyWidgets)
  library(leaflet)
  library(dplyr)
  library(plotly)
  library(xtable)
  library(DT)
  library(shinythemes)
  library(htmltools)
  library(maps)
  
  library(cdlTools) # converting state fips to state name
  options(tigris_use_cache  = TRUE)
})


#-# moving average function

ma <- function(x, n = 3){stats::filter(x, rep(1 / n, n), sides = 1)}

# Read in data for WWTP Address and Centroid for Counties


WWTP <- read_excel("Data/WWTP/WWTP_location.xlsx")


# Get state boundaries
us_states <- states()

# Filter for Texas
texas_boundary <- us_states[us_states$STUSPS == "TX", ]


CountyWWTP = WWTP[-1,] %>% 
  group_by(County, county_centroid_lon, county_centroid_lat) %>% 
  summarize(totalWWTP = n_distinct(WWTP))

names(CountyWWTP)[1] = "NAMELSAD"
CountyWWTP$Color = c("red","lightgreen","lightblue","orange","yellow","darkblue")
CountyWWTP$Radius = c(12,16,20,8,8,8)
CountyWWTP$Weight = rep(3,6)
CountyWWTP$FillOpacity = rep(0.8,6)


county_data_shp <- tigris::counties(state = "TX", year=2021)


merged_CountyWWTP = left_join(county_data_shp,CountyWWTP, by = "NAMELSAD")

merged_CountyWWTP$Weight = replace(merged_CountyWWTP$Weight, is.na(merged_CountyWWTP$Weight), 0)
merged_CountyWWTP$Radius = replace(merged_CountyWWTP$Radius, is.na(merged_CountyWWTP$Radius), 0)
merged_CountyWWTP$FillOpacity = replace(merged_CountyWWTP$FillOpacity, is.na(merged_CountyWWTP$FillOpacity), 0)
merged_CountyWWTP$Color = replace(merged_CountyWWTP$Color, is.na(merged_CountyWWTP$Color), "#F5DEB3")


county_data_shp$wwtp = rep(0,length(county_data_shp$NAMELSAD))

county_data_shp$wwtp <- ifelse(county_data_shp$NAMELSAD == "Harris County", 9,
                                 ifelse(county_data_shp$NAMELSAD == "El Paso County", 4,
                                        ifelse(county_data_shp$NAMELSAD == "Fort Bend County", 1,
                                          ifelse(county_data_shp$NAMELSAD == "Cameron County", 2,
                                                 ifelse(county_data_shp$NAMELSAD == "Lubbock County", 1,
                                                        ifelse(county_data_shp$NAMELSAD == "Wichita County", 1, 0))))))




##################### READ IN METADATA TABLE ###########
#-# load all batches of files

### taxonomical profiles
## list of relevant files in the directory
tax_files <- list.files(sprintf(
  "%s/Data/taxonomical_profiles", 
  find_rstudio_root_file()), 
  pattern = "*.tax.tsv", full.names = TRUE)

## load and combine all as one table
comb_tax_table <- rbindlist(lapply(tax_files, fread))

### metadata
## list of relevant files in the directory
metadata_files <- list.files(sprintf(
  "%s/Data/metadata", 
  find_rstudio_root_file()), 
  pattern = "*.xlsx", full.names = TRUE)

## load and combine all as one table
comb_metadata_table <- rbindlist(lapply(metadata_files, read_excel))

colnames(comb_metadata_table) = c("sample_ID", "Site", "City", "Date", "Flow")

comb_metadata_table$Date <- as.Date(comb_metadata_table$Date)


### codes
## replace real site names with codes 
code_dt <- read_excel(sprintf("%s/Data/site_coding/WWTP_codes1.xlsx", find_rstudio_root_file()))

    
comb_metadata_table <- merge(comb_metadata_table, code_dt, 
                             by.x = "Site", by.y = "Name", all.x = T)

comb_metadata_table$Site <- comb_metadata_table$Code

comb_metadata_table <- comb_metadata_table %>% 
  select(-Code)

### genome coverage
# ## list of relevant files in the directory
# coverage_files <- list.files(sprintf(
#   "Data/genome_coverage", 
#   find_rstudio_root_file()), 
#   pattern = "*.100windows.mean_cov.tsv", full.names = TRUE)
# 
# ## load and combine all as one table
# comb_coverage_table <- rbindlist(
#   lapply(coverage_files, 
#          fread, header = F, 
#          col.names = 
#            c("sample_ID", "accession", "start_base", "end_base", "mean_depth")))
# 

#-# load all batches of qPCR Data

## list of relevant files in the directory
qPCR_files <- list.files(sprintf(
  "%s/Data/qPCR", 
  find_rstudio_root_file()), 
  pattern = "*.csv", full.names = TRUE)
#qPCR_files
## load and combine all as one table
comb_qPCR_table <- rbindlist(lapply(qPCR_files, fread, colClasses = "character" ))


comb_qPCR_table$date_of_collection <- as.POSIXct(comb_qPCR_table$date_of_collection)


## load abbreviations -> sites/cities table
abbr_dt <- read_excel(sprintf("%s/Data/site_coding/Sites_and_abbreviations.xlsx", find_rstudio_root_file()))

# fix all the formatting, average genome copies

comb_qPCR_table <- merge(comb_qPCR_table, abbr_dt,  by = "LocationAbbr") %>%
  mutate(Week = floor_date(as.Date(date_of_collection), "weeks", week_start = 1),
         copiesperml = as.numeric(copiesperml)) %>%
  group_by(LocationAbbr, CMMR_Barcode, Target, SampleName, Week, City) %>%
  summarize(average_genome_copies_L = mean(copiesperml)) %>%
  ungroup()

qPCR_ma_p <- comb_qPCR_table %>%
  #filter(City != "other") %>%
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



## calculate unique cities
virome_cities <- comb_metadata_table %>% 
  ungroup() %>%
  filter(City != "other") %>% 
  select(City) %>%
  unique()


# Load data for US states
states <- map_data("state")

# Filter for Texas
texas <- subset(states, region == "texas")

texas_sf <- st_as_sf(texas, coords = c("long", "lat"), crs = 4326)

#-# prepare table for major pathogen moving average plot

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
                        "Human mastadenovirus B", "Hepatovirus A", 
                        "Human respirovirus 1", "Human respirovirus 3")) %>%
  group_by(sample_ID, City, Week, species) %>%
  summarize(RPKM = sum(RPKM)) %>%
  ungroup() %>%
  group_by(species, City) %>%
  mutate(rel_ab = RPKM/sum(RPKM)) %>%
  ungroup()

#-# Parse to expand but then only keep sampled dates

city_dates <- comb_metadata_table %>%
  mutate(Week = floor_date(Date, "weeks", week_start = 1)) %>%
  select(c(City, Week)) %>%
  distinct()

major_path_expand_dt <- major_path_met_dt %>%
  group_by(Week, City, species) %>%
  summarize(rel_ab = sum(rel_ab),
            RPKM = mean(RPKM)) %>%
  ungroup() %>%
  complete(Week, City, species, fill = list(rel_ab = 0, RPKM = 0)) %>%
  group_by(City, species) %>%
  arrange(City, species, Week) %>%
  mutate(moving_average = ma(RPKM)) %>%
  ungroup() %>%
  mutate(species = gsub("Enterovirus D", "Enterovirus D68", species),
         species = gsub("Norwalk virus", "Noroviruses", species),
         species = gsub("Severe acute respiratory syndrome-related coronavirus",
                        "SARS-CoV-2", species),
         species = gsub("Hepatovirus A", "Hepatitis A Virus", species),
         species = gsub("Human respirovirus 3", "Parainfluenza Virus 3", species),
         species = gsub("Human respirovirus 1", "Parainfluenza Virus 1", species),
         species = gsub("Human mastadenovirus B", "Human Adenovirus B", species))

major_path_expand_dt <- merge(major_path_expand_dt, city_dates, by = c("City", "Week"))

major_path_expand_dt$City = gsub(",.*$", "", major_path_expand_dt$City)
