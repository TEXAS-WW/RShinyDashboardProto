###########################################################
## Read in Data, Shapefiles and Mapped Objects
###########################################################

# Required Packages
source("variables.R")
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
  library(reactable)
  library(DT)
  library(shinythemes)
  library(htmltools)
  library(maps)
  library(reactablefmtr)
  library(gganimate)
  library(sparkline)
 # install_github("timelyportfolio/dataui")
  library(dataui)
  library(devtools)
  library(viridis)
  library(scales)
  library(gifski)
  library(cdlTools) # converting state fips to state name
  options(tigris_use_cache  = TRUE)
})


#-# moving average function

ma <- function(x, n = 3){stats::filter(x, rep(1 / n, n), sides = 1)}

# Read in data for WWTP Address and Centroid for Counties

## list of relevant files in the directory
WWTP_files <- list.files(sprintf(
  "%s/Data/site_coding", 
  find_rstudio_root_file()), 
  pattern = "location.*\\.xlsx$", full.names = TRUE)


## load and combine all as one table
WWTP <- rbindlist(lapply(WWTP_files, read_excel), fill= TRUE)
print("TESTING WWTP dataframe - DataPreprocessing.R - line 76")
print(WWTP)

WWTP <- as.data.frame(WWTP)

colnames(WWTP) = c("State", "County", "City", "WWTP", "lat","lon","county_centroid_lat","county_centroid_lon","city_centroid_lat",
                   "city_centroid_lon","Radius", "Color", "Weight", "FillOpacity")

### site_codes
## replace real site names with codes 
code_dt <- read_excel(sprintf("%s/Data/site_coding/WWTP_codes1.xlsx", find_rstudio_root_file()))

## load abbreviations -> sites/cities table
abbr_dt <- read_excel(sprintf("%s/Data/site_coding/Sites_and_abbreviations.xlsx", find_rstudio_root_file()))

# Get state boundaries
us_states <- states()

# Filter for Texas
texas_boundary <- us_states[us_states$STUSPS == "TX", ]


CountyWWTP = WWTP[-1,] %>% 
  group_by(County, county_centroid_lon, county_centroid_lat, Color, Radius, Weight, FillOpacity) %>% 
  summarize(totalWWTP = n_distinct(WWTP)) 

names(CountyWWTP)[1] = "NAMELSAD"
# CountyWWTP$Color = c("red","lightgreen","lightblue","orange","yellow","darkblue")
# CountyWWTP$Radius = c(12,16,20,8,8,8)
# CountyWWTP$Weight = rep(3,6)
# CountyWWTP$FillOpacity = rep(0.8,6)


county_data_shp <- tigris::counties(state = "TX", year=2021)


merged_CountyWWTP = left_join(county_data_shp,CountyWWTP, by = "NAMELSAD")

merged_CountyWWTP$Weight = replace(merged_CountyWWTP$Weight, is.na(merged_CountyWWTP$Weight), 0)
merged_CountyWWTP$Radius = replace(merged_CountyWWTP$Radius, is.na(merged_CountyWWTP$Radius), 0)
merged_CountyWWTP$FillOpacity = replace(merged_CountyWWTP$FillOpacity, is.na(merged_CountyWWTP$FillOpacity), 0)
merged_CountyWWTP$Color = replace(merged_CountyWWTP$Color, is.na(merged_CountyWWTP$Color), "#F5DEB3")
merged_CountyWWTP$totalWWTP = replace(merged_CountyWWTP$totalWWTP, is.na(merged_CountyWWTP$totalWWTP), 0)

# 
# #county_data_shp$wwtp = rep(0,length(county_data_shp$NAMELSAD))
# 
# #county_data_shp$wwtp <- ifelse(county_data_shp$NAMELSAD == "Harris County", 9,
#                                  ifelse(county_data_shp$NAMELSAD == "El Paso County", 4,
#                                         ifelse(county_data_shp$NAMELSAD == "Fort Bend County", 1,
#                                           ifelse(county_data_shp$NAMELSAD == "Cameron County", 2,
#                                                  ifelse(county_data_shp$NAMELSAD == "Lubbock County", 1,
#                                                         ifelse(county_data_shp$NAMELSAD == "Wichita County", 1, 0))))))




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
comb_tax_table <- as.data.frame(comb_tax_table)

print("TESTING TAXONOMICAL PROFILE")
print(comb_tax_table)
### metadata
## list of relevant files in the directory
metadata_files <- list.files(sprintf(
  "%s/Data/metadata", 
  find_rstudio_root_file()), 
  pattern = "*.xlsx", full.names = TRUE)

## load and combine all as one table
comb_metadata_table <- rbindlist(lapply(metadata_files, read_excel))
comb_metadata_table <- as.data.frame(comb_metadata_table)

colnames(comb_metadata_table) = c("sample_ID", "Site", "City", "Date", "Flow", "PoolID")

comb_metadata_table$Date <- as.Date(comb_metadata_table$Date)




comb_metadata_table <- merge(comb_metadata_table, code_dt, 
                             by.x = "Site", by.y = "Name", all.x = T)

comb_metadata_table$Site <- comb_metadata_table$Code

comb_metadata_table <- comb_metadata_table %>% 
  mutate(sample_ID = paste(sample_ID, PoolID, sep = ".")) %>%
  select(-c(Code, PoolID))

### genome coverage
## list of relevant files in the directory
coverage_files <- list.files(sprintf(
  "%s/Data/genome_coverage", 
  find_rstudio_root_file()), 
  pattern = "*.mean_cov.tsv", full.names = TRUE)


## load and combine all as one table
comb_coverage_table <- rbindlist(
  lapply(coverage_files, 
         fread, header = F, 
         col.names = 
           c("sample_ID", "accession", "start_base", "end_base", "mean_depth")))
comb_coverage_table <- as.data.frame(comb_coverage_table)




#-# load all batches of qPCR Data

## list of relevant files in the directory
qPCR_files <- list.files(sprintf(
  "%s/Data/qPCR", 
  find_rstudio_root_file()), 
  pattern = "*.csv", full.names = TRUE)

#qPCR_files
## load and combine all as one table
comb_qPCR_table <- rbindlist(lapply(qPCR_files, fread, colClasses = "character" ))

comb_qPCR_table <- as.data.frame(comb_qPCR_table)


comb_qPCR_table$date_of_collection <- as.POSIXct(comb_qPCR_table$date_of_collection)


# fix all the formatting, average genome copies

comb_qPCR_table <- merge(comb_qPCR_table, abbr_dt,  by = "LocationAbbr") %>%
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

### Coordinates, Texas Cities with WWTPs in TEPHI program

invisible(capture.output(
  cities <- st_read(sprintf(
    "%s/Data/geographical_files/Texas_Cities/City.shp", 
    find_rstudio_root_file())) %>% 
    filter(CITY_NM %in% WWTP_cities)%>% 
    st_cast("POINT") %>% as("Spatial") 
))




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

# Load data for US states
states <- map_data("state")

# Filter for Texas
texas <- subset(states, region == "texas")

texas_sf <- st_as_sf(texas, coords = c("long", "lat"), crs = 4326)





# qPCR (Targeted): Quantification of Specific Pathogens in Wastewater



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




## Collection Dates and Sites for all Comprehensive Deep Sequencing Samples




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
                        "Human orthopneumovirus",
                        "Human mastadenovirus B", "Hepatovirus A", 
                        "Human respirovirus 1", "Human respirovirus 3")) %>%
  group_by(sample_ID, City, Week, species) %>%
  summarize(RPKMF = sum(RPKMF)) %>%
  ungroup() %>%
  group_by(species, City) %>%
  mutate(rel_ab = RPKMF/sum(RPKMF)) %>%
  ungroup()



# major_path_met_dt <- merge(comb_tax_table, comb_metadata_table, 
#                            by = "sample_ID") %>%
#   mutate(Week = floor_date(Date, "weeks", week_start = 1)) %>%
#   group_by(City) %>%
#   filter(n_distinct(Week) >= 3) %>%
#   ungroup() %>%
#   filter(species %in% c("Norwalk virus", "Enterovirus D", "Rotavirus A", 
#                         "Influenza A virus", 
#                         "Severe acute respiratory syndrome-related coronavirus", 
#                         "Monkeypox virus", "Respiratory syncytial virus",
#                         "Human orthopneumovirus",
#                         "Human mastadenovirus B", "Hepatovirus A", 
#                         "Human respirovirus 1", "Human respirovirus 3")) %>%
#   group_by(sample_ID, City, Week, species) %>%
#   summarize(RPKMF = sum(RPKMF)) %>%
#   ungroup() %>%
#   group_by(species, City) %>%
#   mutate(rel_ab = RPKMF/sum(RPKMF)) %>%
#   ungroup()



#-# Parse to expand but then only keep sampled dates



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

# city_dates <- comb_metadata_table %>%
#   mutate(Week = floor_date(Date, "weeks", week_start = 1)) %>%
#   select(c(City, Week)) %>%
#   distinct()
# 
# major_path_expand_dt <- major_path_met_dt %>%
#   group_by(Week, City, species) %>%
#   summarize(rel_ab = sum(rel_ab),
#             RPKMF = mean(RPKMF)) %>%
#   ungroup() %>%
#   complete(Week, City, species, fill = list(rel_ab = 0, RPKMF = 0)) %>%
#   group_by(City, species) %>%
#   arrange(City, species, Week) %>%
#   mutate(moving_average = ma(RPKMF)) %>%
#   ungroup() %>%
#   mutate(species = gsub("Enterovirus D", "Enterovirus D68", species),
#          species = gsub("Norwalk virus", "Noroviruses", species),
#          species = gsub("Severe acute respiratory syndrome-related coronavirus",
#                         "SARS-CoV-2", species),
#          species = gsub("Hepatovirus A", "Hepatitis A Virus", species),
#          species = gsub("Human respirovirus 3", "Parainfluenza Virus 3", species),
#          species = gsub("Human respirovirus 1", "Parainfluenza Virus 1", species),
#          species = gsub("Human mastadenovirus B", "Human Adenovirus B", species),
#          species = gsub("Respiratory syncytial virus", "Respiratory syncytial virus A", species),
#          species = gsub("Human orthopneumovirus", "Respiratory syncytial virus B", species))
# 
# major_path_expand_dt <- merge(major_path_expand_dt, city_dates, by = c("City", "Week"))
# 


major_path_expand_dt$City = gsub(",.*$", "", major_path_expand_dt$City)


minDate_cds <- as.Date(min(major_path_expand_dt$Week, na.rm = TRUE))
maxDate_cds <- as.Date(max(major_path_expand_dt$Week, na.rm = TRUE))

minDate_qpcr<- as.Date(min(qPCR_ma_p$Week, na.rm = TRUE))
maxDate_qpcr <- as.Date(max(qPCR_ma_p$Week, na.rm = TRUE))




# DATA PREPARATION FOR INTERACTIVE TABLE

#-# Make reactables for major pathogens format tables
#format coverage for reactable
comb_coverage_table$mean_depth <- round(comb_coverage_table$mean_depth)


sum_coverage <- comb_coverage_table %>%
  group_by(sample_ID, accession) %>%
  summarize(coverage = list(mean_depth)) 

sum_coverage <- setDT(sum_coverage)

#merge tax profile, metadata, and coverage

genome_data <- merge(comb_tax_table, comb_metadata_table, 
                     by = "sample_ID") %>%
  filter(species %in% c("Enterovirus D", #"Rotavirus A", 
                        "Influenza A virus", 
                        "Severe acute respiratory syndrome-related coronavirus", 
                        "Monkeypox virus", "Respiratory syncytial virus", 
                        "Human orthopneumovirus",
                        "Human mastadenovirus B", "Hepatovirus A", 
                        "Human respirovirus 1", "Human respirovirus 3"))


combined_react_data <- merge(genome_data, sum_coverage, 
                             by = c("sample_ID","accession"))



combined_react_data <- subset(combined_react_data, select = c("sample_ID", "Site", "City", "Date", "accession", "sequence_name", "reference_length", "RPKMF", "covered_bases", "coverage"))
combined_react_data$Percent_covered <- combined_react_data$covered_bases / combined_react_data$reference_length

combined_react_data <- subset(combined_react_data, select = c("sample_ID", "Site", "City", "Date", "sequence_name", "accession", "reference_length", "Percent_covered", "RPKMF", "coverage"))


#### INTERACTIVE TABLE 

trend_major_path_dt <- major_path_expand_dt %>%
  group_by(City, species) %>%
  mutate(most_recent_week = last(Week),
         four_weeks_before = (most_recent_week - 28)) %>%
  filter(Week == most_recent_week |
           Week == four_weeks_before) %>%
  filter(n() == 2) %>%
  mutate(observation_label = case_when(
    Week == most_recent_week ~ "Most Recent Week",
    Week == four_weeks_before ~ "Four Weeks Earlier",
    TRUE ~ "other"),
    difference = (moving_average[Week == most_recent_week] / moving_average[Week == four_weeks_before]) -1
  ) %>%
  mutate(Interpretation = case_when(
    moving_average[Week == most_recent_week] > 0 & moving_average[Week == four_weeks_before] > 0 & difference > 0.25 ~ "Increase from Baseline",
    moving_average[Week == most_recent_week] > 0 & moving_average[Week == four_weeks_before] > 0 & difference < -0.25 ~ "Decrease from Baseline",
    moving_average[Week == most_recent_week] > 0 & moving_average[Week == four_weeks_before] > 0 & difference >= -0.25 & difference <= 0.25 ~ "Little Change",
    moving_average[Week == most_recent_week] == 0 & moving_average[Week == four_weeks_before] == 0  ~ "Constant at 0",
    moving_average[Week == most_recent_week] > 0 & moving_average[Week == four_weeks_before] == 0  ~ "(Re)-emerging from 0",
    moving_average[Week == most_recent_week] == 0 & moving_average[Week == four_weeks_before] > 0  ~ "Going to 0",
    TRUE ~ "other"
  ))


trend_major_path_dt %>%
  select(c(City, Week, most_recent_week, species, difference, Interpretation, moving_average)) %>%
  filter(Week == most_recent_week) %>%
  distinct() %>%
  mutate(interpretation_color = case_when(
    Interpretation == "Increase from Baseline" ~ "orangered",
    Interpretation == "Decrease from Baseline" ~ "cadetblue",
    Interpretation == "Little Change" ~ "grey60",
    Interpretation == "Constant at 0" ~ "white",
    Interpretation == "(Re)-emerging from 0" ~ "purple",
    Interpretation == "Going to 0" ~ "blue",
    TRUE ~ "black"
  ),
  difference = case_when(
    #difference == "-Inf" ~ 0,
    difference == "NaN" ~ 0,
    Interpretation == "Going to 0" ~ -1,
    Interpretation == "(Re)-emerging from 0" ~ 1,
    TRUE ~ difference
  ),
  difference = difference * 100) %>%
  arrange(desc(difference))


#-# Calculate tSNE

comb_tax_table$RPKMF <- as.numeric(comb_tax_table$RPKMF)

seqname_sID_wide_dt <- 
  comb_tax_table %>% 
  subset(select = c("sequence_name", "sample_ID", "RPKMF")) %>%
  distinct(sequence_name, sample_ID, RPKMF) %>%
  group_by(sequence_name, sample_ID) %>%
  summarize(RPKMF = mean(RPKMF)) %>%
  pivot_wider(names_from = sequence_name, values_from = RPKMF, values_fill = 0)

sampleID_l <- seqname_sID_wide_dt$sample_ID

seqname_sID_wide_dt <- seqname_sID_wide_dt %>% subset(select = -sample_ID)

##
tephi_prcomp1 <- prcomp(log10(seqname_sID_wide_dt+1))

emb <- Rtsne::Rtsne(tephi_prcomp1$x[,1:10])

embb <- emb$Y

rownames(embb) <- sampleID_l

embb_df <- as.data.frame(embb)

embb_dt <- setDT(embb_df, keep.rownames = "sample_ID")

embb_dt <- merge(embb_dt, comb_metadata_table, by ="sample_ID")

#embb_dt$City_Site <- str_c(embb_dt$City, ", ", embb_dt$Site)

embb_dt$Date <- as.Date(embb_dt$Date, "%m-%d-%Y")

# 
# # Prepare data for tSNE
# 
# comb_tax_table$RPKMF <- as.numeric(comb_tax_table$RPKMF)
# 
# seqname_sID_wide_dt <- 
#   comb_tax_table %>% 
#   subset(select = c("sequence_name", "sample_ID", "RPKMF")) %>%
#   distinct(sequence_name, sample_ID, RPKMF) %>%
#   group_by(sequence_name, sample_ID) %>%
#   summarize(RPKMF = mean(RPKMF)) %>%
#   pivot_wider(names_from = sequence_name, values_from = RPKMF, values_fill = 0)
# 
# sampleID_l <- seqname_sID_wide_dt$sample_ID
# 
# seqname_sID_wide_dt <- seqname_sID_wide_dt %>% subset(select = -sample_ID)
# 
# ##
# tephi_prcomp1 <- prcomp(log10(seqname_sID_wide_dt+1))
# 
# emb <- Rtsne::Rtsne(tephi_prcomp1$x[,1:10])
# 
# embb <- emb$Y
# 
# rownames(embb) <- sampleID_l
# 
# embb_df <- as.data.frame(embb)
# 
# embb_dt <- setDT(embb_df, keep.rownames = "sample_ID")
# 
# embb_dt <- merge(embb_dt, comb_metadata_table, by ="sample_ID")
# 
# #embb_dt$City_Site <- str_c(embb_dt$City, ", ", embb_dt$Site)
# 
# embb_dt$Date <- as.Date(embb_dt$Date, "%m-%d-%Y")
# 
# as.Date_origin <- function(x){
#   as.Date(x, origin = '1970-01-01')
# }
# 
# embb_dt$City_Date <- str_c(embb_dt$City, ", ", embb_dt$Date)
# 
# 
# 

anim_date_tsnep <- embb_dt %>%
  arrange(Date) %>%
  ggplot(aes(x=V1, y=V2, color=as.integer(Date), group = Site)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_path(linewidth = 1.1, alpha = 0.8) +
  scale_color_gradient(low = "#FDD262",
                       high = "#3F3F7B") +
  theme_bw() +
  facet_wrap(~City) +
  labs(x="t-SNE 1", y="t-SNE 2") +
  transition_reveal(along = as.integer(Date)) 


# 
# 
# anim_date_tsnep <- embb_dt %>%
#   arrange(Date) %>%
#   ggplot(aes(x=V1, y=V2, color=as.integer(Date), group = Site)) +
#   geom_point(size = 3, alpha = 0.8) +
#   geom_path(linewidth = 1.1, alpha = 0.8) +
#   scale_color_gradient(low = "#FDD262",
#                        high = "#3F3F7B", labels=as.Date_origin, name = "Date") +
#   theme_bw() +
#   facet_wrap(~City) +
#   labs(x="t-SNE 1", y="t-SNE 2") +
#   transition_reveal(along = as.integer(Date)) 








##### preparing data to make a pulldown to check any virus species
species_abund_dt <- merge(comb_tax_table, comb_metadata_table, 
                          by = "sample_ID") %>%
  mutate(Week = floor_date(Date, "weeks", week_start = 1)) %>%
  group_by(City) %>%
  filter(n_distinct(Week) >= 3) %>%
  ungroup() %>%
  group_by(Site, species, Week) %>%
  summarize(RPKMS = sum(RPKMF),
            City = first(City)) %>%
  ungroup() %>%
  group_by(City, species, Week) %>%
  summarize(RPKMFS = mean(RPKMS)) %>%
  ungroup() %>%
  complete(nesting(Week, City), species, fill = list(RPKMFS=0)) %>%
  group_by(City, species) %>%
  arrange(City, species, Week) %>%
  mutate(moving_average = ma(RPKMFS)) %>%
  ungroup()


prevalent_sp_dt <- merge(species_abund_dt, city_dates, by = c("City", "Week")) %>%
  group_by(species) %>%
  mutate(nonzero = sum(RPKMFS != 0)) %>%
  filter(nonzero >=10) %>%
  select(-nonzero) %>%
  ungroup() %>%
  arrange(City, species, Week)
# 
# 
# species_abund_dt <- merge(comb_tax_table, comb_metadata_table, 
#                           by = "sample_ID") %>%
#   mutate(Week = floor_date(Date, "weeks", week_start = 1)) %>%
#   group_by(City) %>%
#   filter(n_distinct(Week) >= 3) %>%
#   ungroup() %>%
#   group_by(Site, species, Week) %>%
#   summarize(RPKMS = sum(RPKMF),
#             City = first(City)) %>%
#   ungroup() %>%
#   group_by(City, species, Week) %>%
#   summarize(RPKMFS = mean(RPKMS)) %>%
#   ungroup() %>%
#   complete(nesting(Week, City), species, fill = list(RPKMFS=0)) %>%
#   group_by(City, species) %>%
#   arrange(City, species, Week) %>%
#   mutate(moving_average = ma(RPKMFS)) %>%
#   ungroup()
# 
# 
# prevalent_sp_dt <- merge(species_abund_dt, city_dates, by = c("City", "Week")) %>%
#   group_by(species) %>%
#   mutate(nonzero = sum(RPKMFS != 0)) %>%
#   filter(nonzero >=10) %>%
#   select(-nonzero) %>%
#   ungroup() %>%
#   arrange(City, species, Week)




