######################################
# R script to process and clean data #
#####################################

### Combine daily data into one dataset
#sheet names indicating data collection date
sheet_names <- excel_sheets("WW_data/WeeklyReports_ElPaso_CMMR(R)_2022.xlsx")
sheet_names

## read in and combine daily data first
dat <- lapply(sheet_names[-c(29,49,60,61)], function(x) { as.data.frame(read_excel("WW_data/WeeklyReports_ElPaso_CMMR(R)_2022.xlsx", sheet = x)) } ) # 29 is repeated measure for 11/9/2021
comb.dat <-bind_rows(dat)[,-c(14,15)] %>% filter(`WWTP Site` %in% c("Fred Hurvey","Haskell","John T. Hickerson","Roberto Bustamante")) %>%
  rename(`Copies per L waste water (N1)`=`Copies/ L of wastewater (N1)`,
         `Copies per L waste water (N2)`=`Copies/ L of wastewater (N2)`) %>%
  mutate(`Date Collected`=as.Date(substr(`Sample Name`,4,11), format="%m/%d/%y"))
## read in cumulative data sheet
# sheet 60: Cumulative Maresso from 2020 and 2021
FH.dat <- read_excel("WW_data/WeeklyReports_ElPaso_CMMR(R)_2022.xlsx", sheet=60, range="A6:I19") %>% mutate(`WWTP Site`="Fred Hurvey", `AVG Flowrate (MGD)`=6.05,`Date Collected`=as.Date(substr(`Sample Name`,4,11), format="%m/%d/%y"))
HS.dat <- read_excel("WW_data/WeeklyReports_ElPaso_CMMR(R)_2022.xlsx", sheet=60, range="K6:S19") %>% mutate(`WWTP Site`="Haskell", `AVG Flowrate (MGD)`=13.16,`Date Collected`=as.Date(substr(`Sample Name`,4,11), format="%m/%d/%y"))
JT.dat <- read_excel("WW_data/WeeklyReports_ElPaso_CMMR(R)_2022.xlsx", sheet=60, range="U6:AC18") %>% mutate(`WWTP Site`="John T. Hickerson",`AVG Flowrate (MGD)`=7.89,`Date Collected`=as.Date(substr(`Sample Name`,4,11), format="%m/%d/%y"))
RB.dat <- read_excel("WW_data/WeeklyReports_ElPaso_CMMR(R)_2022.xlsx", sheet=60, range="K6:S19") %>% mutate(`WWTP Site`="Roberto Bustamante",`AVG Flowrate (MGD)`=28.75,`Date Collected`=as.Date(substr(`Sample Name`,4,11), format="%m/%d/%y"))

# sheet 48: Cumulative CMMR from 2020 (6/22/2020 - 11/4/2020) - different format
# need to reformat the data
cmmr <- read_excel("WW_data/WeeklyReports_ElPaso_CMMR(R)_2022.xlsx", sheet=61,range="A2:G99") %>%
  mutate(`WWTP Site`=ifelse(`Sample Point WWTP RAW`=="FH", "Fred Hurvey", 
                            ifelse(`Sample Point WWTP RAW`=="HS", "Haskell", 
                                   ifelse(`Sample Point WWTP RAW`=="JT", "John T. Hickerson", "Roberto Bustamante"))),
         `Sample ID`=paste(`Sample Point WWTP RAW`, substr(`EP ID`, 2, 9), sep=" "),
         `Sample Name`=paste0(Concat, "/20"),
         `N1 Cт`=`AVG N1`, `N2 CT`=`AVG N2`)

# combine all data together
dat.processed <- bind_rows(comb.dat, FH.dat, HS.dat, JT.dat, RB.dat, cmmr[,c(2,4,8:12)])
# save the processed data 
writexl::write_xlsx(dat.processed, "WeeklyReports_ElPaso_CMMR(R)_2022_Processed.xlsx")  


# read in and process case data  - 7/21/2022
#site1
dat1 <- read_excel("Case_data/WWTP_ZCTA_COVIDCASE11_1_2020-6_13_22_Modified2.xlsx", sheet=2, range="A14:VS17")[,-2]
# convert to data frame
dat1 <- as.data.frame(t(dat1), stringsAsFactors=FALSE)
names(dat1) <- dat1[1,]; dat1 <- dat1[-1,]; rownames(dat1) <- 1:nrow(dat1)
# clean Date variable
dat1$Date<-excel_numeric_to_date(as.numeric(as.character(dat1$Date)), date_system = "modern")
dat1$WWTP <- "Roberto Bustamante"
dat1 <- dat1 %>% rename(Normalized=`Normalized (per 100,000)`)

#site2
dat2 <- read_excel("Case_data/WWTP_ZCTA_COVIDCASE11_1_2020-6_13_22_Modified2.xlsx", sheet=4, range="A8:VS11")[,-2]
# convert to data frame
dat2 <- as.data.frame(t(dat2), stringsAsFactors=FALSE)
names(dat2) <- dat2[1,]; dat2 <- dat2[-1,]; rownames(dat2) <- 1:nrow(dat2)
# clean Date variable
dat2$Date<-excel_numeric_to_date(as.numeric(as.character(dat2$Date)), date_system = "modern")
dat2$WWTP <- "John T. Hickerson"

#site3
dat3 <- read_excel("Case_data/WWTP_ZCTA_COVIDCASE11_1_2020-6_13_22_Modified2.xlsx", sheet=6, range="A4:VS7")[,-2]
# convert to data frame
dat3 <- as.data.frame(t(dat3), stringsAsFactors=FALSE)
names(dat3) <- dat3[1,]; dat3 <- dat3[-1,]; rownames(dat3) <- 1:nrow(dat3)
# clean Date variable
dat3$Date<-excel_numeric_to_date(as.numeric(as.character(dat3$Date)), date_system = "modern")
dat3$WWTP <- "Fred Hurvey"

#site4
dat4 <- read_excel("Case_data/WWTP_ZCTA_COVIDCASE11_1_2020-6_13_22_Modified2.xlsx", sheet=8, range="A11:VS14")[,-2]
# convert to data frame
dat4 <- as.data.frame(t(dat4), stringsAsFactors=FALSE)
names(dat4) <- dat4[1,]; dat4 <- dat4[-1,]; rownames(dat4) <- 1:nrow(dat4)
# clean Date variable
dat4$Date<-excel_numeric_to_date(as.numeric(as.character(dat4$Date)), date_system = "modern")
dat4$WWTP <- "Haskell"


# combine all together
case_data <- bind_rows(dat1, dat2, dat3, dat4) %>% rename(ncase=`Total # Cases`, rate=Normalized) %>%
  mutate(ncase=as.numeric(ncase), rate=round(as.numeric(rate),2))


writexl::write_xlsx(case_data, "WWTP_CaseRate_processed_updated.xlsx")









