require(tidyverse)
install.packages("tidycensus")
library(tidycensus)
library(readr)
require(dplyr)

# Import NYC puma codes
PUMA_CD_crosswalk <- read_csv("~/Google Drive/ANHD/Risk Chart 2021/PUMA CD crosswalk.csv")
pumanums <- as.character(PUMA_CD_crosswalk$PUMA)

cdsnyc <- read_csv('../PUMA CD crosswalk.csv',
                   col_names = TRUE,
                   cols(puma = col_character(),
                        cd_lu = col_character(),
                        cd_adj = col_character()))


# All ACS 2019 1-year estimates variables
v19_1 <- load_variables(2019, "acs1", cache = TRUE)

# Median income

# B06011_001	Estimate!!Median income in the past 12 months --!!Total:
# Get NYS puma data for median income
income19 <- get_acs(geography = "public use microdata area", 
                         variables = "B06011_001", 
                         state =  "New York",
                         survey = "acs1",
                         year = 2019)

# substring for new puma field of 4 digits for joins
income19$puma <-  substr(income19$GEOID,4,8) # New column of 4 digit puma codes

# Filter Income by NYS PUMA data for just NYC
income19nyc <- inner_join(income19, cdsnyc, 
                          by = "puma")


# pull avg household size for NYC pumas 
# B25010_001	Estimate!!Average household size --!!Total:

hhsize19 <- get_acs(geography = "public use microdata area", 
                    variables = "B25010_001", 
                    state =  "New York",
                    survey = "acs1",
                    year = 2019)

# substring for new puma field of 4 digits for joins
hhsize19$puma <-  substr(hhsize19$GEOID,4,8) # New column of 4 digit puma codes

# Filter Income by NYS PUMA data for just NYC
hhsize19nyc <- inner_join(hhsize19, cdsnyc, 
                          by = "puma")

# New table of name, puma, median HH income, and average household size
ami19 <- left_join (
        income19nyc %>% dplyr::select(puma,NAME,estimate,moe),
        hhsize19nyc %>% dplyr::select(puma,estimate,moe),
        by = "puma"
        ) %>%

rename(
       medinc_e = estimate.x,
       medinc_m = moe.x,
       avgsize_e = estimate.y,
       avgsize_m = moe.y) %>%
distinct()

# Pull 2019 income limits from https://www.huduser.gov/portal/datasets/il/il2019/2019summary.odn for AMI calculation
# 4-person 50% AMI = 53350 | 100% = 106700

# Calculate pro-rated AMI based on household size

ami19$proratedami <- (ami19$medinc_e*100/(106700-((0.1*106700)*(4-ami19$avgsize_e))))

view(ami19)

ami19 <- ami19 %>%
  left_join(., cdsnyc, 
  by = "puma")


# Pct service workers

# 2020 criteria: 
# 	Estimate!!Total:!!Service occupations:!!Healthcare support occupations
# 	Estimate!!Total:!!Service occupations:!!Food preparation and serving related occupations
# 	Estimate!!Total:!!Service occupations:!!Building and grounds cleaning and maintenance occupations
# 	Estimate!!Total:!!Production, transportation, and material moving occupations:

# 2021 criteria (includes police & firefighters in aggregate service occupations)
# 	Estimate!!Total:!!Service occupations:
# 	Estimate!!Total:!!Production, transportation, and material moving occupations:


# The variables in tidycensus weren't working for occupations, there was missing data at the PUMA level, so instead:
# Downloaded S2406 from Census ACS 19-1yr: https://data.census.gov/cedsci/table?t=Occupation&g=0400000US36.795000&y=2019&tid=ACSST1Y2019.S2406

occupationbyclass <- read_csv("~/Google\ Drive/ANHD/Risk\ Chart\ 2021/Census/occupation\ by\ class\ of\ worker/ACSST1Y2019.S2406_data_with_overlays_2021-04-21T105321.csv")

# substring for new puma field of 4 digits for joins
occupationbyclass$puma <- substr(occupationbyclass$GEO_ID,13,16)     

# Filter Transportation Workers by NYS PUMA data for just NYC
occupationbyclass_nyc <- inner_join(occupationbyclass, cdsnyc, 
                                    by = "puma")

# select only variables we need
serviceworkers <- select(occupationbyclass_nyc,
                         NAME,
                         puma,
                         S2406_C01_001E, # Estimate!!Total!!Civilian employed population 16 years and over
                         S2406_C01_001M, # MOE ""
                         S2406_C01_003E, # Estimate!!Total!!Civilian employed population 16 years and over!!Service occupations
                         S2406_C01_003M, # MOE ""
                         S2406_C01_006E, # Estimate!!Total!!Civilian employed population 16 years and over!!Production, transportation, and material moving occupations
                         S2406_C01_006M, # MOE ""
                        ) 
# change all columns to numeric data type
serviceworkers$S2406_C01_001E <- as.numeric(serviceworkers$S2406_C01_001E)
serviceworkers$S2406_C01_001M <- as.numeric(serviceworkers$S2406_C01_001M)
serviceworkers$S2406_C01_003E <- as.numeric(serviceworkers$S2406_C01_003E)
serviceworkers$S2406_C01_003M <- as.numeric(serviceworkers$S2406_C01_003M)
serviceworkers$S2406_C01_006E <- as.numeric(serviceworkers$S2406_C01_006E)
serviceworkers$S2406_C01_006M <- as.numeric(serviceworkers$S2406_C01_006M)

# calculate % service + production/transport workers out of total
serviceworkers$pctsvc <-  (serviceworkers$S2406_C01_003E*100 + serviceworkers$S2406_C01_006E)/serviceworkers$S2406_C01_001E
  
serviceworkers <- serviceworkers %>%
  left_join(., cdsnyc,
            by = "puma")


#Overcrowding

#	B25014_001	Estimate!!Total:	TENURE BY OCCUPANTS PER ROOM
#	B25014_006	Estimate!!Total:!!Owner occupied:!!1.51 to 2.00 occupants per room	TENURE BY OCCUPANTS PER ROOM
#	B25014_007	Estimate!!Total:!!Owner occupied:!!2.01 or more occupants per room	TENURE BY OCCUPANTS PER ROOM
#	B25014_012	Estimate!!Total:!!Renter occupied:!!1.51 to 2.00 occupants per room	TENURE BY OCCUPANTS PER ROOM
#	B25014_013	Estimate!!Total:!!Renter occupied:!!2.01 or more occupants per room	TENURE BY OCCUPANTS PER ROOM

# Last year used 1-year estimates but the margins of error are too high; using 5-year instead

totaloccupants <- get_acs(geography = "public use microdata area", 
                    variables = "B25014_001", 
                    state =  "New York",
                    year = 2019,
                    survey = "acs5")

owner15_20 <- get_acs(geography = "public use microdata area", 
                      variables = "B25014_006", 
                      state =  "New York",
                      year = 2019,
                      survey = "acs5")

owner20plus <- get_acs(geography = "public use microdata area", 
                       variables = "B25014_007", 
                       state =  "New York",
                       year = 2019,
                       survey = "acs5")

renter15_20 <- get_acs(geography = "public use microdata area", 
                       variables = "B25014_012", 
                       state =  "New York",
                       year = 2019,
                       survey = "acs5")

renter20plus <- get_acs(geography = "public use microdata area", 
                        variables = "B25014_013", 
                        state =  "New York",
                        year = 2019,
                        survey = "acs5")


overcrowding19 <- 
  
  left_join(totaloccupants %>% dplyr::select(GEOID, NAME, estimate, moe), 
            owner15_20 %>% dplyr::select(GEOID, estimate, moe),
            by = "GEOID",
            keep = FALSE,
            copy = FALSE,
  ) %>%
  left_join (. , 
             owner20plus %>% dplyr::select(GEOID, estimate, moe),
             by = "GEOID",
             keep = FALSE,
             copy = FALSE,
  ) %>%
  left_join (. , 
             renter15_20 %>% dplyr::select(GEOID, estimate, moe),
             by = "GEOID",
             keep = FALSE,
             copy = FALSE,
  ) %>%
  left_join (. , 
             renter20plus %>% dplyr::select(GEOID, estimate, moe),
             by = "GEOID",
             keep = FALSE,
             copy = FALSE,
  ) %>%
  rename(total_e = 3,
         total_m = 4,
         owner15_20_e = 5,
         owner15_20_m = 6,
         owner20plus_e = 7,
         owner20plus_m = 8,
         renter15_20_e = 9,
         renter15_20_m = 10,
         renter20plus_e = 11,
         renter20plus_m = 12)

# substring for new puma field of 4 digits for joins
overcrowding19$puma <-  substr(overcrowding19$GEOID,4,8) # New column of 4 digit puma codes

# Filter Income by NYS PUMA data for just NYC
overcrowding19nyc <- inner_join(overcrowding19, cdsnyc, 
                                by = "puma")

# Calculate severe overcrowding as owners and renters with 1.5+ occupants per room out of total 
overcrowding19nyc$pctsevere <- (overcrowding19nyc$owner15_20_e + overcrowding19nyc$owner20plus_e + overcrowding19nyc$renter15_20_e + overcrowding19nyc$renter20plus_e)*100 / overcrowding19nyc$total_e

view(overcrowding19nyc)

overcrowding19nyc <- overcrowding19nyc %>%
  left_join(.,
            cdsnyc,
            by = "puma")

# RACE
# Calculated as everybody minus anyone who identifies as white alone
# B02001_001	Estimate!!Total:
# B02001_002	Estimate!!Total:!!White alone	

# grab variables for all of NYS
totalpop <- get_acs(geography = "public use microdata area", 
                    variables = "B02001_001", 
                    state =  "New York",
                    year = 2019,
                    survey = "acs1")

whitealone <- get_acs(geography = "public use microdata area", 
                    variables = "B02001_002", 
                    state =  "New York",
                    year = 2019,
                    survey = "acs1")

# join all variables

poc19 <- 
  
  left_join(totalpop %>% dplyr::select(GEOID, NAME, estimate, moe), 
            whitealone %>% dplyr::select(GEOID, estimate, moe),
            by = "GEOID",
            keep = FALSE,
            copy = FALSE
  ) 


# substring for new puma field of 4 digits for joins
poc19$puma <-  substr(poc19$GEOID,4,8) # New column of 4 digit puma codes

# Filter NYS PUMA data for just NYC
poc19nyc <- inner_join(poc19, cdsnyc, 
                                by = "puma") %>%

rename(total_e = 3,
       total_m = 4,
       whitealone_e = 5,
       whitealone_m = 6) 

poc19nyc$pctpoc <- (poc19nyc$total_e-poc19nyc$whitealone_e)*100/poc19nyc$total_e


# Rent burden

# GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS
# B25070_001	Estimate!!Total:	
#	B25070_007	Estimate!!Total:!!30.0 to 34.9 percent	
#	B25070_008	Estimate!!Total:!!35.0 to 39.9 percent
#	B25070_009	Estimate!!Total:!!40.0 to 49.9 percent	
#	B25070_010	Estimate!!Total:!!50.0 percent or more	
#	B25070_011	Estimate!!Total:!!Not computed	

# grab variables for all of NYS
totalpop <- get_acs(geography = "public use microdata area", 
                    variables = "B25070_001", 
                    state =  "New York",
                    year = 2019,
                    survey = "acs1")

rent30_35 <- get_acs(geography = "public use microdata area", 
                      variables = "B25070_007", 
                      state =  "New York",
                      year = 2019,
                      survey = "acs1")

rent35_40 <- get_acs(geography = "public use microdata area", 
                   variables = "B25070_008", 
                   state =  "New York",
                   year = 2019,
                   survey = "acs1")

rent40_50 <- get_acs(geography = "public use microdata area", 
                     variables = "B25070_009", 
                     state =  "New York",
                     year = 2019,
                     survey = "acs1")


rent50plus <- get_acs(geography = "public use microdata area", 
                     variables = "B25070_010", 
                     state =  "New York",
                     year = 2019,
                     survey = "acs1")

rentnotcomputed <- get_acs(geography = "public use microdata area", 
                     variables = "B25070_011", 
                     state =  "New York",
                     year = 2019,
                     survey = "acs1")

# join all variables

rentburden19 <- 
  
  left_join(totalpop %>% dplyr::select(GEOID, NAME, estimate, moe), 
            rent30_35 %>% dplyr::select(GEOID, estimate, moe),
            by = "GEOID",
            keep = FALSE,
            copy = FALSE
  ) %>%
  left_join (. , 
             rent35_40 %>% dplyr::select(GEOID, estimate, moe),
             by = "GEOID",
             keep = FALSE,
             copy = FALSE
  ) %>%
  left_join (. , 
             rent40_50 %>% dplyr::select(GEOID, estimate, moe),
             by = "GEOID",
             keep = FALSE,
             copy = FALSE
  ) %>%
  left_join (. , 
             rent50plus %>% dplyr::select(GEOID, estimate, moe),
             by = "GEOID",
             keep = FALSE,
             copy = FALSE
  ) %>%
  left_join (. , 
             rentnotcomputed %>% dplyr::select(GEOID, estimate, moe),
             by = "GEOID",
             keep = FALSE,
             copy = FALSE)

# substring for new puma field of 4 digits for joins
rentburden19$puma <-  substr(rentburden19$GEOID,4,8) # New column of 4 digit puma codes

# Filter Income by NYS PUMA data for just NYC
rentburden19nyc <- inner_join(rentburden19, cdsnyc, 
                       by = "puma") %>%

rename(total_e = 3,
       total_m = 4,
       rent30_35_e = 5,
       rent30_35_m = 6,
       rent35_40_e = 7,
       rent35_40_m = 8,
       rent40_50_e = 9,
       rent40_50_m = 10,       
       rent50plus_e = 11,
       rent50plus_m = 12,
       rentnotcomputed_e = 13,
       rentnotcomputed_m = 14
)

rentburden19nyc$pctrentburden <- 
  (rentburden19nyc$rent30_35_e + rentburden19nyc$rent35_40_e + rentburden19nyc$rent40_50_e + rentburden19nyc$rent50plus_e)*100/
  (rentburden19nyc$total_e - rentburden19nyc$rentnotcomputed_e)



