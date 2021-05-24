# NYCHA

# Checked the 2020 Dev book to make sure recent RAD conversions are excluded from NYC Open Data source, they were
# 2020 development book https://www1.nyc.gov/assets/nycha/downloads/pdf/pdb2020.pdf
# https://data.cityofnewyork.us/Housing-Development/NYCHA-Development-Data-Book/evjd-dqpz/data 

require(tidyverse)
require(readr)

nychadev19 <- read_csv('../NYCHA/NYCHA_Development_Data_Book_2019.csv')

require(janitor)
nychadev19 <- clean_names(nychadev19)

require(plyr)
nychadev19$borocode <- 
  revalue(nychadev19$borough, c("MANHATTAN" = "1", "BRONX" = "2", "BROOKLYN" = "3", "QUEENS" = "4", "STATEN ISLAND" = "5"))

require(stringr)
nychadev19$cd <- 
  str_c(nychadev19$borocode,sprintf("%02d", as.numeric(nychadev19$community_distirct)))

nychadev19_missing <- filter(nychadev19, is.na(nychadev19$borough))

# FHA repossessed properties are missing boroughs, that matches with dev book info showing that they span many CDs. I think it's scatter site. It's a minimal number of units, 192 total, excluding from total counts.
  sum(nychadev19_missing$total_number_of_apartments)

# num of current apartments = Number of dwelling units as of 01/01/2019 as per data dictionary https://data.cityofnewyork.us/api/views/evjd-dqpz/files/bfb44f96-dfc3-4d7b-92fe-ccf07ff06add?download=true&filename=NYCHA_Data_Dictionary_Development%20Data%20Book.xlsx
nychaunits <- 
  nychadev19 %>% 
  group_by(cd) %>% 
  dplyr::summarise(units=sum(number_of_current_apartments))

# ID the developments in more than one CD
nychadev19 %>%
filter(cd %in% c("2NA", "3NA"))
# 
# data_as_of development hud_amp_number tds_number consolidated_td… development_edp… operating_edp_n…
# <chr>      <chr>       <chr>          <chr>      <chr>            <chr>            <chr>           
# 1 1/1/2019   KINGSBOROU… NY005010100    010        010              205              205             
# 2 1/1/2019   MORRISANIA… NY005012670    267        267              385              385             
# 3 1/1/2019   WEST FARMS… NY005015300    360        530              780              780             

# Apply the CDs I assigned them in 2020 using GIS, adjusted back to regular land use codes and not risk chart codes (there also seemed to be an error with Air Rights)

# DEVELOPMENT	cd
# KINGSBOROUGH	308
# MORRISANIA AIR RIGHTS	204
# WEST FARMS ROAD REHAB	202

nychadev19$cd_fix <- nychadev19$cd
nychadev19$cd_fix[nychadev19$development=='KINGSBOROUGH'] <- '308'
nychadev19$cd_fix[nychadev19$development=='MORRISANIA AIR RIGHTS'] <- '204'
nychadev19$cd_fix[nychadev19$development=='WEST FARMS ROAD REHAB'] <- '202'

nychaunits_cd_fix <- 
  nychadev19 %>% 
  group_by(cd_fix) %>% 
  dplyr::summarise(units=sum(number_of_current_apartments))

# File sent directly by Maxwell Austensen. Based on work here: https://github.com/austensen/nycha-outages 
nychaoutages <- read_csv('/Users/lucy/Google\ Drive/ANHD/Risk\ Chart\ 2021/NYCHA/history_all.csv')

min(nychaoutages$report_date)
max(nychaoutages$report_date)


# Tibbletime: https://cran.r-project.org/web/packages/tibbletime/vignettes/TT-01-time-based-filtering.html
install.packages("tibbletime")
require(tibbletime)

# order by report_date
nychaoutages <- nychaoutages[order(nychaoutages$report_date),] 

# define report_date as index, ordered
nychaoutages <- as_tbl_time(nychaoutages, index = report_date)

# filter for desired date range
nychaoutages_12mos <- filter_time(nychaoutages, '2020-04-01' ~ '2021-03-31') %>%
 filter(planned == 'Unplanned')

nychaoutages_12mos <- left_join(nychaoutages_12mos, 
          nychadev19 %>% select(development, cd_fix),
                                by = c("development_name" = "development"),
                                keep = FALSE,
                                copy = FALSE)

nychaoutages_bycd <- 
  nychaoutages_12mos %>% 
  group_by(cd_fix) %>% 
  dplyr::summarise(count=n()) %>%
  left_join (.,
             nychaunits_cd_fix,
             by = "cd_fix") %>%
  left_join (.,
             cdsnyc,
             by = c("cd_fix" = "cd_lu"),
             keep = FALSE)

nychaoutages_bycd$outagerate <- nychaoutages_bycd$count*1000 / nychaoutages_bycd$units

# Check which developments have high numbers in the highest ranking district
nychaoutages_12mos %>%
  filter(cd_fix == '201') %>%
  group_by(development_name) %>%
  summarize(n()) %>%
  view()




