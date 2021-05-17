# COVID case and death rates

require(tidyverse)
require(readr)
require(tidycensus)
require(janitor)
require(readxl)



# B01003_001 total population 

population19 <- get_acs(geography = "public use microdata area", 
                    variables = "B01003_001", 
                    state =  "New York",
                    survey = "acs5",
                    year = 2019)

population19$puma <-  substr(population19$GEOID,4,8) # New column of 4 digit puma codes

population19nyc <- inner_join(population19, pumanums, 
                                           by = c("puma" = "pumanums")) %>%
  left_join(., cdsnyc,
            by = "puma")

covid <- read_excel('../DOHMH/Cases deaths_commdist_2021.04.28.xlsx', sheet = 'commdist_2021.04.28') %>%
  clean_names() %>%
  inner_join (.,
                    population19nyc %>% dplyr::select(estimate, cd_lu, cd_adj, puma),
                    by = c("community_district" = "cd_lu"),
              keep = FALSE) %>%
  distinct()



# have to account for combined PUMAs (don't know population of each CD). To calculate rate, sum the cases and deaths for both CDs, divide by population, apply the rate to each. 
# (from data notes 2020:) For Community Districts that are combined to make a PUMA, the cases for both CDs are summed and divided by the PUMA population. 

# Combined PUMAs + cd_adj
# 3710 - 101, 102
# 3705 - 103, 106
# 3810 - 301, 302
# 3807 - 304, 305

# define cases_adj as confirmed_and_probable_cases but where PUMAs duplicate, sum confirmed_and_probable_cases
covid$cases_adj <- covid$confirmed_and_probable_cases
covid$cases_adj[covid$puma=='3810'] <- sum(covid$confirmed_and_probable_cases[covid$puma=='3810'])
covid$cases_adj[covid$puma=='3705'] <- sum(covid$confirmed_and_probable_cases[covid$puma=='3705'])
covid$cases_adj[covid$puma=='3710'] <- sum(covid$confirmed_and_probable_cases[covid$puma=='3710'])
covid$cases_adj[covid$puma=='3807'] <- sum(covid$confirmed_and_probable_cases[covid$puma=='3807'])

# same for deaths
covid$deaths_adj <- covid$confirmed_and_probable_deaths
covid$deaths_adj[covid$puma=='3810'] <- sum(covid$confirmed_and_probable_deaths[covid$puma=='3810'])
covid$deaths_adj[covid$puma=='3705'] <- sum(covid$confirmed_and_probable_deaths[covid$puma=='3705'])
covid$deaths_adj[covid$puma=='3710'] <- sum(covid$confirmed_and_probable_deaths[covid$puma=='3710'])
covid$deaths_adj[covid$puma=='3807'] <- sum(covid$confirmed_and_probable_deaths[covid$puma=='3807'])

covid$caserate2 <- covid$cases_adj*100000/covid$estimate
covid$deathrate2 <- covid$deaths_adj*100000/covid$estimate


covid <- covid[order(covid$cd_adj),]


# testing covid death rates, which seem out of alignment with DOHMH's dashboard

data_by_modzcta_deaths <- read_csv('../COVID/data-by-modzcta_210514.csv') %>%
  select(MODIFIED_ZCTA,NEIGHBORHOOD_NAME,COVID_DEATH_COUNT,POP_DENOMINATOR,COVID_DEATH_RATE) %>%
  view()

covid_deaths <- covid %>% select(community_district,cd_adj,confirmed_and_probable_deaths,estimate,deathrate2) %>%
  view()

sum(covid$confirmed_and_probable_deaths)
sum(data_by_modzcta$COVID_DEATH_COUNT)
sum(covid$estimate)
sum(data_by_modzcta$POP_DENOMINATOR)    
    
sum(covid$confirmed_and_probable_deaths) /  sum(covid$estimate)
sum(data_by_modzcta$COVID_DEATH_COUNT) / sum(data_by_modzcta$POP_DENOMINATOR)    

sum(covid$confirmed_and_probable_cases)
sum(data_by_modzcta$COVID_CASE_COUNT)

sum(covid$confirmed_and_probable_cases) /  sum(covid$estimate)
sum(data_by_modzcta$COVID_CASE_COUNT) / sum(data_by_modzcta$POP_DENOMINATOR)   

# conclusion - numbers by CD look ballpark similar to current numbers by mod ZCTA. DOHMH uses significantly lower population estimates than ACS data, which makes the death rates higher. So our estimates are conservative.
# where as total cases are higher in CD data versus ZCTA data (856914 v 746901), total deaths are lower (24660 v 27415)



    