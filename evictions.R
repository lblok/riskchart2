# Marshal evictions in 2020 (pre-covid)

require(tidyverse)

evictionsdap <- read_csv('http://api.displacementalert.org/evictions?format=csv')


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

require(RSocrata)

# credentials in socratacredentials.R
# import pluto21v1

pluto21v1 <- read.socrata(
  "https://data.cityofnewyork.us/resource/64uk-42ks.csv",
  app_token = app_token,
  email     = email,
  password  = password) 

require(tibbletime)
require(lubridate)

evictionsdap$executeddate <- ymd(evictionsdap$executeddate)
evictionsdap <- arrange(evictionsdap,executeddate)
evictionsdap <- as_tbl_time(evictionsdap, executeddate)

# filter for 2020
evictions20 <- filter_time(evictionsdap, ~'2020') 

evictions20_cd$cd <- as.character(evictions20_cd$cd)
pluto_21v1$cd <- as.character(pluto_21v1$cd)

evictions20_cd <- 
  inner_join (evictions20,
              pluto_21v1 %>% select(bbl, cd),
              by = "bbl",
              keep = FALSE) %>%
  left_join (.,
             cdsnyc,
             by = c( "cd" = "cd_lu"),
             keep = FALSE)

evictions20_bycd <- evictions20_cd %>%
  filter(residentialcommercialind == "RESIDENTIAL") %>%
  group_by(cd_adj) %>%
  dplyr::summarize(evictions=n()) %>%
  inner_join(.,
            cdsnyc,
            by = "cd_adj",
            keep = FALSE) %>% 
  left_join(.,
            population19nyc %>% dplyr::select(estimate,puma),
            by = "puma",
            keep = FALSE) %>%
  distinct()
  
# need to adjust per number of residents, use same method as covid 

# Number of individual evictions recorded by court-ordered marshals, divided by the population of the corresponding PUMA. The resulting figure is multiplied by 1,000 to find evictions per 1,000 residents. For Community Districts that are combined to make a PUMA (see notes above), the evictions for both CDs are summed and divided by the PUMA population. We de-duplicate records with identical addresses, unit numbers, eviction date, and marshal last names. Source: NYC Department of Investigations Evictions data.


evictions20_bycd$evictions_adj <- evictions20_bycd$evictions
evictions20_bycd$evictions_adj[evictions20_bycd$puma=='3810'] <- sum(evictions20_bycd$evictions[evictions20_bycd$puma=='3810'])
evictions20_bycd$evictions_adj[evictions20_bycd$puma=='3705'] <- sum(evictions20_bycd$evictions[evictions20_bycd$puma=='3705'])
evictions20_bycd$evictions_adj[evictions20_bycd$puma=='3710'] <- sum(evictions20_bycd$evictions[evictions20_bycd$puma=='3710'])
evictions20_bycd$evictions_adj[evictions20_bycd$puma=='3807'] <- sum(evictions20_bycd$evictions[evictions20_bycd$puma=='3807'])

evictions20_bycd$evictionrate <- evictions20_bycd$evictions_adj*1000/evictions20_bycd$estimate




