# New units in buildings with certificates of occupancy 

require(tidyverse)

## Install the required package with:
install.packages("RSocrata")

library("RSocrata")

# credentials in socratacredentials.R
# import COOs from NYC Open Data in 2020
coo20 <- read.socrata(
  "https://data.cityofnewyork.us/resource/bs8b-p36w.csv?$where=C_O_ISSUE_DATE%20between%20%272019-12-31T12:00:00%27%20and%20%272021-01-01T14:00:00%27",
  app_token = app_token,
  email     = email,
  password  = password) 

# concatenate community district code
require(stringr)
coo20$cd <- str_c(substr(coo20$bbl,1,1),sprintf("%02d", as.numeric(coo20$community_board)))

require(janitor)
coo20 <- clean_names(coo20)

# Filter for Final certificates, PR dwelling units >0, New Buildings, remove duplicate job numbers (very few)
coo20_newunits <- 
  coo20 %>%
  filter(issue_type == 'Final', pr_dwelling_unit > 0, job_type == 'NB') %>%
  distinct(job_number,.keep_all= TRUE)

# Group new units by community district
count_jobs <- 
  coo20_newunits %>% 
  group_by(cd) %>% 
  summarize(newunits=sum(pr_dwelling_unit))

# Test whether count is same as total units - yes
sum(count_jobs$newunits)
sum(coo20_newunits$pr_dwelling_unit)

count_jobs <- count_jobs %>%
  inner_join(.,
            cdsnyc,
            by = c("cd" = "cd_lu"))





