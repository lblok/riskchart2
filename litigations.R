require(tidyverse)
require(readr)

cdsnyc <- read_csv('../PUMA CD crosswalk.csv')

litigations20 <- read_csv('../Litigations/Housing_Litigations_2020.csv')

# Janitor package, clean headers
require(janitor)
litigations20 <- clean_names(litigations20)

# cast case_open_date as date type 
require(lubridate)
litigations20$opendate <- mdy_hms(litigations20$case_open_date)


# Concatenate boro and community district to 3 digit code
require(stringr)
litigations20$cd <- str_c(litigations20$boro,sprintf("%02d", as.numeric(litigations20$community_district))
)

# Group litigations by community district
countlitigations <- 
litigations20 %>% 
group_by(cd) %>% 
summarize(litigations=n())

# Test whether count is same as total litigations
sum(countlitigations$litigations)

countlitigations <- countlitigations %>%
  left_join(.,
            cdsnyc,
            by = c("cd" = "cd_lu"))
    