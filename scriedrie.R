# SCRIE/DRIE recipients

require(tidyverse)
require(readr)
require(janitor)
library(tidyr)
require(lubridate)

scriedrie <- read_excel('../SCRIE-DRIE/P8817_2021.xlsx', sheet = 'rie_tenant_list_2021', skip = 5) %>%
  clean_names()

scriedrie <- scriedrie %>%
  separate(bbl, c("boro", "block", "lot"), "-")

scriedrie$bbl <- str_c(scriedrie$boro,str_pad(scriedrie$block,5,side = "left","0"),str_pad(scriedrie$lot,4,side = "left","0"))

# check whether record was active any time in 2020. I found that in last year's risk chart, I did this filtering incorrectly and included all benefit time periods.
scriedrie$in2020 <- int_overlaps(interval(ymd("2020-01-01"), ymd("2020-12-31")), interval(scriedrie$benefit_start, scriedrie$benefit_end))

scriedrie2020approved <- scriedrie %>%
  filter(scriedrie$application_status == "Approved", 
         in2020 == TRUE)

scriedrie2020approved$docket_number[duplicated(scriedrie2020approved$docket_number)]
scriedrie2020approved$application_number[duplicated(scriedrie2020approved$application_number)]


# De-duplicate indidivudals by docket number. I think this is the right way. I don't think I did this last year. 
scriedrie20distdockets <- 
  scriedrie2020approved %>% distinct(docket_number, .keep_all = TRUE)
# De-duplicating by docket number appears to also remove duplicate application numbers

pluto_21v1$bbl <- as.character(pluto_21v1$bbl)

scriedrie_cds <- 
left_join (scriedrie20distdockets,
            pluto_21v1 %>% select(bbl, cd),
            by = "bbl") %>%
  inner_join (.,
             cdsnyc,
             by = c( "cd" = "cd_lu"))


scriedrie_by_cd <- scriedrie_cds %>%
  group_by(cd_adj) %>%
  dplyr::summarize(scriedrie = n())





  
