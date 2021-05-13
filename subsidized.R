# LIHTC expiration 2022-2026

# LIHTC Units Eligible to Expire, 2022-2026
# Number of units in buildings receiving Low Income Housing Tax Credits (4% or 9%) that are eligible to expire between December 31, 2021 and December 31, 2025, inclusive. Data is current as of September 2019. Source: NYU Furman Center's CoreData.nyc.


require(tidyverse)
require(readxl)



subsdb <- read_excel('../Furman/FC_Subsidized_Housing_Database_2020-06-30.xlsx',
           sheet="SubsidizedHousingDatabase")

# create column of end year 
subsdb$year <- substr(subsdb$end_date,1,4)

# filter LIHTC programs ending in upcoming 5 years
lihtc <- filter(subsdb,
      end_date <= 20251231 &
        end_date >= 20211231 &
       program_name %in% c("LIHTC 4%", "LIHTC 9%"))


# pull properties with missing res_units 
lihtc_missingunits <-  
  filter( lihtc, end_date <= 20251231 &
           end_date >= 20211231 &
           program_name %in% c("LIHTC 4%", "LIHTC 9%") &
            is.na(res_units)
                    )


# create a new column to manually enter res_units 
lihtc$unitsmanual <- NA

lihtc$bbl[6] 
lihtc$res_units[6] <- 733
lihtc$unitsmanual[6] <- TRUE

lihtc$bbl[140] 
lihtc$res_units[140] <- 51
lihtc$unitsmanual[140] <- TRUE

lihtc_missingunits$bbl
# pull res units manually from https://portal.displacementalert.org
# 1007570054 : 733 SROs
# 1010150010 : 1 commercial, 0 res
# 2029910026 : 2 commercial, 0 res
# 3019100052 : 2 commercial, 0 res
# 2028660007 : 1 commercial, 0 res
# 1019690019 : 1 commercial, 0 res
# 1019050063 : 51 SROs 

# manually input missing res unit values (SROs)


countlihtc <- 
  lihtc %>% 
  group_by(cd_id) %>% 
  dplyr::summarize(units=sum(res_units))

countlihtc$cd_id <- as.character(countlihtc$cd_id)

countlihtc <- left_join(countlihtc,
                        cdsnyc,
                        by = c("cd_id" = "cd_lu"))


