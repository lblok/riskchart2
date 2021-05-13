# Rent stabilized units in 2019

# Pull from NYCDB 

# Run connect_postgres.R to connect to nycdb 
# run this to re-connect, hides credentials
library('RPostgres')
con <- dbConnect(RPostgres::Postgres(), dbname = db, host=host_db, port=db_port, user=db_user, password=db_password) 

rentstab_v2 <- dbGetQuery (con, "
select r.*, p.cd from rentstab_v2 r
left join pluto_19v2 p on bbl = ucbbl 
           ")

# Group rent stab units by community district
countrentstab <- 
  rentstab_v2 %>% 
  group_by(cd) %>% 
  summarize(rsunits=sum(uc2019,na.rm=TRUE)) 

countrentstab$cd <- as.character(countrentstab$cd)

countrentstab <- countrentstab %>%
  inner_join(.,
            cdsnyc,
            by = c("cd" = "cd_lu"))






