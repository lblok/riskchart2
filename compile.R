require(readr)
cdsnyc <- read_csv('../PUMA CD crosswalk.csv',
                   col_names = TRUE,
                   cols(puma = col_character(),
                        cd_lu = col_character(),
                        cd_adj = col_character()))

## From covid.R
# COVID Case Rates - checked
covid_caserate_columns <- covid %>%
select(cd_adj, caserate)

# COVID Death Rates - checked
covid_deathrate_columns <- covid %>%
  select(cd_adj, deathrate)

## From acs.R

# Percent uninsured - 5 year
uninsured_columns <- nohinyc %>%
  select(cd_adj, pctnohi) %>%
  arrange(cd_adj)

# Percent Service Workers - 1 year
serviceworkers_columns <- serviceworkers %>%
  select(cd_adj, pctsvc) %>%
  arrange(cd_adj)

# Overcrowded - 5 year
overcrowding_columns <- overcrowding19nyc %>%
  select(cd_adj, pctsevere) %>%
  arrange(cd_adj)

# Percent POC - 1 year
poc_columns <- poc19nyc %>%
  select(cd_adj, pctpoc) %>%
  arrange(cd_adj)

# Rent burden - 1 year
rentburden_columns <- rentburden19nyc %>%
  select(cd_adj, pctrentburden) %>%
  arrange(cd_adj)

# AMI - 1 year, 2019 income, HH size, and HUD limits. Checked and corrected (wasn't household). 
ami_columns <- ami_hh19 %>%
  select(cd_adj, proratedami) %>%
  arrange(cd_adj)

## From evictions.R
evictions_columns <- evictions20_bycd %>%
  select(cd_adj, evictionrate) %>%
  arrange(cd_adj)

## From litigations.R 
litigations_columns <- countlitigations %>%
  select(cd_adj, litigations) %>%
  arrange(cd_adj)

## From hpd_violations.R 
violations_columns <- cviols20_6plus_bycd %>%
  select(cd_adj, rate) %>%
  arrange(cd_adj)

## From sales.R
sales_columns <- sales_fixed %>%
  select(cd_adj, change) %>%
  arrange(cd_adj) 

## From dobcoo.R | CD 406 is missing, double check it 
coo_columns <- count_jobs %>%
  select(cd_adj, newunits) %>%
  arrange(cd_adj)

## From foreclosures.R
foreclosures_columns <- foreclosurecounts %>%
  select(cd_adj, foreclosures) %>%
  arrange(cd_adj)

## From scriedrie.R
scriedrie_columns <- scriedrie_by_cd 
  
## From rentstab.R
rentstab_columns <- countrentstab %>%
  select(cd_adj, rsunits) %>%
  arrange(cd_adj)

## From nycha.R - checked CDs, not total outage numbers 
nychaoutages_columns <- nychaoutages_bycd %>%
  select(cd_adj, outagerate)

## From subsidized.R
lihtc_columns <- countlihtc %>%
  select(cd_adj, units) %>%
  arrange(cd_adj)

## From hmda.R
nonbank_columns <- nonbank %>%
  select(cd_adj, PctNonBank) %>%
  arrange(cd_adj)  

### Joined table

joined_columns <- covid_caserate_columns %>%
  left_join(.,
            covid_deathrate_columns,
            by = "cd_adj") %>%
  left_join(.,
            uninsured_columns,
            by = "cd_adj") %>%
  left_join(.,
            serviceworkers_columns,
            by = "cd_adj") %>%
  left_join(.,
            overcrowding_columns,
            by = "cd_adj") %>%
  left_join(.,
            poc_columns,
            by = "cd_adj") %>%
  left_join(.,
            rentburden_columns,
            by = "cd_adj") %>%
  left_join(.,
            ami_columns,
            by = "cd_adj") %>%
  left_join(.,
            evictions_columns,
            by = "cd_adj") %>%
  left_join(.,
            litigations_columns,
            by = "cd_adj") %>%
  left_join(.,
            violations_columns,
            by = "cd_adj") %>%
  left_join(.,
            sales_columns %>% select(cd_adj, change),
            by = "cd_adj") %>%
  left_join(.,
            coo_columns,
            by = "cd_adj") %>%
  left_join(.,
            foreclosures_columns,
            by = "cd_adj") %>%
  left_join(.,
            scriedrie_columns,
            by = "cd_adj") %>%
  left_join(.,
            rentstab_columns,
            by = "cd_adj") %>%
  left_join(.,
            nychaoutages_columns,
            by = "cd_adj") %>%
  left_join(.,
            lihtc_columns,
            by = "cd_adj") %>%
  left_join(.,
            nonbank_columns,
            by = "cd_adj") %>%
  distinct()
  
  
  write_csv(joined_columns, '~/Google\ Drive/ANHD/Risk\ Chart\ 2021/riskchart/joined_columns6.csv')
  
  
  