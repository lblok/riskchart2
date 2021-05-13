cdsnyc <- read_csv('../PUMA CD crosswalk.csv',
                   col_names = TRUE,
                   cols(puma = col_character(),
                        cd_lu = col_character(),
                        cd_adj = col_character()))
## From covid.R
# COVID Case Rates
covid_caserate_columns <- covid %>%
select(cd_adj, caserate)

# COVID Death Rates
covid_deathrate_columns <- covid %>%
  select(cd_adj, deathrate)

## From acs.R
# Percent Service Workers
serviceworkers_columns <- serviceworkers %>%
  select(cd_adj, pctsvc) %>%
  arrange(cd_adj)

# Overcrowded 
overcrowding_columns <- overcrowding19nyc %>%
  select(cd_adj, pctsevere) %>%
  arrange(cd_adj)

# Percent POC
poc_columns <- poc19nyc %>%
  select(cd_adj, pctpoc) %>%
  arrange(cd_adj)

# Rent burdem
rentburden_columns <- rentburden19nyc %>%
  select(cd_adj, pctrentburden) %>%
  arrange(cd_adj)

# AMI
ami_columns <- ami19 %>%
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
sales_columns <- ressales_grouped %>%
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

## From nycha.R
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
            sales_columns,
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
  
  
  write_csv(joined_columns, 'joined_columns.csv')
  
  
  