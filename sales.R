# Change in res sales per sq foot 2018-2020

## Install the required package with:
install.packages("RSocrata")

library("RSocrata")
require(lubridate)

# credentials in socratacredentials.R
# # import annualized sales from NYC Open Data. Only available thru 2019
# sales_since_2018 <- read.socrata(
#   "https://data.cityofnewyork.us/resource/w2pb-icbu.csv?$where=sale_date%20between%20%272018-01-01T12:00:00%27%20and%20%272021-01-01T14:00:00%27",
#   app_token = app_token,
#   email     = email,
#   password  = password) 
# 
# # convert GSF to number format
# sales_since_2018$gross_square_feet_num <- as.numeric(parse_number(sales_since_2018$gross_square_feet))

pluto_21v1 <- read.socrata(
  "https://data.cityofnewyork.us/resource/64uk-42ks.csv",
  app_token = app_token,
  email     = email,
  password  = password)

# Import rolling sales for 2018 from local downloads (annualized sales was classifying condo and coops differently, threw everything off)
sales2018 <- read_csv('/Users/lucy/Google\ Drive/ANHD/Risk\ Chart\ 2021/Sales/dof_sales_2018.csv')
# Import rolling sales for 2020 from local downloads
sales2020 <- read_csv('/Users/lucy/Google\ Drive/ANHD/Risk\ Chart\ 2021/Sales/dof_sales_2020.csv')


# Change in res sales per gross sf
# Description: Change in average gross price per square foot of residential buildings from 2017 to 2019 for each Community District. Sale prices of less than $100,000, sales with recorded gross square footage of less than 500 sq ft, and buildings containing commercial units are excluded. Community Districts lacking enough sales records to calculate a change show a value of ‘-’. Source: NYC DOF Rolling Sales.

# Filter 2018 and 2020 sales for residential equal to or over $100,000, res units > 0, comm units = 0, gsf > 500, then join 
ressales18 <- 
  sales_since_2018 %>% 
  filter(sale_price>=100000, (year(sale_date) == 2018), residential_units > 0, gross_square_feet_num >= 500, commercial_units == 0) %>%
  select(bbl, sale_price, sale_date, building_class_category, total_units, commercial_units,
                residential_units, gross_square_feet = gross_square_feet_num) %>% 
  left_join (.,pluto_21v1 %>% dplyr::select(bbl,cd),
             by = 'bbl')

ressales18 <- 
  sales2018 %>% 
  select(bbl, saleprice, saledate, buildingclasscategory, totalunits, commercialunits,
         residentialunits, grosssquarefeet) %>% 
  filter(saleprice>=100000, residentialunits > 0, grosssquarefeet >= 500, commercialunits == 0) %>%
  left_join (.,pluto_21v1 %>% dplyr::select(bbl,cd),
             by = 'bbl')

ressales20 <- 
  sales2020 %>% 
  select(bbl, saleprice, saledate, buildingclasscategory, totalunits, commercialunits,
         residentialunits, grosssquarefeet) %>% 
  filter(saleprice>100000, residentialunits > 0, grosssquarefeet >= 500, commercialunits == 0) %>%
  left_join (.,pluto_21v1 %>% dplyr::select(bbl,cd),
             by = 'bbl')

ressales18_20 <- full_join (ressales18,ressales20) 

# there are over 8k missing values but they're almost all condos and in the past I've used DAP map data, which already eliminates condos, so it looks ok.
ressales18_20 %>%
  filter(is.na(ressales18_20$cd) == TRUE) %>%
  group_by(buildingclasscategory) %>%
  summarize(n()) %>%
  view()


# Add price per gsf
ressales18_20$ppgsf <- ressales18_20$saleprice/ressales18_20$grosssquarefeet

#Add year
ressales18_20$year <- year(ressales18_20$saledate)

#Add CD adj

ressales18_20$cd <- as.character(ressales18_20$cd)

ressales18_20 <- 
  ressales18_20 %>%
  inner_join(.,
             cdsnyc %>% dplyr::select(cd_lu, cd_adj),
             by = c("cd" = "cd_lu"))


write_csv(ressales18_20, '~/Google\ Drive/ANHD/Risk\ Chart\ 2021/ressales18_20.csv')

ressales_grouped <- 
  ressales18_20 %>%
  group_by(cd_adj, year) %>%
  summarise(totalsales = sum(saleprice), gsf=sum(grosssquarefeet), count=n())

ressales_grouped$ppgsf <- ressales_grouped$totalsales / ressales_grouped$gsf


ressales_grouped_wide <-
  ressales_grouped %>%
  pivot_wider(names_from = year, values_from = c(totalsales, gsf, count, ppgsf)) 


ressales_grouped_wide$change <- 
  (ressales_grouped_wide$ppgsf_2020 - ressales_grouped_wide$ppgsf_2018)/
  ressales_grouped_wide$ppgsf_2018

sales_fixed <- ressales_grouped_wide 









# old, wrong


# Previous year method
# 
# # test totals based on filter criteria
# sales2018_res <- filter(sales_since_2018, 
#                         year(sale_date)==2018,
#                         residential_units > 0,
#                         commercial_units == 0,
#                         gross_square_feet_num >= 500,
#                         sale_price > 100000
# )
# 
# sales2020_res <- filter(sales2020, 
#                         year(saledate)==2020,
#                         residentialunits > 0,
#                         commercialunits == 0,
#                         grosssquarefeet >= 500,
#                         saleprice > 100000
# )
# 
# summary(sales2018_res)
# summary(sales2020_res)
# 
# 




# 2020 code:
# 
# 
# --the original dataset comes from DAP Map early 2020 with 1/1/7 - 12/31/19. 
# --Only includes sales > $1,000.  
# 
# select count(*) from (
#   select a. bbl, saleprice, saledate, buildingclasscategory, 
#   totalunits, commercialunits, residentialunits, grosssquarefeet, 
#   p.cd, saleprice/grosssquarefeet as ppgsf,
#   left(saledate::text,4) as year
#   from dap_sales200101 a
#   left join pluto_19v2 p on p.bbl = a.bbl 
#   where (left(saledate::text,4) = '2019' or 
#          left(saledate::text,4) = '2017')
#   and residentialunits > 0
#   and grosssquarefeet > 0) a 
# --47,474
# 
# select count(*) from (
#   select a. bbl, saleprice, saledate, buildingclasscategory, 
#   totalunits, commercialunits, residentialunits, grosssquarefeet, 
#   p.cd, saleprice/grosssquarefeet as ppgsf,
#   left(saledate::text,4) as year
#   from dap_sales200101 a
#   left join pluto_19v2 p on p.bbl = a.bbl 
#   where (left(saledate::text,4) = '2019' or 
#          left(saledate::text,4) = '2017')
#   and residentialunits > 0
#   and grosssquarefeet > 0
#   and saleprice > 100000) a 
# --46,880
# 
# select count(*) from (
#   select a. bbl, saleprice, saledate, buildingclasscategory, 
#   totalunits, commercialunits, residentialunits, grosssquarefeet, 
#   p.cd, saleprice/grosssquarefeet as ppgsf,
#   left(saledate::text,4) as year
#   from dap_sales200101 a
#   left join pluto_19v2 p on p.bbl = a.bbl 
#   where (left(saledate::text,4) = '2019' or 
#          left(saledate::text,4) = '2017')
#   and residentialunits > 0
#   and grosssquarefeet > 0
#   and saleprice > 100000
#   and commercialunits = 0) a 
# --44,412
# 
# create table ressales17_19 as 
# select a. bbl, saleprice, saledate, buildingclasscategory, 
# totalunits, commercialunits, residentialunits, grosssquarefeet, 
# p.cd, saleprice/grosssquarefeet as ppgsf,
# left(saledate::text,4) as year
# from dap_sales200101 a
# left join pluto_19v2 p on p.bbl = a.bbl 
# where (left(saledate::text,4) = '2019' or 
#        left(saledate::text,4) = '2017')
# and residentialunits > 0
# and grosssquarefeet > 0
# and saleprice > 100000
# and commercialunits = 0

