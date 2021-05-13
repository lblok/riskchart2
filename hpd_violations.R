# Run connect_postgres.R to connect to nycdb 
# run this to re-connect, hides credentials
con <- dbConnect(RPostgres::Postgres(), dbname = db, host=host_db, port=db_port, user=db_user, password=db_password) 

# count # class C violations in 2019: 168,086 is close to # I had in last year's analysis, 167,184
dbGetQuery (con, "
select count(*) from hpd_violations
where left(novissueddate::text,4) = '2019'
and class = 'C'
           ")

# count # C violations in 2020: 134,118
dbGetQuery (con, "
select count(*) from hpd_violations
where left(novissueddate::text,4) = '2020'
and class = 'C'
           ")

# count # C violations in 2020 in properties w/more than 6 units: 114,786
dbGetQuery (con, "
select count(*) from hpd_violations h
left join pluto_20v8 p on p.bbl = h.bbl
where left(novissueddate::text,4) = '2020'
and class = 'C'
and unitsres > 5
           ")

# query all C viols in 6+ units, optional to store
dbGetQuery (con, "
select * from hpd_violations h
left join pluto_20v8 p on p.bbl = h.bbl
where left(novissueddate::text,4) = '2020'
and class = 'C'
and unitsres > 5
           ")

# group C viols in 6+ units by CD
viols20c6plus_bycd <- dbGetQuery (con, "
with allviols as (
select * from hpd_violations h
left join pluto_20v8 p on p.bbl = h.bbl
where left(novissueddate::text,4) = '2020'
and class = 'C'
and unitsres > 5)
select cd, count(*) as viols
from allviols
group by cd
order by cd
           ")

# group # unitsres by CD total and in properties of 6+ units
unitsbycd <- dbGetQuery (con, 'select cd,
sum(unitsres) as units,
sum(unitsres) filter (where unitsres > 6) as units6
from pluto_20v8
group by cd
order by cd')

# join # C viols and # res units by CD
cviols20_6plus_bycd <- left_join (
  viols20c6plus_bycd,
  unitsbycd,
  by = "cd"
) 

# calculate rate of C viols per 1,000 units
cviols20_6plus_bycd$rate <- (cviols20_6plus_bycd$viols*1000)/cviols20_6plus_bycd$units6

cviols20_6plus_bycd$cd <- as.character(cviols20_6plus_bycd$cd)

cviols20_6plus_bycd <- cviols20_6plus_bycd %>%
  left_join(.,
            cdsnyc,
            by = c("cd" = "cd_lu"))
