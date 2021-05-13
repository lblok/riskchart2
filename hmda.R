# HMDA % change in non-bank loans, 2017-2019

require(tidyverse)
require(readr)

nonbank <- read_excel('../HMDA/HMDA 2017-19.xlsx', sheet = 'PUMA_2019_HPL')

nonbank$pctnonbank2 <- nonbank$PctNonBank*100

nonbank$CD <- as.character(nonbank$CD)

nonbank <- left_join(
  nonbank,
  cdsnyc,
  by = c("CD" = "cd_lu")
)