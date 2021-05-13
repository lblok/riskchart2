# Foreclosures
# download full dataset from DAP Portal API here: https://api.displacementalert.org/admin/core/dataset/39/change/

# use XLS geocoding workbook to ID lat/lon
# use QGIS to do joins 

foreclosurecounts <- read_csv('../Foreclosures/foreclosurecounts20.csv') 

foreclosurecounts$boro_cd <- as.character(foreclosurecounts$boro_cd)

foreclosurecounts <- foreclosurecounts %>%
  inner_join(.,
            cdsnyc,
            by = c("boro_cd" = "cd_lu")) %>%
  rename(foreclosures = NUMPOINTS)
