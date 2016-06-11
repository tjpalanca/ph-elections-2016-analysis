# PH Elections Analysis
# Gathering of City/Municipality Level Data
# tjpalanca@me.com | 2016-06-01

# Libraries -----------------------------------------------------------------------------------
library(readxl)
library(dplyr)
library(stringr)

# Data: Philippines Standard Geographic Code (PSGC) Dataset -----------------------------------

download.file( # NOTE: This file may change at any given time. If it has changed, use the repo copy.
  url      = 'http://nap.psa.gov.ph/activestats/psgc/publications/Revised_13April2016_PSGC.xlsx',
  destfile = 'data/03-psgc-raw.xlsx'
)

# Read in
psgc.dt <-
  read_excel(
    path = 'data/03-psgc-raw.xlsx',
    sheet = 'PSGC',
    skip = 7
  ) %>%
  filter( # Remove extraneous rows
    !is.na(Name)
  ) %>%
  rename( # More readable and R-compliant names
    region_code   = Reg,
    province_code = Prov,
    psgc_code     = PSGC,
    name          = Name,
    level         = `Inter-Level`,
    urban_rural   = `Urban/Rural\r\n2000 CPH`
  ) %>% # Add citymuni code
  mutate(
    citymuni_code =
      ifelse(level %in% c('City', 'Mun', 'Brgy'),
             str_sub(as.character(psgc_code), 1, 6), NA)
  ) %>%
  mutate( # Manual corrections
    citymuni_code = # City of Manila - eliminimating 'SubMunicipalities'
      ifelse(province_code == '39', 133900, citymuni_code),
    level = # Cities of Isabela and Cotabato, marking them as provinces in hierarchy
      ifelse(psgc_code %in% c('099700000', '129800000'),
             'Prov', level)
  )

# Clean up into hierarchical data frame

psgc_brgy.dt <-
  psgc.dt %>%
  filter(level == 'Brgy') %>%
  left_join(
    psgc.dt %>% filter(level %in% c('City', 'Mun')) %>% select(citymuni_code, citymuni_name = name),
    by = 'citymuni_code'
  ) %>%
  left_join(
    psgc.dt %>% filter(level %in% c('Prov', 'Dist')) %>% select(province_code, province_name = name),
    by = 'province_code'
  ) %>%
  left_join(
    psgc.dt %>% filter(level %in% c('Reg')) %>% select(region_code, region_name = name),
    by = 'region_code'
  )

# Save out results of PSGC dataset
saveRDS(psgc_brgy.dt, 'data/04-psgc-processed-barangay.rds')


