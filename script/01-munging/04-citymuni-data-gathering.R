# PH Elections Analysis
# Gathering of City/Municipality Level Data
# tjpalanca@me.com | 2016-06-01

# Libraries -----------------------------------------------------------------------------------
library(readxl)
library(dplyr)
library(stringr)
library(rvest)
library(purrr)
library(readr)

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
    psgc.dt %>%
      filter(level %in% c('City', 'Mun')) %>%
      select(citymuni_code, citymuni_name = name),
    by = 'citymuni_code'
  ) %>%
  left_join(
    psgc.dt %>%
      filter(level %in% c('Prov', 'Dist')) %>%
      select(province_code, province_name = name),
    by = 'province_code'
  ) %>%
  left_join(
    psgc.dt %>%
      filter(level %in% c('Reg')) %>%
      select(region_code, region_name = name),
    by = 'region_code'
  )

# Cleanup
rm(psgc.dt)

# Save out results of PSGC dataset
saveRDS(psgc_brgy.dt, 'data/04-psgc-processed-barangay.rds')

# Data: Land Area and 2010 Population per City/Municipality -----------------------------------

# Load HTML tables in city page
city.tables <-
  read_html('http://nap.psa.gov.ph/activestats/psgc/listcity.asp') %>%
  html_table(header = TRUE, trim = TRUE)

city_info.dt <- city.tables[[2]] %>%
  rename( # friendly column names
    citymuni_name = City,
    psgc_code = Code,
    province_name = Province,
    citymuni_incomeclass = `Income Class`,
    citymuni_regvoters_2010 = `Registered Voters1(2010)`,
    citymuni_landarea_ha_2007 = `Land Area(as of 2007, in hectares)`
  ) %>%
  mutate(
    # clean up the citymuni name to remove
    citymuni_name = str_replace_all(str_extract(citymuni_name, '^.*\\r'), '\\r$', ''),
    # remove commas and correct landarea type
    citymuni_landarea_ha_2007 = as.numeric(str_replace_all(citymuni_landarea_ha_2007, ',', '')),
    # add level marker
    level = 'City'
  )

# Load HTML tables in muni page
muni.tables <-
  read_html('http://nap.psa.gov.ph/activestats/psgc/listmun.asp?whichpage=1&pagesize=1489&sqlquery=select+municipalities%2Ename%2C+municipalities%2Eregprovmunbgy%2C+municipalities%2Eincomeclass%2C+municipalities%2Ereg%2C+municipalities%2Eprov%2C+municipalities%2Emun%2C+municipalities%2Esubmun%2C+municipalities%2Ecity%2C+province%2Eprov%2C+province%2Ename%2C+municipalities%2Ejan2010regvoters%2C+municipalities%2Ea2010Pop%2C+municipalities%2Elandarea2007+from+municipalities+INNER+JOIN+Province+ON+Municipalities%2EProv+%3D+Province%2EProv+where+municipalities%2Esubmun%3D0+and+municipalities%2Ecity%3D0+ORDER+BY+municipalities%2Ename') %>%
  html_table(trim = TRUE)

muni.tables[[2]][1,]
colnames(muni.tables[[3]])
muni_info.dt <- muni.tables[[3]] %>%
  rename(
    citymuni_name = X1,
    psgc_code = X2,
    province_name = X3,
    citymuni_incomeclass = X4,
    citymuni_regvoters_2010 = X5,
    citymuni_population_2010 = X6,
    citymuni_landarea_ha_2007 = X7
  ) %>%
  select( # remove non-common column
    -citymuni_population_2010
  ) %>%
  mutate( # clean up
    # clean up data types
    citymuni_regvoters_2010 = as.numeric(str_replace_all(citymuni_regvoters_2010, ',', '')),
    citymuni_landarea_ha_2007 = as.numeric(str_replace_all(citymuni_landarea_ha_2007, ',', '')),
    # add level marker,
    level = 'Mun'
  )

citymuni_info.dt <-
  rbind(
    city_info.dt,
    muni_info.dt
  ) %>%
  mutate(
    # convert PSGC code to string
    psgc_code = formatC(psgc_code, digits = 9, flag = 0)
  )

# Cleanup
rm(muni.tables, city.tables, city_info.dt, muni_info.dt)

# Save out to disk
saveRDS(citymuni_info.dt, 'data/05-citymuni-landarea-regvoter-psa.rds')

# Data: Population per Barangay ---------------------------------------------------------------

# Download files
c( # Taken from the press release
  'https://psa.gov.ph/sites/default/files/attachments/hsd/pressrelease/CAR_0.xlsx',
  'https://psa.gov.ph/sites/default/files/attachments/hsd/pressrelease/NCR.xlsx',
  'https://psa.gov.ph/sites/default/files/attachments/hsd/pressrelease/R01.xlsx',
  'https://psa.gov.ph/sites/default/files/attachments/hsd/pressrelease/R02.xlsx',
  'https://psa.gov.ph/sites/default/files/attachments/hsd/pressrelease/R03.xlsx',
  'https://psa.gov.ph/sites/default/files/attachments/hsd/pressrelease/R04A.xlsx',
  'https://psa.gov.ph/sites/default/files/attachments/hsd/pressrelease/R04B.xlsx',
  'https://psa.gov.ph/sites/default/files/attachments/hsd/pressrelease/R05.xlsx',
  'https://psa.gov.ph/sites/default/files/attachments/hsd/pressrelease/R06.xlsx',
  'https://psa.gov.ph/sites/default/files/attachments/hsd/pressrelease/R07.xlsx',
  'https://psa.gov.ph/sites/default/files/attachments/hsd/pressrelease/R08.xlsx',
  'https://psa.gov.ph/sites/default/files/attachments/hsd/pressrelease/R09.xlsx',
  'https://psa.gov.ph/sites/default/files/attachments/hsd/pressrelease/R10.xlsx',
  'https://psa.gov.ph/sites/default/files/attachments/hsd/pressrelease/R11.xlsx',
  'https://psa.gov.ph/sites/default/files/attachments/hsd/pressrelease/R12.xlsx',
  'https://psa.gov.ph/sites/default/files/attachments/hsd/pressrelease/Caraga.xlsx',
  'https://psa.gov.ph/sites/default/files/attachments/hsd/pressrelease/ARMM.xlsx',
  'https://psa.gov.ph/sites/default/files/attachments/hsd/pressrelease/NIR.xlsx'
) %>%
  walk(
    function(url) {
      download.file(
        url = url,
        destfile =
          paste0(
            'data/06-census-2015-raw/',
            str_replace(url, 'https://psa.gov.ph/sites/default/files/attachments/hsd/pressrelease/', '')
          )
      )
    }
  )

# Read in files and process them into tidy data

population.dt <-
  map_df(
    list.files('data/06-census-2015-raw/'),
    function(filename) {
      read_excel(
        paste0('data/06-census-2015-raw/', filename), skip = 6,
        col_names = FALSE
      ) %>%
        select(
          name = X1,
          population = X2
        ) %>%
        mutate( # name the levels
          level =
            ifelse(
              is.na(name), NA,
              ifelse(
                row_number() == 1, 'Reg',
                ifelse(
                  is.na(lag(name)) & is.na(lead(name)), 'Prov',
                  ifelse(
                    is.na(lag(name)) & !is.na(lead(name)), 'CityMuni',
                    'Brgy'
                  )
                )
              )
            )
        ) %>%
        filter(!is.na(name) & !is.na(population)) %>%
        mutate(region_name = name[level == 'Reg']) %>% # Cascade region name
        filter(level != 'Reg') %>%
        mutate(group = cumsum(level == 'Prov')) %>%
        group_by(group) %>%
        mutate(province_name = name[level == 'Prov']) %>% # Cascade province name
        filter(level != 'Prov') %>%
        ungroup() %>% mutate(group = cumsum(level == 'CityMuni')) %>%
        group_by(group) %>%
        mutate(citymuni_name = name[level == 'CityMuni']) %>% # Cascade citymuni name
        filter(level != 'CityMuni') %>% ungroup() %>%
        select(
          region_name, province_name, citymuni_name, barangay_name = name,
          population
        )
    }
  )

# Save out
saveRDS(population.dt, 'data/06-census-2015-processed.rds')

# Data: Poverty Estimates ---------------------------------------------------------------------

# Download file
download.file(
  'http://storage.googleapis.com/amt-dgph.appspot.com/uploads/h9KWMWZfeoy0Ztn51bfT/pov_muni.csv',
  destfile = 'data/07-poverty-citymuni-raw.csv'
)

# Read in
poverty.dt <-
  read_csv(
    'data/07-poverty-citymuni-raw.csv'
  ) %>%
  rename(
    psgc_code = psgc_muni,
    pov_year = pov_muni_year,
    pov_measure = pov_muni_measure,
    pov_estimate = estimate,
    pov_std_error = standard_error,
    pov_coef_var = coefficient_of_variation
  )

# Save RDS
saveRDS(poverty.dt, 'data/08-poverty-citymuni-processed.rds')
