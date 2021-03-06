# PHILIPPINES 2016 ELECTIONS RESULTS SCRAPER
# contact@tjpalanca.com | 2016-05-16

# Libraries -----------------------------------------------------------------------------------

library(rPython) # Use cfscrape python module to bypass CloudFront DDOS protection
library(dplyr)
library(stringr)

# Setup ---------------------------------------------------------------------------------------

# URLs
base.url <- "https://www.pilipinaselectionresults2016.com/"
country.url <- "data/regions/0.json"

# Scraping Functions --------------------------------------------------------------------------

# Load Python scraper
python.load('script/01-munging/01_scraper.py')

ScrapeURL <- function (url) {
  cat(paste0("Scraping from ", base.url, url, "...\n"))
  # Call to previously defined Python scraper
  result <- try({
    python.call('scrape', url = paste0(base.url, url))
  })
  # Error Catching
  if (class(result) == 'try-error') return(NULL)
  # Return if error not caught
  return(result)
}

GetSubRegions <- function(urls) {
  # GetSubRegions takes a list of urls and returns their reported subRegions.
  do.call(
    "rbind",
    lapply(
      urls,
      function (url) {
        json <- ScrapeURL(url)
        do.call(
          "rbind",
          lapply(
            json$subRegions,
            function(subRegion) {
              data.frame(
                parentName         = subRegionsubRegion$parentName,
                parentCategoryName = subRegion$parentCategoryName,
                parentUrl          = json$url,
                name               = subRegion$name,
                categoryName       = subRegion$categoryName,
                url                = subRegion$url,
                stringsAsFactors   = FALSE
              )
            }
          )
        )
      }
    )
  )
}

GetContests <- function(urls) {
  # Get contest takes a list of urls and returns the contests the region is associated with,
  # along with the results and stats for that particular contest.
  lapply(
    urls,
    function(citymuni_url) {
      citymuni_json <- ScrapeURL(citymuni_url)
      lapply(
        citymuni_json$contests,
        function(contest) {
          contest_json <- ScrapeURL(contest['url'])
          if(is.null(contest_json)) return(NULL)
          contest_details <-
            data.frame(
              citymuni_url      = citymuni_url,
              contest_name      = contest_json$name,
              tally_total       = contest_json$stats$regionInfo$rows[[1]][['value']],
              tally_reported    = contest_json$stats$regionInfo$rows[[2]][['value']],
              transmission      = contest_json$stats$transmission,
              registered_voters = contest_json$stats$voters$rows[[1]][['value']],
              ballots_reported  = contest_json$stats$ballots$rows[[1]][['value']],
              voter_turnout     = contest_json$stats$voters$rows[[2]][['value']],
              stringsAsFactors  = FALSE
            )
          contest_results <-
            data.frame(
              citymuni_url      = citymuni_url,
              contest_name      = contest_json$name,
              candidate         = do.call('c', lapply(contest_json$results, function(x) x[['bName']])),
              votes             = do.call('c', lapply(contest_json$results, function(x) x[['votes']])),
              stringsAsFactors  = FALSE
            )
          return(list(contest_details, contest_results))
        }
      )
    }
  )
}

GetContestInfo <- function(contests, type = c('voting', 'result')) {
  # GetContestInfo extracts either voting stats or results from objects
  # returned from the GetContests function
  pos <- ifelse(type == 'voting', 1, 2)
  contest_details.dt <-
    do.call(
      'rbind',
      lapply(contests, function(x) {
        do.call(
          'rbind',
          lapply(x, function(x) x[[pos]])
        )
      })
    )
}

# Scraping Execution ----------------------------------------------------------------------

# Get subregions per level
region.dt <- GetSubRegions(country.url)
province.dt <- GetSubRegions(region.dt$url)
citymuni.dt <- GetSubRegions(province.dt$url)
# barangay.dt <- GetSubRegions(citymuni.dt$url)
# lvg.dt <- GetSubRegions(barangay.dt$url)

# NOTE(tjpalanca): It's taking too long to go down that deep, might reserve
# barangay and precinct data for a future time.

# Consolidate results hierarchy
result_hierarchy.dt <-
  region.dt %>% select(country = parentName, region = name) %>%
  left_join(province.dt %>% select(region = parentName, province = name)) %>%
  left_join(citymuni.dt %>% select(province = parentName, citymuni = name, url))

# Get contest results
contests.ls <- GetContests(result_hierarchy.dt$url)

# Parse out contest results
contest_details.dt <-
  GetContestInfo(contests.ls, 'voting') %>%
  mutate_each(funs = funs(. = as.numeric(str_replace_all(., ",|%", ""))),
              -contest_name, -citymuni_url)
contest_results.dt <-
  GetContestInfo(contests.ls, 'result') %>%
  mutate(votes = as.numeric(str_replace_all(votes, ",", "")))

# Output Results ------------------------------------------------------------------------------

# Remove scraping functions
rm(GetContests, GetContestInfo, GetContestResults, GetSubRegions, ScrapeURL)

# Remove unused values
rm(contests.ls, country.url, base.url)

# Save out data
save.image("data/01_citymuni_election_results.RData")
