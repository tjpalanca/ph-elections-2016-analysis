# PHILIPPINES 2016 ELECTIONS RESULTS ANALYSIS
# Data Profiling and Cleaning
# contact@tjpalanca.com | 2016-05-16

# Libraries -----------------------------------------------------------------------------------
library(dplyr)
library(stringr)

# Setup ---------------------------------------------------------------------------------------

# Load scraped data
load("data/01_citymuni_election_results.RData")

# Asserting that citymuni_url is a unique identifier
n_distinct(result_hierarchy.dt$url) == nrow(result_hierarchy.dt)

# Joining result hierarchy with the contest details and results
results.dt <- result_hierarchy.dt %>%
  left_join(contest_details.dt, by = c("url" = "citymuni_url")) %>%
  left_join(contest_results.dt, by = c("url" = "citymuni_url", "contest_name" = "contest_name"))

# Profiling ------------------------------------------------------------------------------------

# No Results or Zero Transmission
n_distinct(results.dt$url[is.na(results.dt$transmission) | results.dt$transmission == 0])/
  n_distinct(results.dt$url)
## 0.03343109 or 3.34%

# Partial Transmission
n_distinct(results.dt$url[results.dt$transmission < 100 & results.dt$transmission > 0])/
  n_distinct(results.dt$url)
## 0.1530792 or 15.31%

# Full Transmission
n_distinct(results.dt$url[results.dt$transmission == 100])/
  n_distinct(results.dt$url)
## 0.8193548 or 81.94%

# Overall Voter Turnout
results.dt %>%
  group_by(url) %>%
  summarise(
    registered_voters = mean(registered_voters, na.rm = TRUE),
    ballots_reported = mean(ballots_reported, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  summarise(
    voter_turnout = sum(ballots_reported, na.rm = TRUE)/sum(registered_voters, na.rm = TRUE)
  )

# 0.8097527 or 80.97%

# Output municipalities and their transmission status
results_status.dt <- results.dt %>%
  group_by(citymuni, province, url) %>%
  summarise(
    transmission_status = ifelse(
      mean(ifelse(is.na(transmission), 0 , transmission)) == 100, "FULL TRANSMISSION",
      ifelse(mean(ifelse(is.na(transmission), 0 , transmission)) > 0, "PARTIAL TRANSMISSION",
      "NO TRANSMISSION"
    ))
  ) %>%
  ungroup() %>%
  mutate(
    `City / Municipality` = paste0(citymuni, ", ", province),
    `Transmission Status` = transmission_status
  ) %>%
  select(`City / Municipality`, `Transmission Status`)

write.csv(
  results_status.dt,
  "output/05-results-transmission-status.txt",
  row.names = FALSE
)

# Data Preparation - Vote Padding -------------------------------------------------------------

# Clean Up Enyes
results.dt <- results.dt %>%
  mutate_each(funs = funs(. = str_replace_all(., "\xd1", "N")), citymuni, candidate)

# Calculate recorded values by actual amounts
results.dt <- results.dt %>%
  mutate(voter_turnout = ballots_reported/registered_voters * 100,
         transmission = tally_reported/tally_total * 100)

# Calculate percentage winning of each candidate in each province
results.dt <- results.dt %>%
  group_by(url, contest_name) %>%
  mutate(votes_prop = votes/sum(votes) * 100) %>%
  ungroup()

# Restrict to national
results.dt <-
  results.dt %>%
  filter(contest_name %in% c("PRESIDENT PHILIPPINES", "VICE-PRESIDENT PHILIPPINES",
                             "SENATOR PHILIPPINES", "PARTY LIST PHILIPPINES"))

# Save out to relevant directories
saveRDS(results.dt, file = "data/02_results_per_citymuni.rds")
saveRDS(results.dt, file = "apps/data/02_results_per_citymuni.rds")
rm(citymuni.dt, contest_details.dt, contest_results.dt, province.dt, region.dt, result_hierarchy.dt)


