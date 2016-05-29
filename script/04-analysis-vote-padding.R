# PHILIPPINES 2016 ELECTIONS RESULTS ANALYSIS
# Voter Turnout vs Votes for Candidate Analysis
# contact@tjpalanca.com | 2016-05-27

# Libraries -----------------------------------------------------------------------------------
library(ggplot2)
library(dplyr); library(stringr)
library(e1071); library(R.utils)
library(shiny)
library(highcharter); library(htmlwidgets)

# Setup ---------------------------------------------------------------------------------------
results.dt <- readRDS('data/02_results_per_citymuni.rds') %>%
  mutate(
    party = ifelse(contest_name != "PARTY LIST PHILIPPINES",
                   str_replace_all(str_extract(candidate, " \\(.*\\)$"), "\\(|\\)", ""), NA),
    candidate = str_replace(candidate, " \\(.*\\)$", "")
  )

repo.url <- "https://raw.githubusercontent.com/tjpalanca/ph-elections-2016-analysis/master/"

# Fingerprint Plots ---------------------------------------------------------------------------

# General Plotting Function

GenerateFingerPrintPlot <- function (contest, ncol) {
  ggplot(data = results.dt %>% filter(contest_name == contest),
         aes(x = voter_turnout, y = votes_prop)) +
    facet_wrap(~candidate, ncol = ncol, scales = "free_y") +
    geom_point(size = 0.1, alpha = 0.8, color = "gray") +
    geom_density_2d(color = "forestgreen", lwd = 0.5) +
    labs(x = "Voter Turnout (%)", y = "Proportion of Votes for Candidate (%)") +
    ggtitle(paste0(capitalize(tolower(str_replace(contest, " PHILIPPINES$", ""))),
                   "ial Race \nElection Fingerprints")) +
    theme_minimal() +
    theme(legend.position = "none",
          text = element_text("Roboto"),
          strip.text = element_text(face = "bold", size = ifelse(ncol == 3, 12, 8)),
          panel.grid = element_blank(),
          plot.title = element_text(hjust = 0, face = "bold", size = 16),
          plot.background = element_rect("#fafafa", NA)) +
    coord_cartesian(xlim = c(50, 100))
}

# Output Plots

png("output/01-fig-fingerprint-president.png", width = 1600, height = 1000, res = 150)
GenerateFingerPrintPlot("PRESIDENT PHILIPPINES", 3)
dev.off()

png("output/02-fig-fingerprint-vice-president.png", width = 1600, height = 1000, res = 150)
GenerateFingerPrintPlot("VICE-PRESIDENT PHILIPPINES", 3)
dev.off()

png("output/03-fig-fingerprint-senator.png", width = 1600, height = 3000, res = 150)
GenerateFingerPrintPlot("SENATOR PHILIPPINES", 5)
dev.off()

# Generate UI ---------------------------------------------------------------------------------

tabsetPanel(
  tabPanel(
    title = "President",
    tags$img(src = paste0(repo.url, "output/01-fig-fingerprint-president.png"))
  ),
  tabPanel(
    title = "Vice-President",
    tags$img(src = paste0(repo.url, "output/02-fig-fingerprint-vice-president.png"))
  ),
  tabPanel(
    title = "Senator",
    tags$img(src = paste0(repo.url, "output/03-fig-fingerprint-senator.png"))
  )
)

# Fraud Score ---------------------------------------------------------------------------------

# As defined in http://www.pnas.org/content/109/41/16469.full.pdf, skewness is highly negative
# and kurtosis is highly positive for candidates that have high fraud risk.

results_scaled.dt <-
  results.dt %>%
  mutate(scaled_votes = log((registered_voters - votes)/votes)) %>% # scaled votes
  filter(!is.na(scaled_votes) & !is.infinite(scaled_votes)) # remove invalid and infinite values

results_candidate.dt <-
  results_scaled.dt %>%
  group_by(contest_name, candidate) %>% # group by candidate name
  mutate(z_scaled_votes = (scaled_votes - mean(scaled_votes))/sd(scaled_votes)) %>% # compute z-score
  summarise(
    skewness_votes = skewness(z_scaled_votes),
    kurtosis_votes = kurtosis(z_scaled_votes))

sk.hc <-
  highchart(
    theme = hc_theme_smpl()
  ) %>%
  hc_xAxis(
    title = list(text = "Skewness of Rescaled votes",
                 style = list("font-size" = "150%")),
    gridLineWidth = 0,
    tickWidth = 0
  ) %>%
  hc_yAxis(
    title = list(text = "Kurtosis of Rescaled votes",
                 style = list("font-size" = "150%")),
    gridLineWidth = 0,
    lineWidth = 1
  ) %>%
  hc_annotations(
    list(
      xValue = -3,
      yValue = 15,
      title = list(text = "Test Annotation"),
      shape = list(type = "circle", params = list(r = 220, fill = "rgba(255,0,0,0.2)", stroke = FALSE))
    ),
    list(
      xValue = -3,
      yValue = 15,
      title = list(text = "Test Annotation"),
      shape = list(type = "circle", params = list(r = 150, fill = "rgba(255,0,0,0.2)", stroke = FALSE))
    )
  ) %>%
  hc_title(
    align = 'left',
    text = 'Electoral Exceptions',
    style = list(
      fontFamily = 'Roboto',
      fontSize = '24px'
    )

  ) %>%
  hc_subtitle(
    text = 'Analysis of skewness and kurtosis of scaled candidate winning percentages, 2016 Philippine National Election',
    style = list(
      fontFamily = 'Roboto',
      fontSize = '12px'
    )
  ) %>%
  hc_add_series_list(
    lapply(
      unique(results_candidate.dt$contest_name),
      function(contest) {
        list(
          type = "scatter",
          name = contest,
          marker = list(
            lineWidth = 0,
            fillOpacity = 0.4,
            radius = 3,
            symbol = "circle"
          ),
          data =
            lapply(
              which(results_candidate.dt$contest_name == contest),
              function(i) {
                list(
                  x = results_candidate.dt$skewness_votes[i],
                  y = results_candidate.dt$kurtosis_votes[i],
                  name = results_candidate.dt$candidate[i]
                )
              }
            ),
          tooltip = list(
            pointFormat =
              paste0(
                "<strong>{point.name} </strong><br>",
                "Skewness: {point.x:.2f}<br>",
                "Kurtosis: {point.y:.2f}"
              )
          )
        )
      }
    )
  ) %>%
  hc_chart(
    style =
      list(
        fontFamily = 'Roboto'
      ),
    backgroundColor = '#fafafa'
  )

saveWidget(
  sk.hc,
  "04-skewness-kurtosis-chart.html",
  selfcontained = F,
  background = "#fafafa",
  libdir = "js"
)