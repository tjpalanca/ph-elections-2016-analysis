# PHILIPPINES 2016 ELECTIONS RESULTS ANALYSIS
# Voter Turnout vs Votes for Candidate
# contact@tjpalanca.com | 2016-05-16

# Libraries -----------------------------------------------------------------------------------
library(dplyr)
library(highcharter)
library(htmlwidgets)
library(shiny)

# Setup ---------------------------------------------------------------------------------------

results.dt <- readRDS('../data/02_results_per_citymuni.rds')

# App -----------------------------------------------------------------------------------------

server <- function(input, output) {
  data <- reactive({
    req(input$contest_name)
    req(input$candidate)
    results.dt %>%
      filter(
        contest_name == input$contest_name,
        candidate == input$candidate
      )
  })

  output$selector_contest_name <- renderUI({
    selectInput(
      'contest_name',
      label = "Contest",
      choices = unique(results.dt$contest_name),
      width = "100%"
    )
  })

  output$selector_candidate <- renderUI({
    req(input$contest_name)
    selectInput(
      'candidate',
      label = "Candidate",
      choices = unique(results.dt$candidate[results.dt$contest_name == input$contest_name &
                                              !is.na(results.dt$candidate)]),
      width = "100%"
    )
  })

  output$chart <- renderHighchart({
    highchart() %>%
      hc_xAxis(
        title = list(text = "Voter Turnout (%)",
                     style = list("font-size" = "150%")),
        gridLineColor = "#D8D8D8",
        gridLineDashStyle = "Solid",
        gridLineWidth = 1,
        floor = 0
      ) %>%
      hc_yAxis(
        title = list(text = "Votes for Candidate (%)",
                     style = list("font-size" = "150%")),
        floor = 0, ceiling = 100
      ) %>%
      hc_add_series_list(
        lapply(
          unique(data()$region),
          function(region) {
            list(
              type = "bubble",
              name = region,
              candidate = unique(data()$candidate),
              marker = list(
                lineWidth = 0,
                fillOpacity = 0.4,
                radius = 0.5
              ),
              data =
                lapply(
                  which(data()$region == region),
                  function(i) {
                    list(
                      citymuni = data()$citymuni[i],
                      province = data()$province[i],
                      x = data()$voter_turnout[i],
                      y = data()$votes_prop[i],
                      z = data()$ballots_reported[i],
                      candidate = data()$candidate[i]
                    )
                  }
                ),
              tooltip = list(
                headerFormat =
                  paste0(
                    ""
                  ),
                pointFormat =
                  paste0(
                    "<strong>{point.candidate}</strong><br>",
                    "<strong>City/Municipality:</strong> {point.citymuni}, {point.province}<br>",
                    "<strong>Voter Turnout: </strong>{point.x:.2f}%</strong><br>",
                    "<strong>Votes for Candidate:</strong> {point.y:.2f}%<br>",
                    "<strong>Ballots Reported:</strong> {point.z:.0f}"
                  )
              )
            )
          }
        )
      ) %>%
      hc_plotOptions(
        bubble = list(
            maxSize = "3%"
        )
      )
  })
}

ui <- fluidPage(
  theme = "http://tjpalanca.com/assets/css/paper.css",
    fluidRow(
        column(
          width = 12,
          div(
            h4(strong("Election Result Analyzer")),
            p(paste0("Select a contest and a candidate to view their voter turnout vs votes proportion charts.",
                     "When a candidate has a significant number of high-turnout, high-votes-percentage combinations,",
                     "it may be a sign that those areas were subjected to vote padding.")),
            fluidRow(
              column(
                width = 6,
                uiOutput('selector_contest_name')
              ),
              column(
                width = 6,
                uiOutput('selector_candidate')
              )
            ),
            highchartOutput('chart', height = "500px"),
            tags$ul(
              tags$li("Each city/municipality is represented by a circle. The color represents the region to which the city/municipality belongs."),
              tags$li("Hover over each circle to view details about that city/municipality."),
              tags$li("Click on the region name in the legend to hide/unhide certain regions.")
            )
          )
      )

    )
  )

shinyApp(ui, server)

# Deploy --------------------------------------------------------------------------------------

# deployApp(appDir = 'ap')
