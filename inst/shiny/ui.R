library(shiny)
library(shinydashboard)

library(shinyBS)
library(leaflet)
library(plotly)

ui <- navbarPage(
    title=div(img(src="crea_logo.svg",
                  height=44)),
    windowTitle="CREA - Air Quality Dashboard",
    theme = "theme.css",
    id = "nav-page",
        # ,

    # tabsetPanel(id = "tabSetPanel1",
        # Measurements
        tabPanel("Measurements",
                 value="measurements",
                 class ="no-padding-tab",
            sidebarLayout(
                sidebarPanel(
                    width = 2,
                    h4("Data selection"),
                    uiOutput("selectInputCountry"),
                    # selectInput("regionLevel",
                    #             "Region Level:",
                    #             choices = list("station"="station","city"="city"),
                    #             multiple=F,
                    #             selected = "city"
                    # ),
                    uiOutput("selectInputRegion"),
                    selectInput("poll",
                                "Pollutant:",
                                choices = polls,
                                selected = rcrea::PM25,
                                multiple = T
                    ),
                    selectInput("averaging",
                                "Time averaging:",
                                choices = averagings,
                                selected = "day"
                    ),
                    sliderInput("years", "Year", min=2010, max=2021, value=c(2018, 2021), step=1, sep = "", ticks = F
                    ),
                    actionButton("meas_refresh",
                                 "Refresh Measurements",
                                 class="btn-primary"),
                    h4("Display options", id="h4_display"),
                    uiOutput("selectInputSources"),
                    sliderInput("running_width", "Rolling average (day)", min=1, max=30, value=14, step=1, sep = ""
                    ),
                    sliderInput("months", "Month", min=1, max=12, value=c(1, 12), step=1, sep = "", ticks = F
                    ),
                    selectInput("plot_type",
                                "Plot type",
                                choices = plot_types,
                                selected = plot_types[2]
                    ),
                    conditionalPanel( condition = "input.plot_type=='ts'",
                                      checkboxInput("overlayCities", "Overlay cities", value=FALSE)),
                    # uiOutput("selectInputProcess"),
                    selectInput("process",
                                label=span("Processing:",
                                           # tags$style(type = "text/css", "{width: 100%; justify-content: space-between;}"),
                                           bsButton("qProcess", label = "", class="btn-help", icon = icon("question"), style = "info", size = "extra-small"),
                                           class="inline-flex"),
                                multiple=T, choices = c(), selected = NULL),
                    bsPopover(id = "qProcess",
                              title = "Process Ids",
                              placement="right",
                              paste0(
                                  "<ul>",
                                  "<li><b>city_day:</b> Daily <b>observed</b> level [µg/m3 or ppm]</li>",
                                  "<li><b>anomaly:</b> <b>Deweathered</b> indication of how observed values differs from what would be expected in these weather conditions [µg/m3 or ppm]</li>",
                                  "<li><b>counterfactual:</b> Predicted level based on weather conditions (also called <i>predicted</i> [µg/m3 or ppm]</li>",
                                  "<li><b>anomaly_vs_counterfactual:</b> <b>Deweathered</b> indication of how observed values differs from what would be expected in these weather conditions, ",
                                  "expressed as (observed-predicted)/predicted [%]</li>",
                                  "<li><b>anomaly_offsetted:</b> <b>Deweathered</b> indication of how observed values differs from what would be expected in these weather conditions, ",
                                  "brought back to an absolute scale (observed-predicted) + average [µg/m3 or ppm]</li>",
                                  "<li><b>trend</b>: long-term <b>deweathered</b> trend [µg/m3 or ppm]</li>",
                                  "</ul>")
                    ),

                    uiOutput("selectInputTarget"),
                    uiOutput("selectInputScale"),
                    downloadButton(outputId="download_csv", "Download (.csv)", class="btn-secondary"),
                    downloadButton("download_rds", "Download (.rds)", class="btn-secondary")

                ),
                # Show a plot of the generated distribution
                mainPanel(
                   width=10,
                   htmlOutput("meas_plot_message", class="plot-msg"),
                   plotOutput("meas_plot", height = 800)  %>% withSpinner(color="#0dc5c1")
                   # DT::dataTableOutput("processes_table")
                )
            )
        ),

#         tabPanel("Exceedances", fluid = TRUE,
#              sidebarLayout(
#                  sidebarPanel(
#                      width = 2,
#                      selectInput("exc_country",
#                                  "Country:",
#                                  choices = unique(locations$country),
#                                  multiple=T,
#                                  selected = "IN"
#                      ),
#                      uiOutput("selectInputExcCity"),
#
#
#                      sliderInput("exc_year",
#                                  "Year:",
#                                  min=2015, max=2020, value=2020, sep="", step=1, ticks = F
#                      ),
#
#                      pickerInput("exc_status",
#                                  "Status",
#                                  choices = exc_status_labels,
#                                  options = list(`actions-box` = TRUE,
#                                                 `selected-text-format` = "count > 3"),
#                                  multiple = T,
#                                  selected = exc_status_labels),
#
#                      pickerInput("exc_poll",
#                                  "Pollutant",
#                                  choices = polls,
#                                  options = list(`actions-box` = TRUE,
#                                                 `selected-text-format` = "count > 3"),
#                                  multiple = T,
#                                  selected = polls),
#
#                      pickerInput("exc_aggregation_period",
#                                  "Aggregation period:",
#                                  choices = unique(standards$aggregation_period),
#                                  options = list(`actions-box` = TRUE,
#                                                 `selected-text-format` = "count > 3"),
#                                  multiple = T,
#                                  selected = unique(standards$aggregation_period)
#                      ),
#
#                      pickerInput("exc_standard_org",
#                                  "Standard source:",
#                                  choices = unique(standards$organization),
#                                  multiple = T,
#                                  options = list(`actions-box` = TRUE),
#                                  selected = c("EU","WHO","NAAQS")
#                      ),
#                      downloadButton("exc_download_csv", "Download (.csv)"),
#                      downloadButton("exc_download_rds", "Download (.rds)")
#
#                  ),
#
#                  mainPanel(
#                      width=10,
#                     # plotOutput("exc_status_map"),
#                     # DT::dataTableOutput("exc_status_table")
#                     DT::dataTableOutput("exc_table")
#                  )
#             )
#         ),

        tabPanel("Trajectories",
                 value="trajectories",
                 class = "no-padding-tab",
                     sidebarLayout(
                         mainPanel(
                             width=8,
                             leafletOutput("maptrajs", height = "calc(100%)"),
                             absolutePanel(left=25,
                                           top=10,
                                           width=160,
                                           htmlOutput("trajsInfos", height="120px")

                             ),
                             absolutePanel(bottom = 10, right = "10%", width="80%",
                                           uiOutput("selectInputTrajsDates", height = "30px")
                             )
                         ),
                         sidebarPanel(
                             width = 4,
                             div(
                                 class="row-inline",
                                 height=50,
                                 uiOutput("selectInputTrajsCountry"),
                                 uiOutput("selectInputTrajsCity")
                             ),

                             sliderInput("trajs_running_width", "Rolling average (day)", min=1, max=30, value=7, step=1, sep = ""),
                             plotlyOutput("trajsPlots", height=600) #"calc(100% - 300px)")
                             # verbatimTextOutput("trajsLogs", placeholder = TRUE)

                         )

                     )
            )
        # tabPanel("Trajectories", value="trajectories", fluid = TRUE,
        #          sidebarLayout(
        #              sidebarPanel(
        #                  width = 2,
        #                  uiOutput("selectInputTrajsCountry"),
        #                  uiOutput("selectInputTrajsCity"),
        #                  uiOutput("selectInputTrajsDates")
        #
        #                  # downloadButton("trajs_download_jpg", "Download (.jpg)"),
        #              ),
        #
        #              mainPanel(
        #                  width=10,
        #                  uiOutput("imageTrajs")  %>% withSpinner(color="#0dc5c1")
        #                  # plotOutput("exc_status_map"),
        #                  # DT::dataTableOutput("exc_status_table")
        #                  # DT::dataTableOutput("trajs_table")
        #              )
        #          )
        # )
#
#         tabPanel("Download", fluid = TRUE,
#                  sidebarLayout(
#                      sidebarPanel(
#                          width = 3,
#                          selectInput("down_country",
#                                      "Country:",
#                                      choices = unique(locations$country),
#                                      multiple=T,
#                                      selected = "IN"
#                          ),
#                          uiOutput("selectInputDownCity"),
#                          pickerInput("down_poll",
#                                      "Pollutant",
#                                      choices = polls,
#                                      options = list(`actions-box` = TRUE,
#                                                     `selected-text-format` = "count > 3"),
#                                      multiple = T,
#                                      selected = polls),
#                          sliderInput("down_years",
#                                      "Year:",
#                                      min=2015, max=2020, value=c(2018, 2020), sep="", step=1, ticks = F
#                          ),
#                          selectInput("down_averaging",
#                                      "Time averaging:",
#                                      choices = averagings,
#                                      selected = "day"
#                          ),
#                          actionButton("down_refresh", "Refresh Measurements!")
#
#                      ),
#
#                      mainPanel(
#                          width=9,
#                          # plotOutput("exc_status_map"),
#                          # DT::dataTableOutput("exc_status_table")
#                          DT::dataTableOutput("down_table") %>% withSpinner(color="#0dc5c1"),
#                          downloadButton("down_download_csv", "Download (.csv)"),
#                          downloadButton("down_download_rds", "Download (.rds)")
#                      )
#                  )
#         )
    # )
)


