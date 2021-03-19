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
)
