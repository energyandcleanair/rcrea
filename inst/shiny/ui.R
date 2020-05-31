ui <- fluidPage(
    theme = "theme.css",

    # Application title
    titlePanel(windowTitle="CREA - Air Quality Monitoring", title=div(img(src="crea_logo.svg", width=220))),

    tabsetPanel(
        # Measurements
        tabPanel("Measurements", fluid = TRUE,
            sidebarLayout(
                sidebarPanel(
                    width = 2,
                    h4("Data selection"),
                    selectInput("source",
                                "Source:",
                                choices = unique(sources),
                                multiple=F,
                                selected = "openaaq"
                    ),

                    uiOutput("selectInputCountry"),
                    selectInput("regionLevel",
                                "Region Level:",
                                choices = list("city"="city","county"="gadm2","state/province"="gadm1","country"="country"),
                                multiple=F,
                                selected = "city"
                    ),
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
                    sliderInput("years", "Year", min=2015, max=2020, value=c(2018, 2020), step=1, sep = "", ticks = F
                    ),
                    actionButton("meas_refresh", "Refresh Measurements"),
                    h4("Display options"),
                    sliderInput("running_width", "Rolling average (day)", min=1, max=30, value=1, step=1, sep = ""
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
                    uiOutput("selectInputProcess"),
                    uiOutput("selectInputTarget"),
                    uiOutput("selectInputScale"),
                    downloadButton("download_csv", "Download (.csv)"),
                    downloadButton("download_rds", "Download (.rds)"),

                ),
                # Show a plot of the generated distribution
                mainPanel(
                   width=10,
                   plotOutput("meas_plot", height = 800)  %>% withSpinner(color="#0dc5c1"),
                   DT::dataTableOutput("processes_table")
                )
            )
        ),

        tabPanel("Exceedances", fluid = TRUE,
             sidebarLayout(
                 sidebarPanel(
                     width = 2,
                     selectInput("exc_country",
                                 "Country:",
                                 choices = unique(locations$country),
                                 multiple=T,
                                 selected = "IN"
                     ),
                     uiOutput("selectInputExcCity"),


                     sliderInput("exc_year",
                                 "Year:",
                                 min=2015, max=2020, value=2020, sep="", step=1, ticks = F
                     ),

                     pickerInput("exc_status",
                                 "Status",
                                 choices = exc_status_labels,
                                 options = list(`actions-box` = TRUE,
                                                `selected-text-format` = "count > 3"),
                                 multiple = T,
                                 selected = exc_status_labels),

                     pickerInput("exc_poll",
                                 "Pollutant",
                                 choices = polls,
                                 options = list(`actions-box` = TRUE,
                                                `selected-text-format` = "count > 3"),
                                 multiple = T,
                                 selected = polls),

                     pickerInput("exc_aggregation_period",
                                 "Aggregation period:",
                                 choices = unique(standards$aggregation_period),
                                 options = list(`actions-box` = TRUE,
                                                `selected-text-format` = "count > 3"),
                                 multiple = T,
                                 selected = unique(standards$aggregation_period)
                     ),

                     pickerInput("exc_standard_org",
                                 "Standard source:",
                                 choices = unique(standards$organization),
                                 multiple = T,
                                 options = list(`actions-box` = TRUE),
                                 selected = c("EU","WHO","NAAQS")
                     ),
                     downloadButton("exc_download_csv", "Download (.csv)"),
                     downloadButton("exc_download_rds", "Download (.rds)")

                 ),

                 mainPanel(
                     width=10,
                    # plotOutput("exc_status_map"),
                    # DT::dataTableOutput("exc_status_table")
                    DT::dataTableOutput("exc_table")
                 )
            )
        ),
        tabPanel("Download", fluid = TRUE,
                 sidebarLayout(
                     sidebarPanel(
                         width = 3,
                         selectInput("down_country",
                                     "Country:",
                                     choices = unique(locations$country),
                                     multiple=T,
                                     selected = "IN"
                         ),
                         uiOutput("selectInputDownCity"),
                         pickerInput("down_poll",
                                     "Pollutant",
                                     choices = polls,
                                     options = list(`actions-box` = TRUE,
                                                    `selected-text-format` = "count > 3"),
                                     multiple = T,
                                     selected = polls),
                         sliderInput("down_years",
                                     "Year:",
                                     min=2015, max=2020, value=c(2018, 2020), sep="", step=1, ticks = F
                         ),
                         selectInput("down_averaging",
                                     "Time averaging:",
                                     choices = averagings,
                                     selected = "day"
                         ),
                         actionButton("down_refresh", "Refresh Measurements!")

                     ),

                     mainPanel(
                         width=9,
                         # plotOutput("exc_status_map"),
                         # DT::dataTableOutput("exc_status_table")
                         DT::dataTableOutput("down_table") %>% withSpinner(color="#0dc5c1"),
                         downloadButton("down_download_csv", "Download (.csv)"),
                         downloadButton("down_download_rds", "Download (.rds)")
                     )
                 )
        )
    )
)

