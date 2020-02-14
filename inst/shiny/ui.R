ui <- fluidPage(

    # Application title
    titlePanel("CREA - Air Quality Monitoring"),

    tabsetPanel(
        # Measurements
        tabPanel("Measurements", fluid = TRUE,
            sidebarLayout(
                sidebarPanel(
                    width = 2,
                    selectInput("country",
                                "Country:",
                                choices = unique(locations$country),
                                multiple=T,
                                selected = "IN"
                    ),
                    uiOutput("selectInputCity"),
                    selectInput("poll",
                                "Pollutant:",
                                choices = polls,
                                selected = creadb::PM25
                    ),
                    selectInput("averaging",
                                "Time averaging:",
                                choices = averagings,
                                selected = "day"
                    ),
                    sliderInput("running_width", "Rolling average (day)", min=1, max=30, value=1, step=1, sep = ""
                    ),
                    sliderInput("years", "Year", min=2015, max=2020, value=c(2018, 2020), step=1, sep = ""
                    ),
                    selectInput("plot_type",
                                "Plot type",
                                choices = plot_types,
                                selected = plot_types[2]
                    ),
                    uiOutput("selectInputTarget"),
                    uiOutput("selectInputScales"),
                    downloadButton("downloadMeas", "Download")

                ),
                # Show a plot of the generated distribution
                mainPanel(
                   plotOutput("meas_plot", height = 600)
                )
            )
        ),

        tabPanel("Exceedances", fluid = TRUE,
             sidebarLayout(
                 sidebarPanel(
                     width = 2,
                     sliderInput("exc_year",
                                 "Year:",
                                 min=2015, max=2020, value=2020, sep="", step=1
                     ),
                     selectInput("exc_city",
                                 "Cities:",
                                 choices = unique(locations$city),
                                 multiple = T,
                                 selected = "Delhi"
                     ),
                     selectInput("exc_poll",
                                 "Pollutant:",
                                 choices = polls,
                                 selected = creadb::PM25
                     ),
                     selectInput("exc_aggregation_period",
                                 "Averaging time:",
                                 choices = unique(standards$aggregation_period),
                                 multiple = T,
                                 selected = unique(standards$aggregation_period)
                     ),
                     selectInput("exc_standard_org",
                                 "Standard source:",
                                 choices = unique(standards$organization),
                                 multiple = T,
                                 selected = c("EU","WHO","NAAQS")
                     ),
                     downloadButton("exc_download", "Download")

                 ),

                 mainPanel(
                    # plotOutput("exc_status_map"),
                    # DT::dataTableOutput("exc_status_table")
                    DT::dataTableOutput("exc_table")
                 )
            )
        )
    )
)

