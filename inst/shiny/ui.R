ui <- fillPage(

    # Application title
    titlePanel("CREA - Air Quality Monitoring"),

    tabsetPanel(
        # Measurements
        tabPanel("Measurements", fluid = TRUE,
            sidebarLayout(
                sidebarPanel(
                    selectInput("city",
                                "City:",
                                choices = locations$city,
                                multiple=T,
                                selected = "Delhi"
                    ),
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
                     sliderInput("year",
                                 "Year:",
                                 min=2015, max=2020, value=2020, sep="", step=1
                     ),
                     selectInput("country",
                                 "Country:",
                                 choices = unique(locations$country),
                                 multiple = F,
                                 selected = "IN"
                     ),
                     selectInput("standard_org",
                                 "Standard source:",
                                 choices = unique(standards$organization),
                                 multiple = T,
                                 selected = c("EU","WHO")
                     ),
                     downloadButton("downloadExcs", "Download")

                 ),

                 mainPanel(
                    plotOutput("exc_status_map"),
                    dataTableOutput("exc_status_table")
                 )
            )
        )
    )
)

