ui <- fluidPage(

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
                    )
                ),
                # Show a plot of the generated distribution
                mainPanel(
                   plotOutput("meas_plot")
                )
            )
        ),

        tabPanel("Exceedances", fluid = TRUE,
             sidebarLayout(
                 sidebarPanel(
                     sliderInput("year",
                                 "Year:",
                                 min = 2015, max = 2020, value = 2020
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
                 ),

                 mainPanel(
                    plotOutput("exc_status_map")
                )
            )
        )
    )
)

