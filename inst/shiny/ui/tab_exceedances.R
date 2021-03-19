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
        )
