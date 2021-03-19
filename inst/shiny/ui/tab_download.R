

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
