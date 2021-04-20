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
             div(
               class="row-inline",
               height=50,
               uiOutput("selectInputTrajsDuration"),
               uiOutput("selectInputTrajsBuffer")
             ),
             div(
               class="row-inline",
               height=50,
               sliderInput("trajs_running_width", "Rolling average (day)", min=1, max=30, value=7, step=1, sep=""),
               uiOutput("selectInputTrajsPlots")
             ),

             plotlyOutput("trajsPlots", height="calc(100% - 300px)") #"calc(100% - 300px)")
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
