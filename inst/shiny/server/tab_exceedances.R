# Reactive Values ---------------------------------------
# countries_map <- reactive({
#     getData("GADM", country=input$country, level=1)
# })

exc <- reactive({
  country <- input$exc_country
  year <- input$exc_year
  city <- input$exc_city
  req(year, country, city)

  # Get exceedances status
  exceedances(country=country, city=city, year=year) %>%
    dplyr::mutate(status=cut(
      ifelse(exceedance_allowed_per_year==0,
             exceedance_this_year, exceedance_this_year/exceedance_allowed_per_year),
      breaks=exc_status_breaks,
      labels=exc_status_labels
    ))
})


# Downloadable csv of selected dataset ----
output$exc_download_csv <- downloadHandler(
  filename = function() {
    paste("standard_exceedances.csv", sep = "")
  },
  content = function(file) {
    write.csv(exc(), file, row.names = FALSE)
  }
)

output$exc_download_rds <- downloadHandler(
  filename = function() {
    paste("standard_exceedances.rds", sep = "")
  },
  content = function(file) {
    saveRDS(exc(), file)
  }
)

# Output Elements --------------------------------------
output$selectInputExcCity <- renderUI({
  cities <- unique((locations %>% dplyr::filter(country==input$country))$city)
  pickerInput("exc_city","City", choices=cities, options = list(`actions-box` = TRUE),multiple = T)
  # selectInput("exc_city", "City:", multiple=T, selected = cities, choices = cities)
})

output$exc_table <- DT::renderDataTable({
  DT::datatable(data=exc() %>%
                  dplyr::filter(aggregation_period %in% input$exc_aggregation_period) %>%
                  dplyr::filter(poll %in% input$exc_poll) %>%
                  dplyr::filter(standard_org %in% input$exc_standard_org) %>%
                  dplyr::filter(status %in% input$exc_status) %>%
                  dplyr::mutate(threshold_str=paste0(threshold," ", unit, " [", aggregation_period,"]")) %>%
                  dplyr::select(
                    city,
                    poll,
                    status,
                    exceedance_this_year,
                    exceedance_allowed_per_year,
                    breach_date,
                    threshold_str,
                    standard_org,
                  ),
                options = list(
                  columnDefs = list(list(visible=FALSE, targets=c())),
                  pageLength = 15,
                  autoWidth = TRUE,
                  rowCallback = JS(
                    "function(row, data) {",
                    "var n_exc = data[3];",
                    "var str_exc = n_exc < 1 ? (n_exc * 100).toFixed(0).toString() + '%' :  Math.floor(n_exc).toString() + ' times';",
                    "$('td:eq(3)', row).html(str_exc);",
                    "}"
                  )
                  # callback = JS("var tips = ['tooltip1', 'tooltip2', 'tooltip3', 'tooltip4', 'tooltip5'],
                  #             firstRow = $('#exc_status_table thead tr th');
                  #             for (var i = 0; i < tips.length; i++) {
                  #               $(firstRow[i]).attr('title', tips[i]);
                  #             }")
                ),
                rownames = FALSE,
  ) %>%
    formatDate(c(6), "toLocaleDateString") %>%
    formatStyle(
      'status',
      target = 'row',
      backgroundColor = styleEqual(exc_status_labels, exc_status_colours)
    )
})
