down <- reactive({

  # Take a dependency on input$down_refresh
  input$down_refresh

  source <- isolate(input$source)
  country <- isolate(input$down_country)
  years <- isolate(input$down_years)
  city <- isolate(input$down_city)
  poll <- isolate(input$down_poll)
  averaging <- isolate(input$down_averaging)
  req(years, country, city, poll, averaging)

  date_from <- lubridate::ymd(years[1]*10000+101)
  date_to <- lubridate::ymd(years[2]*10000+1231)

  # Get measurements
  rcrea::measurements(country=country, city=city, poll=poll, date_from=date_from, date_to=date_to,
                      average_by=averaging, source=source, with_metadata = T) %>%
    dplyr::select(location_id, location_name, date, poll, unit, source, timezone, process_id) %>%
    dplyr::arrange(desc(date))
})


output$selectInputDownCity <- renderUI({
  cities <- unique((locations %>% dplyr::filter(country==input$down_country))$city)
  pickerInput("down_city","City", choices=cities, options = list(`actions-box` = TRUE), multiple = T)
  # selectInput("exc_city", "City:", multiple=T, selected = cities, choices = cities)
})

output$down_table <- DT::renderDataTable({
  format_date_str <- ifelse(input$down_averaging=='hour', 'toLocaleTimeString', 'toLocaleDateString')
  DT::datatable(data=down(),
                options = list(
                  columnDefs = list(list(visible=FALSE, targets=c())),
                  pageLength = 15,
                  autoWidth = TRUE,
                  rowCallback = JS(
                    "function(row, data) {",
                    "var timezone = data[5];",
                    "var datetime = new Date(Date.parse(data[1]));",
                    "const options = {
                                  timeZone: timezone,
                              };",
                    "var str_datetime = datetime.toLocaleDateString('en-GB',options) +'  '+ datetime.toLocaleTimeString('en-US',options);",
                    "$('td:eq(1)', row).html(str_datetime);",
                    "}"
                  )
                ),
                rownames = FALSE
  ) # %>% formatDate(c(2), format_date_str)
})

output$down_download_csv <- downloadHandler(
  filename = function() {
    paste("measurements.csv", sep = "")
  },
  content = function(file) {
    write.csv(down(), file, row.names = FALSE)
  }
)

output$down_download_rds <- downloadHandler(
  filename = function() {
    paste("measurements.rds", sep = "")
  },
  content = function(file) {
    saveRDS(down(), file)
  }
)
