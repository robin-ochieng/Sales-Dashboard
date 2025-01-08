# UI for data table
dataTableUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    box(title = "New Business Sales Data", status = "white", solidHeader = TRUE, width = 12,
                    DTOutput(ns("sales_table_new_business")) %>% withSpinner())
              )
  
}

# Server logic for data table
dataTableServer <- function(id, filtered_data__new_business) {
  moduleServer(id, function(input, output, session) {
    output$sales_table_new_business <- renderDT({
      datatable(filtered_data__new_business(), filter = 'top', options = list(scrollX = TRUE, pageLength = 20))
    })
  })
}
