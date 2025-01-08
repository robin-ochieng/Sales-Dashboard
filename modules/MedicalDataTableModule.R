# UI for data table
MedicaldataTableUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    box(title = "Medical Business Sales Data", status = "white", solidHeader = TRUE, width = 12,
                    DTOutput(ns("sales_table_health")) %>% withSpinner())
              )
  
}

# Server logic for data table
MedicaldataTableServer <- function(id, filtered_data__health_business) {
  moduleServer(id, function(input, output, session) {
  output$sales_table_health <- renderDT({
    datatable(filtered_data__health_business(), filter = 'top',
              options = list(scrollX = TRUE, pageLength = 20))
  })
  })
}
