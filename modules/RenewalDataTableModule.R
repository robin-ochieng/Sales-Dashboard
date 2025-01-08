# UI for data table
RenewaldataTableUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    box(title = "Renewal Business Sales Data", status = "white", solidHeader = TRUE, width = 12,
                    DTOutput(ns("sales_table_renewals")) %>% withSpinner())
              )
  
}

# Server logic for data table
RenewaldataTableServer <- function(id, filtered_data__renewal_business) {
  moduleServer(id, function(input, output, session) {
  output$sales_table_renewals <- renderDT({
    datatable(filtered_data__renewal_business(), filter = 'top',
              options = list(scrollX = TRUE, pageLength = 20))
  })
  })
}
