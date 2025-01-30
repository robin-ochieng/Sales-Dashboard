# UI for sales dashboard including graphs
salesDashboardUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      valueBoxOutput(ns("total_sales")),
      valueBoxOutput(ns("avg_sales_day")),
      valueBoxOutput(ns("count_sales"))
    ),
    fluidRow(       
      box(title = "Sales by Day of the Week", status = "white", solidHeader = TRUE, 
          plotlyOutput(ns("sales_by_day")) %>% withSpinner(type = 6)),
      box(title = "Sales Over Time", status = "white", solidHeader = TRUE,
          plotlyOutput(ns("sales_over_time")) %>% withSpinner(type = 6)),
      box(title = "Sales by Day Heatmap", status = "white", solidHeader = TRUE,
          plotlyOutput(ns("calendar_plot")) %>% withSpinner(type = 6)),
      tabBox( 
          solidHeader = TRUE,
          selected = "Count by Insurer",
          status = "white",
          type = "tabs",
          id = ns("insurer_tabs"),
          tabPanel("Sales by Insurer", plotlyOutput(ns("sales_by_insurer")) %>% withSpinner(type = 6)),
          tabPanel("Count by Insurer", plotlyOutput(ns("salescount_by_insurer")) %>% withSpinner(type = 6))
        ),
      tabBox( 
          solidHeader = TRUE,
          selected = "Count by Time of Day",
          status = "white",
          type = "tabs",
          id = ns("timeofday_tabs"),
          tabPanel("Sales by Time of Day", plotlyOutput(ns("sales_by_time_of_day")) %>% withSpinner(type = 6)),
          tabPanel("Count by Time of Day", plotlyOutput(ns("salescount_by_time_of_day")) %>% withSpinner(type = 6))
        ),
      tabBox( 
          solidHeader = TRUE,
          selected = "Count by Cover Type",
          status = "white",
          type = "tabs",
          id = ns("covertype_tabs"),
          tabPanel("Sales by Cover Type", plotlyOutput(ns("sales_by_cover_type")) %>% withSpinner(type = 6)),
          tabPanel("Count by Cover Type", plotlyOutput(ns("salescount_by_cover_type")) %>% withSpinner(type = 6))
        ),
      tabBox( 
          solidHeader = TRUE,
          selected = "Count by SalesPerson",
          status = "white",
          type = "tabs",
          id = ns("sales_tabs"),
          tabPanel("Sales by SalesPerson", plotlyOutput(ns("sales_by_person")) %>% withSpinner(type = 6)),
          tabPanel("Count by SalesPerson", plotOutput(ns("sales_count_by_person")) %>% withSpinner(type = 6))
        ),
      tabBox(
          solidHeader = TRUE,
          selected = "Count by Source",
          type = "tabs",
          status = "white",
          id = ns("count_tabs"),
          tabPanel("Sales by Source", plotlyOutput(ns("sales_by_source")) %>% withSpinner(type = 6)),
          tabPanel("Count by Source", plotOutput(ns("sales_count_by_source")) %>% withSpinner(type = 6))
        )
    )
  )
}


# Server logic for sales dashboard
salesDashboardServer <- function(id, filtered_data__new_business, new_business_month) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns  # Namespace function to handle IDs

    # Output for total sales value box
    output$total_sales <- renderValueBox({
      sales_sum <- sum(filtered_data__new_business()$Sales, na.rm = TRUE)
      formatted_sales_sum <- format(sales_sum, big.mark = ",")
      valueBox(
        formatted_sales_sum,
        "Total Sales (KES)",
        icon = icon("usd"),
        color = "white"
      )
    })

    # Output for average sales per day value box
    output$avg_sales_day <- renderValueBox({
      avg_per_day <- filtered_data__new_business() %>%
        group_by(Date) %>%
        summarize(DailySales = sum(Sales, na.rm = TRUE)) %>%
        summarise(Average = mean(DailySales))
      valueBox(
        formatted_average <- format(round(avg_per_day$Average, 0), big.mark = ","),
        "Average Sales Per Day (KES)",
        icon = icon("calendar"),
        color = "white"
      )
    })

    # Output for count of sales value box
    output$count_sales <- renderValueBox({
      valueBox(
        nrow(filtered_data__new_business()),
        "Count of Sales",
        icon = icon("shopping-cart"),
        color = "white"
      )
    })

    # Plot for sales by day of the week
    output$sales_by_day <- renderPlotly({
      data <- filtered_data__new_business() %>%
        mutate(DayOfWeek = lubridate::wday(Date, label = TRUE)) %>%
        group_by(DayOfWeek) %>%
        summarize(TotalSales = sum(Sales, na.rm = TRUE)) %>%
        mutate(TotalSalesMillions = TotalSales / 1e6) %>%
        arrange(DayOfWeek)

      plot_ly(data, x = ~DayOfWeek, y = ~TotalSales, type = 'bar',
              text = ~paste(scales::comma(TotalSalesMillions, accuracy = 0.01), "M"),
              textposition = 'outside',
              hoverinfo = 'text',
              textfont = list(size = 9, color = "black"),
              marker = list(color = '#1CA4F8')) %>%
        layout(
          title = "",
          xaxis = list(title = "Day of the Week", tickfont = list(size = 10, color = "#333333")),
          yaxis = list(title = "Total Sales", tickfont = list(size = 10, color = "#333333")),
          font = list(family = "Mulish", color = "#333333"),
          plot_bgcolor = "white",
          paper_bgcolor = "white"
        )
    })

    output$sales_over_time <- renderPlotly({
    data <- filtered_data__new_business() %>%
      mutate(Date = as.Date(Date)) %>% # Ensure Date is in Date format
      arrange(Date)
    plot_ly(data, x = ~Date, y = ~Sales, type = 'scatter', mode = 'lines+markers',
            line = list(color = '#1CA4F8'), marker = list(color = '#0d6efd')) %>%
      layout(
        title = "",
        xaxis = list(title = "Date", tickfont = list(size = 10, color = "#333333")),
        yaxis = list(title = "Sales", tickfont = list(size = 10, color = "#333333")),
        font = list(family = "Mulish", color = "#333333"),
        plot_bgcolor = "white",
        paper_bgcolor = "white"
      )
  })
  
  output$sales_by_insurer <- renderPlotly({
    data <- filtered_data__new_business() %>%
      filter(!is.na(Insurer)) %>%
      group_by(Insurer) %>%
      summarize(TotalSales = sum(Sales, na.rm = TRUE)) %>%
      mutate(TotalSalesMillions = TotalSales / 1e6) %>%
      arrange(-TotalSalesMillions)  # Ensuring data is sorted in descending order
    # Resetting the factor levels based on descending order of TotalSalesMillions
    data$Insurer <- factor(data$Insurer, levels = data$Insurer)
    # Create the plot
    p <- plot_ly(data, x = ~TotalSalesMillions, y = ~Insurer, type = 'bar', orientation = 'h',
                 text = ~paste(scales::comma(TotalSalesMillions, accuracy = 0.1), "M"),
                 textposition = 'outside',
                 hoverinfo = 'text',
                 textfont = list(size = 8, color = "black"),
                 marker = list(color = '#99CCFF')) %>%
      layout(
        yaxis = list(title = "Insurer", automargin = TRUE, showgrid = FALSE, zeroline = FALSE),
        xaxis = list(title = "Total Sales (Millions KES)", showgrid = FALSE, zeroline = FALSE),
        title = "Sum of Sales by Insurer",
        margin = list(l = 100, r = 25, t = 25, b = 70), # Adjust margins to fit labels
        hoverlabel = list(bgcolor = "white"),
        font = list(family = "Mulish", size = 10), # Apply global font styling
        paper_bgcolor = 'rgba(0,0,0,0)', # Transparent background
        plot_bgcolor = 'rgba(0,0,0,0)' # Transparent background
      )
    
    return(p)
  })

  output$salescount_by_insurer <- renderPlotly({
    data <- filtered_data__new_business() %>%
      filter(!is.na(Insurer)) %>%
      group_by(Insurer) %>%
      summarize(Count = n()) %>%
      arrange(-Count)  # Ensuring data is sorted in descending order of count
    # Resetting the factor levels based on descending order of counts
    data$Insurer <- factor(data$Insurer, levels = data$Insurer)
    
    # Create the plot
    p <- plot_ly(data, x = ~Count, y = ~Insurer, type = 'bar', orientation = 'h',
                text = ~Count,
                textposition = 'outside',
                hoverinfo = 'text',
                textfont = list(size = 8, color = "black"),
                marker = list(color = '#17a2b8')) %>%
      layout(
        yaxis = list(title = "Insurer", automargin = TRUE, showgrid = FALSE, zeroline = FALSE),
        xaxis = list(title = "Number of Transactions", showgrid = FALSE, zeroline = FALSE),
        title = "Distribution of Sales Transactions by Insurer",
        margin = list(l = 100, r = 25, t = 25, b = 70), # Adjust margins to fit labels
        hoverlabel = list(bgcolor = "white"),
        font = list(family = "Mulish", size = 10), # Apply global font styling
        paper_bgcolor = 'rgba(0,0,0,0)', # Transparent background
        plot_bgcolor = 'rgba(0,0,0,0)' # Transparent background
      )
    
    return(p)
  })


  
  output$sales_by_person <- renderPlotly({
    data <- filtered_data__new_business() %>%
      group_by(SalesPerson) %>%
      summarize(TotalSales = sum(Sales, na.rm = TRUE)) %>%
      mutate(TotalSalesMillions = TotalSales / 1e6) %>%
      mutate(SalesPerson = fct_reorder(SalesPerson, TotalSalesMillions)) %>%  # Reorder factor levels
      arrange((TotalSalesMillions))
    
    plot_ly(data, x = ~TotalSalesMillions, y = ~SalesPerson, type = 'bar', orientation = 'h',
            marker = list(color = '#0d6efd')) %>%
      layout(
        title = "",
        yaxis = list(title = "SalesPerson", tickangle = 0, tickfont = list(size = 10, color = "#333333")),
        xaxis = list(title = "Total Sales (Million KES)", tickfont = list(size = 10, color = "#333333")),
        font = list(family = "Mulish", color = "#333333"),
        margin = list(b = 100), # To accommodate rotated x-axis labels
        plot_bgcolor = "white",
        paper_bgcolor = "white"
      ) %>%
      add_annotations(
        x = ~TotalSalesMillions,
        y = ~SalesPerson,
        text = ~sprintf("%.2f M", TotalSalesMillions),
        showarrow = FALSE,
        xshift = 15,
        font = list(size = 9, color = "#333333")
      )
  })
  
  custom_colors <- c("#87CEEB", "#4682B4", "#006064", "#2ca02c", "#007bff", "#9467bd", "#ff7f0e", "#1f77b4", "#d62728")
  output$sales_by_cover_type <- renderPlotly({
    data <- filtered_data__new_business()  # Ensure 'sales_data' is already loaded and contains the right columns
    # Filter out rows where Sales or Type of Cover might be NA
    data <- data[!is.na(data$Sales) & !is.na(data$`Type of Cover`), ]
    # Group data by 'Type of Cover' and sum 'Sales'
    sales_by_cover <- data %>%
      group_by(`Type of Cover`) %>%
      summarise(Sales = sum(Sales, na.rm = TRUE)) %>%
      ungroup()  # Always good to ungroup after summarise
    # Calculate percentages
    sales_by_cover$percentage <- sales_by_cover$Sales / sum(sales_by_cover$Sales) * 100
    # Generate a qualitative color palette
    num_categories <- length(unique(sales_by_cover$`Type of Cover`))
    #colors <- hcl.colors(num_categories, "Set2") 
    # Create the donut chart
    p <- plot_ly(sales_by_cover, labels = ~`Type of Cover`, values = ~Sales, type = 'pie', hole = 0.4,
                 textposition = 'inside', 
                 textinfo = 'label+value+percent',
                 insidetextorientation = 'tangential',
                 marker = list(colors = custom_colors),
                 textfont = list(color = 'white', family = "Mulish", size = 12))
    # Add title and display the plot
    p <- p %>% layout(title = "Sales by Cover Type",
                      showlegend = TRUE,
                      font = list(family = "Mulish"))
    p
  })
  
  custom_colors_cover <- c("#2176C7", "#008b8b", "#00838f", "#2ca02c", "#9467bd", "#F28E2B", "#1f77b4", "#d62728")
  output$salescount_by_cover_type <- renderPlotly({
  data <- filtered_data__new_business()  # Ensure 'sales_data' is already loaded and contains the right columns
  # Filter out rows where Sales or Type of Cover might be NA
  data <- data[!is.na(data$Sales) & !is.na(data$`Type of Cover`), ]
  # Group data by 'Type of Cover' and sum 'Sales', and count the occurrences
  sales_by_cover <- data %>%
    group_by(`Type of Cover`) %>%
    summarise(Count = n(), .groups = 'drop')
  # Generate a qualitative color palette
  num_categories <- length(unique(sales_by_cover$`Type of Cover`))
  # Create the donut chart
  p <- plot_ly(sales_by_cover, labels = ~`Type of Cover`, values = ~Count, type = 'pie', hole = 0.4,
               textposition = 'inside', 
               textinfo = 'label+value+percent',  
               insidetextorientation = 'tangential',  
               marker = list(colors = custom_colors_cover),
               textfont = list(color = 'white', family = "Mulish", size = 12))
  # Add title and display the plot
  p <- p %>% layout(title = "Distribution of Sales by Cover Type",
                    showlegend = TRUE,
                    font = list(family = "Mulish"))
  p
})


  output$calendar_plot <- renderPlotly({
    # Prepare data
    sales_data <- filtered_data__new_business() %>%
      mutate(
        Date = as.Date(Date),  # Ensure Date is in Date format
        Day = day(Date),
        WeekOfMonth = (day(Date) - 1) %/% 7 + 1,  # Calculate week of the month
        DayOfWeek = lubridate::wday(Date, label = TRUE, abbr = TRUE)  # Get abbreviated weekday names
      ) %>%
      group_by(WeekOfMonth, DayOfWeek, Day) %>%
      summarize(TotalSales = sum(Sales, na.rm = TRUE), .groups = 'drop')
    # Check if a specific month is selected (assuming input$new_business_month exists)
    specific_month_selected <- !is.null(new_business_month()) && new_business_month() != "All"
    # Generate the heatmap
    fig <- plot_ly(data = sales_data, x = ~DayOfWeek, y = ~WeekOfMonth, z = ~TotalSales, type = "heatmap",
                   colorscale = list(c(0, "lightblue"), c(1, "blue")),
                   colorbar = list(title = 'Total Sales (KES)'),
                   text = ~paste("Sales: ", scales::comma(TotalSales), " KES"), 
                   hoverinfo = "text+x+y")
    # Add day annotations
    annotations <- if (specific_month_selected) {
      lapply(1:nrow(sales_data), function(i) {
        list(
          x = sales_data$DayOfWeek[i],
          y = sales_data$WeekOfMonth[i],
          text = as.character(sales_data$Day[i]),
          xref = 'x',
          yref = 'y',
          showarrow = FALSE,
          font = list(size = 10, color = "white")
        )
      })
    } else {
      list()  # Empty list if 'All' is selected
    }
    # Adjust layout
    fig <- fig %>% layout(title = "Weekly Sales Heatmap",
                          yaxis = list(title = "Week of Month", dtick = 1, tickmode = "array",
                                       tickvals = 1:max(sales_data$WeekOfMonth, na.rm = TRUE),
                                       ticktext = paste("Week", 1:max(sales_data$WeekOfMonth, na.rm = TRUE))),
                          xaxis = list(title = "Day of Week", dtick = 1, tickmode = "array",
                                       tickvals = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"),
                                       ticktext = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")),
                          annotations = annotations,
                          font = list(family = "Mulish", color = "#333333"),
                          plot_bgcolor = "white",
                          paper_bgcolor = "white")
    fig
  })
  
  
    output$sales_count_by_person <- renderPlot({
      data <- filtered_data__new_business() %>%
        group_by(SalesPerson) %>%
        summarize(SalesCount = n(), TotalSales = sum(Sales)) %>%
        arrange(desc(SalesCount))
      ggplot(data, aes(x = reorder(SalesPerson, SalesCount), y = SalesCount, fill = "#17a2b8")) +
        geom_bar(stat = "identity") +  # Bars filled based on TotalSales
        scale_fill_identity() +  # Gradient from blue to red
        geom_text(aes(label = SalesCount), vjust = 0.5, hjust = -0.15, color = "#333333", size=3.02) +
        coord_flip() +
        theme_minimal() +
        theme(legend.position = "none",
              legend.title = element_text(family = "Mulish", size = 10),
              text = element_text(family = "Mulish"),
              axis.title = element_text(family = "Mulish", size = 12),
              axis.text = element_text(family = "Mulish"),
              axis.text.y = element_text(family = "Mulish", size = 12),
              plot.title = element_text(family = "Mulish", size = 14),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank()) +
        labs(title = "",
            y = "Count of Sales", x = "SalesPerson")
    })
    
    custom_colors2 <- c("#17a2b8", "#48d1cc", "#9467bd", "#F28E2B",  "#d62728","#1f77b4", "#F28E2B")
    output$sales_by_time_of_day <- renderPlotly({
      data <- filtered_data__new_business()  # Ensure 'sales_data' is already loaded and contains the right columns
      # Filter out rows where Sales or `Time of Day` might be NA
      data <- data[!is.na(data$Sales) & !is.na(data$`Time of Day`), ]
      # Group data by '`Time of Day`' and sum 'Sales'
      sales_by_time_of_day <- aggregate(Sales ~ `Time of Day`, data, sum)
      # Calculate percentages
      sales_by_time_of_day$percentage <- sales_by_time_of_day$Sales / sum(sales_by_time_of_day$Sales) * 100
      # Create the donut chart
      p <- plot_ly(sales_by_time_of_day, labels = ~`Time of Day`, values = ~Sales, type = 'pie', hole = 0.4,
                  textposition = 'inside', 
                  textinfo = 'label+value+percent',
                  insidetextorientation = 'tangential',
                  marker = list(colors = custom_colors2),
                  textfont = list(color = 'white', family = "Mulish", size = 12))
      # Add title and display the plot
      p <- p %>% layout(title = list(text = "Sum of Sales by Time of Day", x = 0.5),  # Center title
                        showlegend = TRUE,
                        font = list(family = "Mulish"))
      p
    })

    custom_colors_time_of_day <- c("#17a2b8", "#008b8b", "#48d1cc", "#F28E2B",  "#d62728","#1f77b4", "#F28E2B")
    output$salescount_by_time_of_day <- renderPlotly({
      data <- filtered_data__new_business()  # Load the necessary sales data
      # Filter out rows where Sales or `Time of Day` might be NA
      data <- data[!is.na(data$Sales) & !is.na(data$`Time of Day`), ]
      # Group data by 'Time of Day' and count the number of transactions
      sales_by_time_of_day <- data %>%
        group_by(`Time of Day`) %>%
        summarise(TransactionCount = n(), .groups = 'drop')
      # Create the donut chart
      p <- plot_ly(sales_by_time_of_day, labels = ~`Time of Day`, values = ~TransactionCount, type = 'pie', hole = 0.4,
                  textposition = 'inside', 
                  textinfo = 'label+value+percent',  
                  insidetextorientation = 'tangential', 
                  marker = list(colors = custom_colors_time_of_day),
                  textfont = list(color = 'white', family = "Mulish", size = 12))
      # Add title and display the plot
      p <- p %>% layout(title = "Distribution of Sales by Time of Day" ,showlegend = TRUE,
                        font = list(family = "Mulish"))
      p
    })

    output$sales_by_source <- renderPlotly({
    data <- filtered_data__new_business() %>%
      group_by(Source) %>%
      summarize(TotalSales = sum(Sales, na.rm = TRUE)) %>%
      mutate(TotalSalesMillions = TotalSales / 1e6) %>%
      mutate(Source = fct_reorder(Source, TotalSalesMillions)) %>%  # Reorder factor levels
      arrange((TotalSalesMillions))
    
    plot_ly(data, x = ~TotalSalesMillions, y = ~Source, type = 'bar',
            marker = list(color = '#17a2b8')) %>%
      layout(
        title = "Sales by Source",
        yaxis = list(title = "Source", tickangle = 0, tickfont = list(size = 10, color = "#333333")),
        xaxis = list(title = "Total Sales (Million KES)", tickfont = list(size = 10, color = "#333333")),
        font = list(family = "Mulish", color = "#333333"),
        margin = list(b = 100), # To accommodate rotated x-axis labels
        plot_bgcolor = "white",
        paper_bgcolor = "white"
      ) %>%
      add_annotations(
        x = ~TotalSalesMillions,
        y = ~Source,
        text = ~sprintf("%.2f M", TotalSalesMillions),
        showarrow = FALSE,
        xshift = 15,
        font = list(size = 9, color = "#333333")
      )
  })

    output$sales_count_by_source <- renderPlot({
    data <- filtered_data__new_business() %>%
      group_by(Source) %>%
      summarize(SalesCount = n(), TotalSales = sum(Sales)) %>%
      arrange(desc(SalesCount))
    ggplot(data, aes(x = reorder(Source, SalesCount), y = SalesCount, fill = "#0d6efd")) +
      geom_bar(stat = "identity") +  
      scale_fill_identity() +  
      geom_text(aes(label = SalesCount), vjust = 0.5, hjust = -0.15, color = "#333333", size=3.02) +
      coord_flip() +
      theme_minimal() +
      theme(legend.position = "none",
            legend.title = element_text(family = "Mulish", size = 10),
            text = element_text(family = "Mulish"),
            axis.title = element_text(family = "Mulish", size = 12),
            axis.text = element_text(family = "Mulish"),
            axis.text.y = element_text(family = "Mulish", size = 12),
            plot.title = element_text(family = "Mulish", size = 14),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      labs(title = "Sales Count by Source",
           y = "Count of Sales", x = "Source")
  })

  })
}
