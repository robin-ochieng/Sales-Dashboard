# UI for sales dashboard including graphs
MedicalsalesDashboardUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      valueBoxOutput(ns("total_sales_health")),
      valueBoxOutput(ns("count_new_bus_health")),
      valueBoxOutput(ns("count_renewal_bus_health"))
    ),
    fluidRow( 
      box(title = "Sales by Day of the Week", status = "white", solidHeader = TRUE, 
          plotlyOutput(ns("sales_by_day_health")) %>% withSpinner(type = 5)),
      box(title = "Sales Over Time", status = "white", solidHeader = TRUE,
          plotlyOutput(ns("sales_over_time_health"))  %>% withSpinner(type = 5)),
      tabBox( 
          solidHeader = TRUE,
          selected = "Count by Salesperson",
          status = "white",
          type = "tabs",
          id = ns("salesperson_tabs"),
          tabPanel("Sales by Salesperson", plotlyOutput(ns("sales_by_salesperson_health")) %>% withSpinner(type = 5)),
          tabPanel("Count by Salesperson", plotlyOutput(ns("count_by_salesperson_health")) %>% withSpinner(type = 5))
        ),
      tabBox( 
          solidHeader = TRUE,
          selected = "Count by Business Type",
          status = "white",
          type = "tabs",
          id = ns("salesperson_tabs"),
          tabPanel("Sales by Business Type", plotlyOutput(ns("sales_by_business_category_health")) %>% withSpinner(type = 5)),
          tabPanel("Count by Business Type", plotlyOutput(ns("count_by_business_category_health")) %>% withSpinner(type = 5))
        ),
      tabBox( 
          solidHeader = TRUE,
          selected = "Count by Location",
          status = "white",
          type = "tabs",
          id = ns("location_tabs"),
          tabPanel("Sales by Location", plotlyOutput(ns("sales_by_location_health")) %>% withSpinner(type = 5)),
          tabPanel("Count by Location", plotlyOutput(ns("count_by_location_health")) %>% withSpinner(type = 5))
        ),
      tabBox( 
          solidHeader = TRUE,
          selected = "Sales by Insurer",
          status = "white",
          type = "tabs",
          id = ns("insurer_tabs"),
          tabPanel("Count by Insurer", plotlyOutput(ns("count_by_insurer_health")) %>% withSpinner(type = 5)),
          tabPanel("Sales by Insurer", plotlyOutput(ns("sales_by_Insurer_health")) %>% withSpinner(type = 5))
        ),
      box(title = "Sales by Source of Business", status = "white", solidHeader = TRUE,
          plotlyOutput(ns("sales_by_source_of_business_health"))  %>% withSpinner(type = 5)),
      box(title = "Distribution of Sales by Source", status = "white", solidHeader = TRUE,
          plotlyOutput(ns("count_by_source_of_business_health"))  %>% withSpinner(type = 5))         
    )
  )
}



# Server logic for Medical Sales dashboard
MedicalsalesDashboardServer <- function(id, filtered_data__health_business) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns  # Namespace function to handle IDs

    output$total_sales_health <- renderValueBox({
    valueBox(
      format(sum(filtered_data__health_business()$Sales, na.rm = T), big.mark = ","),
      "Total Sales (KES)",
      icon = icon("usd"),
      color = "white"
    )
  })
  
  output$count_new_bus_health <- renderValueBox({
    count_new_bus <- filtered_data__health_business() %>%
      filter(`Busines Category` == "New Business") %>%
      summarize(CountSalesNew = n(), .groups = 'drop')  # Ensure the result is always a data frame
    # Handle cases where there might be no new business sales
    count_to_display <- if(nrow(count_new_bus) == 0) 0 else count_new_bus$CountSalesNew
    valueBox(
      format(count_to_display, big.mark = ","),
      "Count of New Business Sales",
      icon = icon("shopping-cart"),
      color = "white"
    )
  })
  
  output$count_renewal_bus_health <- renderValueBox({
    count_renewal_bus <- filtered_data__health_business() %>%
      filter(`Busines Category` == "Renewal") %>%
      summarize(CountSalesReNewal = n(), .groups = 'drop')  # Ensure the result is always a data frame
    # Handle cases where there might be no new business sales
    count_to_display <- if(nrow(count_renewal_bus) == 0) 0 else count_renewal_bus$CountSalesReNewal
    valueBox(
      format(count_to_display, big.mark = ","),
      "Count of Renewal Business Sales",
      icon = icon("shopping-cart"),
      color = "white"
    )
  })
  
  output$sales_by_day_health <- renderPlotly({
    data <- filtered_data__health_business() %>%
      mutate(DayOfWeek = wday(Date, label = TRUE)) %>%
      group_by(DayOfWeek) %>%
      summarize(TotalSales = sum(Sales, na.rm = TRUE)) %>%
      mutate(TotalSalesMillions = TotalSales / 1e6) %>%
      arrange(DayOfWeek)
    plot_ly(data, x = ~DayOfWeek, y = ~TotalSales, type = 'bar',
            text = ~paste(scales::comma(TotalSalesMillions, accuracy = 0.01), "M"),
            textposition = 'outside',
            hoverinfo = 'text',
            textfont = list(size = 9, color = "black"),
            marker = list(color = '#17a2b8')) %>%
      layout(
        title = "",
        xaxis = list(title = "Day of the Week", tickfont = list(size = 10, color = "#333333")),
        yaxis = list(title = "Total Sales", tickfont = list(size = 10, color = "#333333")),
        font = list(family = "Mulish", color = "#333333"),
        plot_bgcolor = "white",
        paper_bgcolor = "white"
      ) 
  })
  
  output$sales_over_time_health <- renderPlotly({
    data <- filtered_data__health_business()%>%
      mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% # Convert Date to Date format, adjust format as necessary
      arrange(Date)
    
    plot_ly(data, x = ~Date, y = ~Sales, type = 'scatter', mode = 'lines+markers',
            line = list(color = '#17a2b8'), marker = list(color = '#0d6efd')) %>%
      layout(
        title = "",
        xaxis = list(title = "Date", tickfont = list(size = 10, color = "#333333")),
        yaxis = list(title = "Sales", tickfont = list(size = 10, color = "#333333")),
        font = list(family = "Mulish", color = "#333333"),
        plot_bgcolor = "white",
        paper_bgcolor = "white"
      )
  })
  
  custom_colors_bus_category_health <- c( "#00acc1","#80deea", "#ff7f0e", "#1f77b4", "#9467bd", "#d62728")
  output$sales_by_business_category_health <- renderPlotly({
    sales_by_business_category <- filtered_data__health_business() %>%
      # Filter out rows where Sales or Business Category might be NA
      filter(!is.na(Sales), !is.na(`Busines Category`)) %>%
      # Group data by 'Busines Category' and sum 'Sales'
      group_by(`Busines Category`) %>%
      summarise(Sales = sum(Sales, na.rm = TRUE)) %>%
      # Calculate percentages
      mutate(percentage = Sales / sum(Sales) * 100)
    # Generate color palette
    num_categories <- length(unique(sales_by_business_category$`Busines Category`))
    colors <- custom_colors_bus_category_health 
    # Create the donut chart
    p <- plot_ly(sales_by_business_category, labels = ~`Busines Category`, values = ~Sales, type = 'pie', hole = 0.6,
                 textposition = 'outside',
                 textinfo = 'label+value+percent',
                 insidetextorientation = 'tangential',
                 marker = list(colors = colors),
                 textfont = list(color = 'black', family = "Mulish", size = 12))
    # Add title and display the plot
    p <- p %>% layout(showlegend = TRUE,
                      font = list(family = "Mulish"))
    p
  })

custom_colors5 <- c("#48d1cc", "#00838f", "#ff7f0e", "#1f77b4", "#9467bd", "#d62728")
output$count_by_business_category_health <- renderPlotly({
  count_by_business_category <- filtered_data__health_business() %>%
    # Filter out rows where Business Category might be NA
    filter(!is.na(Sales), !is.na(`Busines Category`)) %>%
    # Group data by 'Business Category' and count occurrences
    group_by(`Busines Category`) %>%
    summarise(Count = n(), .groups = 'drop') %>%
    # Calculate percentages
    mutate(percentage = Count / sum(Count) * 100)
  # Generate color palette
  num_categories <- length(unique(count_by_business_category$`Busines Category`))
  colors <- custom_colors5 
  # Create the donut chart
  p <- plot_ly(count_by_business_category, labels = ~`Busines Category`, values = ~Count, type = 'pie', hole = 0.6,
               textposition = 'outside',
               textinfo = 'label+value+percent',
               insidetextorientation = 'tangential',
               marker = list(colors = colors),
               textfont = list(color = 'black', family = "Mulish", size = 12))
  # Add title and display the plot
  p <- p %>% layout(showlegend = TRUE,
                    font = list(family = "Mulish"))
  p
})




output$sales_by_salesperson_health <- renderPlotly({
    # Process the data
    data <- filtered_data__health_business() %>%
      filter(!is.na(Salesperson)) %>%
      group_by(`Salesperson`) %>%
      summarize(TotalSales = sum(Sales, na.rm = TRUE)) %>%
      mutate(TotalSalesMillions = TotalSales / 1e6) %>%
      arrange(desc(TotalSalesMillions))
    # Factor with levels in descending order to show the highest sales at the top
    data$Salesperson <- factor(data$Salesperson, levels = rev(data$Salesperson))
    maxTotalSalesMillions <- max(data$TotalSalesMillions)
    # Creating the Plotly plot
    plot <- plot_ly(
      data,
      x = ~TotalSalesMillions,
      y = ~Salesperson,
      type = 'bar',
      orientation = 'h',
      marker = list(color = '#17a2b8')  # Set the bar color to Bootstrap's info color
    ) %>%
      layout(
        xaxis = list(title = 'Total Sales (Millions KES)', tickformat = ',.1f'),
        yaxis = list(title = 'Salesperson', title_standoff = 20),  # Add title_standoff to create space between title and ticks
        showlegend = FALSE,
        margin = list(t = 10, r = 20, b = 10, l = 10),
        annotations = lapply(1:nrow(data), function(i) {
          list(
            x = data$TotalSalesMillions[i] + (maxTotalSalesMillions * 0.02),
            y = data$Salesperson[i],
            text = paste(comma(data$TotalSalesMillions[i], accuracy = 0.01), "M"),
            showarrow = FALSE,
            xanchor = 'left',
            yanchor = 'middle',
            font = list(color = 'black', size = 9)
          )
        }),
        font = list(family = "Mulish", size = 12)
      )
    
    plot
  })
    
  custom_colors6 <- c("#17a2b8", "#48d1cc", "#ff7f0e","#2ca02c", "#1f77b4", "#9467bd", "#d62728")
  output$sales_by_location_health <- renderPlotly({
    sales_by_location <- filtered_data__health_business() %>%
      # Filter out rows where Sales or Business Category might be NA
      filter(!is.na(Sales), !is.na(Business_Location_Type)) %>%
      # Group data by 'Busines Category' and sum 'Sales'
      group_by(Business_Location_Type) %>%
      summarise(Sales = sum(Sales, na.rm = TRUE)) %>%
      # Calculate percentages
      mutate(percentage = Sales / sum(Sales) * 100)
    # Generate color palette
    num_categories <- length(unique(sales_by_location$Business_Location_Type))
    colors <- custom_colors6 
    # Create the donut chart
    p <- plot_ly(sales_by_location, labels = ~Business_Location_Type, values = ~Sales, type = 'pie', hole = 0.6, 
                 textposition = 'outside',
                 textinfo = 'label+value+percent',
                 insidetextorientation = 'tangential',
                 marker = list(colors = colors),
                 textfont = list(color = 'black', family = "Mulish", size = 12))
    # Add title and display the plot
    p <- p %>% layout(showlegend = TRUE,
                      font = list(family = "Mulish"))
    p
  })


custom_colors_location_health <- c("#17a2b8", "#80deea", "#ff7f0e", "#2ca02c", "#1f77b4", "#9467bd", "#d62728")
output$count_by_location_health <- renderPlotly({
  sales_by_location <- filtered_data__health_business() %>%
    # Filter out rows where Business Location Type might be NA
    filter(!is.na(Business_Location_Type)) %>%
    # Group data by 'Business Location Type' and count occurrences
    group_by(Business_Location_Type) %>%
    summarise(Count = n()) %>%
    # Calculate percentages
    mutate(percentage = Count / sum(Count) * 100)
  # Generate color palette
  num_categories <- length(unique(sales_by_location$Business_Location_Type))
  colors <- custom_colors_location_health
  # Create the donut chart
  p <- plot_ly(sales_by_location, labels = ~Business_Location_Type, values = ~Count, type = 'pie', hole = 0.6, 
               textposition = 'outside',
               textinfo = 'label+value+percent',
               insidetextorientation = 'tangential',
               marker = list(colors = colors),
               textfont = list(color = 'black', family = "Mulish", size = 12))
  # Add title and display the plot
  p <- p %>% layout(showlegend = TRUE,
                    font = list(family = "Mulish"))
  p
})

  
  output$sales_by_Insurer_health <- renderPlotly({
    # Process the data
    data <- filtered_data__health_business() %>%
      group_by(`Insurers Proposed`) %>%
      summarize(TotalSales = sum(Sales, na.rm = TRUE)) %>%
      mutate(TotalSalesMillions = TotalSales / 1e6) %>%
      arrange(desc(TotalSalesMillions))
    # Creating the Plotly plot
    plot <- plot_ly(
      data,
      x = ~reorder(`Insurers Proposed`, -TotalSalesMillions),
      y = ~TotalSalesMillions,
      type = 'bar',
      marker = list(color = '#17a2b8', line = list(color = '#333333', width = 0.2))  # Set the bar color to Bootstrap's warning color and add a black border
    ) %>%
      layout(
        xaxis = list(title = 'Insurer', tickangle = -45),
        yaxis = list(title = 'Total Sales (Millions KES)', tickformat = ',.0f'),
        showlegend = FALSE,
        margin = list(t = 20, r = 20, b = 80, l = 20),
        annotations = lapply(1:nrow(data), function(i) {
          list(
            x = i - 1,
            y = data$TotalSalesMillions[i],
            text = sprintf("%.1f M", data$TotalSalesMillions[i]),
            showarrow = FALSE,
            xanchor = 'center',
            yanchor = 'bottom',
            font = list(color = 'black', size = 9)
          )
        }),
        font = list(family = "Mulish"),
        plot_bgcolor = '#f8f9fa',  # Match panel background
        paper_bgcolor = '#f8f9fa'  # Match panel background
      )
    
    plot
  })
  
  output$count_by_insurer_health <- renderPlotly({
    # Process the data
    data <- filtered_data__health_business() %>%
      group_by(`Insurers Proposed`) %>%
      summarize(CountSales = n()) %>%
      arrange(desc(CountSales))
    # Calculate the maximum value of CountSales
    maxCountSales <- max(data$CountSales)
    # Factor with levels in descending order to show the highest counts at the top
    data$`Insurers Proposed` <- factor(data$`Insurers Proposed`, levels = rev(data$`Insurers Proposed`))
    # Creating the Plotly lollipop plot
    plot <- plot_ly(
      data,
      x = ~CountSales,
      y = ~`Insurers Proposed`,
      type = 'scatter',
      mode = 'lines+markers',
      line = list(color = '#17a2b8', width = 2),  
      marker = list(color = '#17a2b8', size = 8, line = list(color = '#17a2b8', width = 1)), 
      text = ~CountSales,
      textposition = 'right',
      hoverinfo = 'text'
    ) %>%
      layout(
        xaxis = list(title = 'Number of Sales Transactions'),
        yaxis = list(title = '`Insurers Proposed`', title_standoff = 20),  
        showlegend = FALSE,
        margin = list(t = 10, r = 20, b = 10, l = 10),
        annotations = lapply(1:nrow(data), function(i) {
          list(
            x = data$CountSales[i] + (maxCountSales * 0.05),
            y = data$`Insurers Proposed`[i],
            text = data$CountSales[i],
            showarrow = FALSE,
            xanchor = 'left',
            yanchor = 'middle',
            font = list(color = 'black', size = 9)
          )
        }),
        font = list(family = "Mulish", size = 12),
        plot_bgcolor = '#f8f9fa',  
        paper_bgcolor = '#f8f9fa' 
      )
    
    plot
  })
  
  output$count_by_salesperson_health <- renderPlotly({
    # Process the data
    data <- filtered_data__health_business() %>%
      filter(!is.na(Salesperson)) %>%
      group_by(`Salesperson`) %>%
      summarize(CountSales = n()) %>%
      arrange(desc(CountSales))
    # Calculate the maximum value of CountSales
    maxCountSales <- max(data$CountSales)
    # Factor with levels in descending order to show the highest counts at the top
    data$Salesperson <- factor(data$Salesperson, levels = rev(data$Salesperson))
    # Creating the Plotly lollipop plot
    plot <- plot_ly(
      data,
      x = ~CountSales,
      y = ~Salesperson,
      type = 'scatter',
      mode = 'lines+markers',
      line = list(color = '#17a2b8', width = 2),  # Line color set to Bootstrap's "info" color
      marker = list(color = '#17a2b8', size = 8, line = list(color = '#17a2b8', width = 1)),  # Marker color set to Bootstrap's "info" color
      text = ~CountSales,
      textposition = 'right',
      hoverinfo = 'text'
    ) %>%
      layout(
        xaxis = list(title = 'Number of Sales Transactions'),
        yaxis = list(title = 'Salesperson', title_standoff = 20),  # Add title_standoff to create space between title and ticks
        showlegend = FALSE,
        margin = list(t = 10, r = 20, b = 10, l = 10),
        annotations = lapply(1:nrow(data), function(i) {
          list(
            x = data$CountSales[i] + (maxCountSales * 0.05),
            y = data$Salesperson[i],
            text = data$CountSales[i],
            showarrow = FALSE,
            xanchor = 'left',
            yanchor = 'middle',
            font = list(color = 'black', size = 9)
          )
        }),
        font = list(family = "Mulish", size = 12),
        plot_bgcolor = '#f8f9fa',  # Match panel background
        paper_bgcolor = '#f8f9fa'  # Match panel background
      )
    
    plot
  })
  
output$sales_by_source_of_business_health <- renderPlotly({
  sales_by_source <- filtered_data__health_business() %>%
    filter(!is.na(Sales), !is.na(Source)) %>%
    group_by(Source) %>%
    summarise(Sales = sum(Sales, na.rm = TRUE))
  # Define a custom blue color palette
  blue_palette <- c("#006064", "#80deea", "#17a2b8", "#008b8b", "#b2ebf2")
  # Create the sunburst chart
  p <- plot_ly(data = sales_by_source, labels = ~Source, parents = "", values = ~Sales,
               type = 'sunburst',
               branchvalues = 'total',
               marker = list(colors = blue_palette), 
               textinfo = "label+value+percent parent",
               insidetextorientation = 'radial')
  # Add title and configure layout
  p <- p %>% layout(margin = list(t = 0, l = 0, r = 0, b = 0),
                    font = list(family = "Mulish"))
  p
})


  output$count_by_source_of_business_health <- renderPlotly({
    # Process the data
    data <- filtered_data__health_business() %>%
      filter(!is.na(Sales), !is.na(Source)) %>%
      group_by(Source) %>%
      summarise(TotalCounts = n()) %>%
      arrange(desc(TotalCounts))
    # Factor with levels in descending order to show the highest counts at the top
    data$Source <- factor(data$Source, levels = data$Source)
    # Creating the Plotly plot
    plot <- plot_ly(
      data,
      x = ~TotalCounts,
      y = ~Source,
      type = 'bar',
      orientation = 'h',
      marker = list(color = '#007bff')  # Set the bar color to Bootstrap's primary color
    ) %>%
      layout(
        xaxis = list(title = 'Count', tickformat = ','),
        yaxis = list(title = 'Source of Sales', title_standoff = 20),
        showlegend = FALSE,
        margin = list(t = 20, r = 25, b = 10, l = 25),
        annotations = lapply(1:nrow(data), function(i) {
          list(
            x = data$TotalCounts[i] + 0.05,
            y = data$Source[i],
            text = comma(data$TotalCounts[i]),
            showarrow = FALSE,
            xanchor = 'left',
            yanchor = 'middle',
            font = list(color = 'black', size = 12)
          )
        }),
        font = list(family = "Mulish", size = 12)
      ) %>%
      layout(
        margin = list(t = 20, r = 25, b = 10, l = 25),  # Adds border around the plot
        xaxis = list(title = "Count"),
        yaxis = list(title = "Source of Sales")
      )

    plot
  })


  })
}
