library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(scales)
library(bs4Dash)
library(bslib)
library(plotly)
library(readxl)
library(lubridate)
library(treemapify)
library(tidyr)
library(colorspace)
library(RColorBrewer)
library(forcats)
library(shinycssloaders)
library(shinyjs)

# Load New Business Data
sales_data <- read_excel("./data/Sales Data New Business.xlsx", 
                         sheet = "2024 Data", col_types = c("date","text", "text", "text", "text", "numeric", "text", "text", "text", "text", "numeric", "text", "text"))

# Load Renewals Data
sales_data_renewals <- read_excel("./data/Sales Data Renewal Business.xlsx", 
                                  sheet = "2024 Data", col_types = c("text", "text", "text", "text", "text", "date", "text", "numeric", "numeric", "text", "text"))

# Load Health Business Data
sales_data_health <- read_excel("./data/Sales Data Health.xlsx", 
                                sheet = "2024 Data", col_types = c("date",  "text", "text", "text", "numeric", "text", "text", "text", "text", "text", "numeric", "text"))

# Source New Business module scripts
source("modules/NewDataTableModule.R", local = TRUE)
source("modules/NewSalesDashboardModule.R", local = TRUE)

# Source Renewal Business module scripts
source("modules/RenewalDataTableModule.R", local = TRUE)
source("modules/RenewalSalesDashboardModule.R", local = TRUE)

# Source Renewal Business module scripts
source("modules/MedicalDataTableModule.R", local = TRUE)
source("modules/MedicalSalesDashboardModule.R", local = TRUE)

# Define a custom theme using bslib
my_theme <- bs_theme(
  bg = "#202123", 
  fg = "#E1E1E1", 
  primary = "#EA80FC", 
  secondary = "#00BFA5",
  base_font = font_google("Mulish"),
  heading_font = font_google("Mulish"),
  code_font = font_google("Mulish"),
  navbar_bg = "#333333", 
  navbar_fg = "#ffffff"  
)

ui <- dashboardPage(
  title = "Sales Dashboard",
  dark = NULL,
  help = NULL,
  fullscreen = FALSE,
  scrollToTop = TRUE,
  freshTheme = my_theme,
  dashboardHeader(
    title = dashboardBrand(
      title = HTML("<div class='header-left'><strong style='font-weight: bold;'>SALES DASHBOARD</strong></div>"),
      color = "white",
      href = "https://vehicle.co.ke/"
    ),
    sidebarIcon = NULL,
    controlbarIcon = NULL,
    fixed = TRUE,
    tags$div(class = "control-bar", actionButton("toggleControlbar", "Input Controls", class = "btn btn-primary control-button"))
  ),
  sidebar = dashboardSidebar(
    skin = "light",
    sidebarMenu(
      br(),
      br(),
      menuItem("Business Line 1", tabName = "dashboard", icon = icon("plus-square")),
      menuItem("Business Line 2", tabName = "dashboard_renewals", icon = icon("sync")),
      menuItem("Business Line 3", tabName = "dashboard_medical", icon = icon("hospital-symbol"))#,
      #menuItem("Data", icon = icon("database"),
        #menuSubItem("Business Line 1", tabName = "data_table_new_business"),
        #menuSubItem("Business Line 2", tabName = "data_table_renewal_business"),
        #menuSubItem("Business Line 3", tabName = "data_table_medical_business")
      #)
    ),
    div(class = "sidebar-footer",
        img(src = "images/kenbright2.png")
    )
  ),
  controlbar = bs4DashControlbar(
    skin = "lightblue",
    title = "Filter Settings", 
    id = "dashboardControlbar",
    width = 300,
    bs4Card(
      width = 12,
      solidHeader = TRUE,
      status = "primary",
      title = HTML(paste(icon("plus-square"), " Business Line 1 Filters")),
      background = "white",
      class = "bs4-card-custom",
      selectInput(inputId = "new_business_month", label = "Select Month", choices = NULL),
      selectInput(inputId = "new_business_quarter", label = "Select Quarter", choices = NULL),
      selectInput(inputId = "new_business_year", label = "Select Year", choices = NULL)
    ),
    bs4Card(
      title = HTML(paste(icon("sync"), " Business Line 2 Filters")),
      width = 12,
      solidHeader = TRUE,
      status = "primary",
      background = "white",
      class = "bs4-card-custom",
      selectInput(inputId = "renewal_business_month", label = "Select Month", choices = NULL),
      selectInput(inputId = "renewal_business_quarter", label = "Select Quarter", choices = NULL),
      selectInput(inputId = "renewal_business_year", label = "Select Year", choices = NULL)
    ),
    bs4Card(
      title = HTML(paste(icon("hospital-symbol"), " Business Line 3 Filters")),
      width = 12,
      solidHeader = TRUE,
      status = "primary",
      background = "white",
      class = "bs4-card-custom",
      selectInput(inputId = "health_business_month", label = "Select Month", choices = NULL),
      selectInput(inputId = "health_business_quarter", label = "Select Quarter", choices = NULL),
      selectInput(inputId = "health_business_year", label = "Select Year", choices = NULL)
    )
  ),
  dashboardBody(
    tags$head(
      includeCSS("www/css/custom_styles.css"),
      tags$link(href = "https://fonts.googleapis.com/css2?family=Mulish:wght@400;700&display=swap", rel = "stylesheet"),
      tags$link(rel = "shortcut icon", href = "favicon/kenbright2.ico", type = "image/x-icon")
      ),
    tabItems(
      tabItem(tabName = "dashboard", salesDashboardUI("sales_dashboard")),
      tabItem(tabName = "dashboard_renewals", RenewalsalesDashboardUI("sales_dashboard_renewal")),
      tabItem(tabName = "dashboard_medical", MedicalsalesDashboardUI("sales_dashboard_medical")),
      tabItem(tabName = "data_table_new_business", dataTableUI("data_table")),
      tabItem(tabName = "data_table_renewal_business", RenewaldataTableUI("data_table_renewal")),
      tabItem(tabName = "data_table_medical_business", MedicaldataTableUI("data_table_medical"))  
  )
  ),
  footer = bs4DashFooter(
    div(style = "background-color: #fff; color: black; text-align: center; padding: 8px;", 
        "© 2024 Sales Dashboard | Powered by Robin")
  )
)

# Define server logic
server <- function(input, output, session) {
  
  observeEvent(input$toggleControlbar, {
    updateBoxSidebar("dashboardControlbar")
  })
  
#1. New Business -----------------------------------------------------------------------------------
  observe({
    # Ensure Month is two digits for date parsing
    sales_data <- sales_data %>%
      mutate(
        Month = as.character(Month),
        Month = trimws(Month),
        Year = as.numeric(as.character(Year)),
        Quarter = as.character(Quarter))
    
    month_choices <- sales_data$Month[!is.na(sales_data$Month)] %>% unique()
    month_choices <- c("All" = "All", month_choices)
    quarter_choices <- sales_data$Quarter[!is.na(sales_data$Quarter)] %>% unique()
    quarter_choices <- c("All" = "All", quarter_choices)
    year_choices <- sales_data$Year[!is.na(sales_data$Year)] %>% unique()

    updateSelectInput(session, "new_business_month", choices = month_choices, selected = "All")
    updateSelectInput(session, "new_business_quarter", choices = quarter_choices, selected = "All")
    updateSelectInput(session, "new_business_year", choices = year_choices, selected = format(Sys.Date(), "%Y"))
  })
  
  
  # Reactive expression to filter the data based on selected month, quarter, and year
  filtered_data__new_business <- reactive({
    req(sales_data)  
    if (input$new_business_month == "All" && input$new_business_quarter == "All") {
      sales_data %>%
        filter(Year == as.numeric(input$new_business_year))
    } else if (input$new_business_quarter == "All") {
      sales_data %>%
        filter(Month == input$new_business_month, Year == as.numeric(input$new_business_year))
    } else if (input$new_business_month == "All") {
      sales_data %>%
        filter(Quarter == as.character(input$new_business_quarter), Year == as.numeric(input$new_business_year))
    } else {
      sales_data %>%
        filter(Month == input$new_business_month, Quarter == as.character(input$new_business_quarter), Year == as.numeric(input$new_business_year))
    }
  })



#New Business Server Modules
  salesDashboardServer("sales_dashboard", filtered_data__new_business, reactive({ input$new_business_month }))
  dataTableServer("data_table", filtered_data__new_business)

#2. Renewal Business -----------------------------------------------------------------------------------
observe({
    sales_data_renewals <- sales_data_renewals %>%
      mutate(Month = as.character(Month),  
             Month = trimws(Month),
             Year = as.numeric(as.character(Year)),
             Quarter = as.character(Quarter))
    
    # Filter out NA values if present
    month_choices <- sales_data_renewals$Month[!is.na(sales_data_renewals$Month)] %>% unique()
    month_choices <- c("All" = "All", month_choices)
    quarter_choices <- sales_data_renewals$Quarter[!is.na(sales_data_renewals$Quarter)] %>% unique()
    quarter_choices <- c("All" = "All", quarter_choices)
    year_choices <- sales_data_renewals$Year[!is.na(sales_data_renewals$Year)] %>% unique()
    
    # Update the selectInput for months and years
    updateSelectInput(session, "renewal_business_month", choices = month_choices, selected = "All")
    updateSelectInput(session, "renewal_business_quarter", choices = quarter_choices, selected = "All")
    updateSelectInput(session, "renewal_business_year", choices = year_choices, selected = format(Sys.Date(), "%Y"))
  })
  
    # Reactive expression to filter the data based on selected month, quarter, and year
  filtered_data__renewal_business <- reactive({
    req(sales_data_renewals)  
    if (input$renewal_business_month == "All" && input$renewal_business_quarter == "All") {
      sales_data_renewals %>%
        filter(Year == as.numeric(input$renewal_business_year))
    } else if (input$renewal_business_quarter == "All") {
      sales_data_renewals %>%
        filter(Month == input$renewal_business_month, Year == as.numeric(input$renewal_business_year))
    } else if (input$renewal_business_month == "All") {
      sales_data_renewals %>%
        filter(Quarter == as.character(input$renewal_business_quarter), Year == as.numeric(input$renewal_business_year))
    } else {
      sales_data_renewals %>%
        filter(Month == input$renewal_business_month, Quarter == as.character(input$renewal_business_quarter), Year == as.numeric(input$renewal_business_year))
    }
  })

#Renewal Business Server Modules
  RenewalsalesDashboardServer("sales_dashboard_renewal", filtered_data__renewal_business)
  RenewaldataTableServer("data_table_renewal", filtered_data__renewal_business)



#3. Medical Business -----------------------------------------------------------------------------------
  observe({
    sales_data_health <- sales_data_health %>%
      mutate(Month = as.character(Month),  
             Month = trimws(Month),
             Year = as.numeric(as.character(Year)),
             Quarter = as.character(Quarter))
    
    # Filter out NA values if present
    month_choices <- sales_data_health$Month[!is.na(sales_data_health$Month)] %>% unique()
    month_choices <- c("All" = "All", month_choices)
    quarter_choices <- sales_data_health$Quarter[!is.na(sales_data_health$Quarter)] %>% unique()
    quarter_choices <- c("All" = "All", quarter_choices)
    year_choices <- sales_data_health$Year[!is.na(sales_data_health$Year)] %>% unique()
    
    # Update the selectInput for months and years
    updateSelectInput(session, "health_business_month", choices = month_choices, selected = "All")
    updateSelectInput(session, "health_business_quarter", choices = quarter_choices, selected = "All")                  
    updateSelectInput(session, "health_business_year", choices = year_choices, selected = format(Sys.Date(), "%Y"))
  })
  

    # Reactive expression to filter the data based on selected month, quarter, and year
  filtered_data__health_business <- reactive({
    req(sales_data_health)  
    if (input$health_business_month == "All" && input$health_business_quarter == "All") {
      sales_data_health %>%
        filter(Year == as.numeric(input$health_business_year))
    } else if (input$health_business_quarter == "All") {
      sales_data_health %>%
        filter(Month == input$health_business_month, Year == as.numeric(input$health_business_year))
    } else if (input$health_business_month == "All") {
      sales_data_health %>%
        filter(Quarter == as.character(input$health_business_quarter), Year == as.numeric(input$health_business_year))
    } else {
      sales_data_health %>%
        filter(Month == input$health_business_month, Quarter == as.character(input$health_business_quarter), Year == as.numeric(input$health_business_year))
    }
  })

#Medical Business Server Modules
  MedicalsalesDashboardServer("sales_dashboard_medical", filtered_data__health_business)
  MedicaldataTableServer("data_table_medical", filtered_data__health_business)
}

# Run the application
shinyApp(ui = ui, server = server)