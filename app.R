## app.R ##


library(shiny)
library(shinydashboard)
library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)
library(DT)
library(lubridate)


# set up data source
data <- read_xlsx('data/301 Aug Sep 2019.xlsx', col_names = TRUE)
data <- select(data, 'Scan Type', 'Scan Date','Scan Entered into CME', 'Latitude', 'Longitude', 'Run No.')
data$scan_type <- data$`Scan Type`
# create factor for use in plots
data$scan_type <- as.factor(data$scan_type)

# Separate out date component for day for later plotting
data$scan_day <- day(data$`Scan Date`)


ui <- dashboardPage(
        dashboardHeader(title = "Courier Performance"),
        # SideBar Content
        dashboardSidebar(
                sidebarMenu(
                        menuItem("KPIs", tabName = "KPIs", icon = icon("chart-line")),
                        menuItem("Time Analysis", tabName = "widgets", icon = icon("th"))
                )
        ),
        dashboardBody(
                tabItems(
                        # First Tab Content
                        tabItem(tabName = "KPIs",
                                h2("KPIs"),
                                # Boxes need to be put in a row or column.
                                fluidRow(
                                        valueBoxOutput("run_num_box")
                                ),
                                fluidRow(
                                         box(title = "Scans Summary for WCP301 for Aug Sep 2019",
                                             solidHeader = TRUE,
                                             status = "primary",
                                             width = 6,
                                             plotOutput("scanSummaryPlot", height = 250)),
                                         box(title = "Scan Type Filter",
                                             width = 6,
                                             uiOutput("scanTypeFilter")
                                         )
                                ),
                                fluidRow(
                                        # infoBoxOutput("scanTypeCount", width = 3)
                                        DT::dataTableOutput(outputId = "st_summary")                                        
                                ) 
                        ),
                        tabItem(tabName = "widgets",
                                h2("Performance over Time"),
                                fluidRow(
                                        plotOutput("scanDetailPlot", height = 250))
                                )
                        )
                )
        )        

server <- function(input, output) {
        # dynamically generate dropdown list plot filter for 'scan_type'
        output$scanTypeFilter <- renderUI({
                selectInput("scanTypeInput", "Scan Type", sort(unique(data$scan_type)),
                            selected = "Out for Delivery")
        })
        
        output$run_num_box <- renderValueBox({
                valueBox(
                        subtitle =  "Run Number",
                        as.character(unique(data$`Run No.`)),
                        icon = icon("list"),
                        color = "purple"
                )
        })
        
        # Apply the filter to plot data
        filtered <- reactive({
                if (is.null(input$scanTypeInput)){
                        return(NULL)
                }
                
                data %>% 
                        filter(scan_type == input$scanTypeInput)
                
        })
        
        # Calculate summary table of scan type values unique counts (e.g., 'Delivery: 1200', 'Attempted Delivery: 37' ...)
        st_count <- reactive({
                data %>% 
                        group_by(`Scan Type`) %>% 
                        summarize(count=n())                
        })

        # Display summary table data
        output$st_summary <- DT::renderDataTable(datatable(
                st_count(), filter = 'top', rownames = FALSE, options = list(sDom  = '<"top">lrt<"bottom">')
        ))


        # render plot to display
        output$scanSummaryPlot <- renderPlot({
                if (is.null(filtered())) {
                        return()
                }
                
                ggplot(filtered(), aes(scan_type)) +
                        theme_classic() +
                        # coord_flip() +
                        geom_bar(fill = "steelblue4", stat = "count") +
                        labs(x = "Scan Type")
        })
        
        
        counts <- data %>% group_by('Scan Date') %>% summarize(count=n())
        
        output$scanDetailPlot <- renderPlot({
                ggplot(data, aes(fill = 'Scan Type', x = 'Scan Date', y = counts)) +
                        theme_classic() +
                        # coord_flip() +
                        geom_bar(position = "dodge") +
                        labs(x = "Scan Date")
        })
}

shinyApp(ui, server)