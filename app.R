## app.R ##


library(shiny)
library(shinydashboard)
library(readxl)
library(dplyr)
library(ggplot2)

ui <- dashboardPage(
        dashboardHeader(title = "Courier Performance"),
        # SideBar Content
        dashboardSidebar(
                sidebarMenu(
                        menuItem("KPIs", tabName = "KPIs", icon = icon("chart-line")),
                        menuItem("Widgets", tabName = "widgets", icon = icon("th"))
                )
        ),
        dashboardBody(
                tabItems(
                        # First Tab Content
                        tabItem(tabName = "KPIs",
                                h2("KPIs"),
                                # Boxes need to be put in a row or column.
                                fluidRow(
                                         box(title = "Scans Summary for WCP301 for Aug Sep 2019",
                                             solidHeader = TRUE,
                                             status = "primary",
                                             width = 12,
                                             plotOutput("scanSummary", height = 250))
                                        
                                ),
                                fluidRow(
                                        box(
                                                title = "Scan Type Filter",
                                                uiOutput("scanTypeFilter")
                                        )
                                ) 
                        ),
                        tabItem(tabName = "widgets",
                                h2("Widgets Tab Content")
                        )
                )
        )        
)

server <- function(input, output) {
        # set up data source
        data <- read_xlsx('data/301 Aug Sep 2019.xlsx', col_names = TRUE)
        data <- select(data, 'Scan Type', 'Scan Date','Scan Entered into CME', 'Latitude', 'Longitude', 'Run No.')
        data$scan_type <- data$`Scan Type`
        # create factor for use in plots
        data$scan_type <- as.factor(data$scan_type)
        
        # dynamically generate dropdown list plot filter for 'scan_type'
        output$scanTypeFilter <- renderUI({
                selectInput("scantype", "Scan Type", sort(unique(data$scan_type)))
        })
        
        output$scanSummary <- renderPlot({
                ggplot(data, aes(scan_type)) +
                        theme_classic() +
                        # coord_flip() +
                        geom_bar(fill = "#FFDB6D", stat = "count") +
                        labs(x = "Scan Type")
        })
}

shinyApp(ui, server)