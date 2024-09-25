# Set working directory
setwd("/Users/polla/OneDrive/Desktop/FRONTENDBIO")

# Load necessary libraries
library(shiny)
library(shinythemes)
library(leaflet)
library(ggplot2)
library(dplyr)
library(DT)
library(readr)
library(tidyr)
library(rmarkdown)

# Load the cholera data
cholera_data <- read.csv("/Users/polla/OneDrive/Desktop/FRONTENDBIO/Choleracases_2000_2023.csv")

# Data frame of country coordinates
Country.name = c('Afghanistan', 'Australia', 'Bahrain', 'Bangladesh', 'Benin', 'Kenya', 'United States', 'India', 'Nigeria', 'Brazil')
Latitude = c(33.93911, -25.2744, 26.0667, 23.685, 9.3077, -1.286389, 37.0902, 20.5937, 9.082, -14.235)
Longitude = c(67.709953, 133.7751, 50.5577, 90.3563, 2.3158, 36.817223, -95.7129, 78.9629, 8.6753, -51.9253)
country_coords <- data.frame(Country.name, Latitude, Longitude)

# Checking column names
names(cholera_data)
names(country_coords)

# Renaming columns in cholera_data
cholera_data <- cholera_data %>%
  rename(Countries..territories.and.areas = Country) %>%
  rename(Number.of.reported.cases.of.cholera = Total_Cases) %>%
  rename(Number.of.reported.deaths.from.cholera = Deaths) %>%
  rename(Cholera.case.fatality.rate = CFR)

# Merging cholera data with the full country coordinates using the 'Country' column
cholera_data_with_full_coords <- merge(cholera_data, country_coords, by.x = "Countries..territories.and.areas", by.y = "Country.name", all.x = TRUE)

# Display the first few rows to ensure proper merging
head(cholera_data_with_full_coords)

# UI for the Shiny app
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel("Cholera Outbreak Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("year_range", "Select Year Range:",
                  min = min(cholera_data$Year), 
                  max = max(cholera_data$Year),
                  value = c(2000, 2023), step = 1, sep = ""),
      
      selectInput("country", "Select Country:", 
                  choices = c("All", unique(cholera_data$Countries..territories.and.areas)), 
                  selected = "Kenya"),
      
      actionButton("gen_report", "Generate Kenya Report")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Overview",
                 leafletOutput("cholera_map", height = 500),
                 h4("Cholera Cases Summary"),
                 DTOutput("cases_table")
        ),
        tabPanel("Trends",
                 plotOutput("cases_plot"),
                 plotOutput("fatalities_plot"),
                 plotOutput("fatality_rate_plot")
        ),
        tabPanel("Reports",
                 downloadButton("download_report", "Download Kenya Report")
        )
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  # Reactive data for filtering by year and country
  filtered_data <- reactive({
    data <- cholera_data %>%
      filter(Year >= input$year_range[1] & Year <= input$year_range[2])
    
    if (input$country != "All") {
      data <- data %>% filter(Countries..territories.and.areas == input$country)
    }
    return(data)
  })
  
  # Cholera map
  output$cholera_map <- renderLeaflet({
    data <- filtered_data()
    
    leaflet(data) %>%
      addTiles() %>%
      addCircles(lng = ~Longitude, lat = ~Latitude, weight = 1,
                 radius = ~sqrt(Number.of.reported.cases.of.cholera) * 500, popup = ~paste0(Countries..territories.and.areas, ": ", Number.of.reported.cases.of.cholera, " cases"))
  })
  
  # Cases summary table
  output$cases_table <- renderDT({
    data <- filtered_data()
    datatable(data %>%
                group_by(Countries..territories.and.areas, Year) %>%
                summarise(Total_Cases = sum(Number.of.reported.cases.of.cholera),
                          Total_Deaths = sum(Number.of.reported.deaths.from.cholera),
                          CFR = mean(Cholera.case.fatality.rate)))
  })
  
  # Cases plot
  output$cases_plot <- renderPlot({
    data <- filtered_data()
    ggplot(data, aes(x = Year, y = Number.of.reported.cases.of.cholera)) +
      geom_line() + 
      theme_minimal() + 
      labs(title = "Cholera Cases in Selected Country (2000-2023)", x = "Year", y = "Total Cases")
  })
  
  # Fatalities plot
  output$fatalities_plot <- renderPlot({
    data <- filtered_data()
    ggplot(data, aes(x = Year, y = Number.of.reported.deaths.from.cholera)) +
      geom_line() + 
      theme_minimal() + 
      labs(title = "Cholera Fatalities in Selected Country (2000-2023)", x = "Year", y = "Total Deaths")
  })
  
  # Fatality rate plot
  output$fatality_rate_plot <- renderPlot({
    data <- filtered_data()
    ggplot(data, aes(x = Year, y = Cholera.case.fatality.rate)) +
      geom_line() + 
      theme_minimal() + 
      labs(title = "Cholera Case Fatality Rate in Selected Country (2000-2023)", x = "Year", y = "CFR")
  })
  
  # Generate Kenya report
  output$download_report <- downloadHandler(
    filename = function() { paste("Kenya_Cholera_Report_", Sys.Date(), ".pdf", sep = "") },
    content = function(file) {
      rmarkdown::render("kenya_report_template.Rmd", output_file = file)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
