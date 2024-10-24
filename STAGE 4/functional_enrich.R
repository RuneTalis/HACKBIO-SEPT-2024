
#load the following packages:
library(BiocManager)
library(TCGAbiolinks)
library(dplyr)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(EDASeq)

#organisms_data frame
org_df <- data.frame(id = 1:7, 
                     scientific_name = c("Homo sapiens", "Anopheles gambiae","Mus musculus", "Arabidopsis thaliana", "Passer domesticus", "Panthera leo", "Giraffa camelopardalis"),
                     common_name = c("Human", "African malaria mosquito", "House mouse", "Thale cress", "Common sparrow", "African lion", "Northern giraffe"),
                     ncbi_tax_id = c("9606", "7165", "10090", "3702", "48849", "9689", "9894"),
                     stringsAsFactors = FALSE)

#UI Page
ui <- dashboardPage(
  dashboardHeader(title = "Functional Enrichment Analysis App"),
  
  dashboardSidebar(
    sidebarMenu(menuItem("Home", tabName = "home", icon = icon("home")),
                menuItem("Enrichment Analysis", tabName = "analysis", icon = icon("bar-chart")),
                menuItem("Contacts", tabName = "contact", icon("envelope"))
    )
  ),
  
  dashboardBody(
    tabItems(
      
      tabItem(
        tabName = "home",
        fluidPage(
          h1("Welcome to the Functional Enrichment Analyis Application"),
          p("This interactive app is designed to perform enrichment analysis on any gene lists and displays their functions in pathways, molecular processes, biological processes, cellular components."),
          p("Use the side bar to navigate through the app.")
        )
      )
    ),
    
    
    tabItem(tabName = "analysis",
            fluidRow(
              box(
                title = "Functional Enrichment Analysis App", status = "primary", solidHeader = TRUE, width = 4,
                selectInput("speciesSelect", "Select Annotated Species:", choices = org_df$scientific_name),
                textAreaInput(inputId = "gene_list", label = "Paste your genes here(comma-separated):", rows = 5),
                actionButton("analyze", "Run Enrichment Analysis")
              ),
              box(
                title = "Results", status = "primary", solidHeader = TRUE, width = 10,
                plotOutput("enrichment_plot")
              )
            )
    ),
    tabItem(tabName = "contacts",
            fluidPage(
              h2("Contact Us"),
              p("For any inquiries, reach out via:"),
              p("Email: pollanda@outlook.com"),
              p("Slack: @Pollanda,@HackBio Cancer Internship 2024")
            )
    )
  )
)

#Server 
server <- function(input, output) {
  
  
  
  #reactive gene list, convert the user input (comma-separated) into a list
  gene_l <- reactive({
    strsplit(input$gene_list, ",")[[1]]
  }) 
  
  #Section for enrichment analysis using TCGAanalyze EAcomplete()
  enrich_result <- reactive({
    TCGAanalyze_EAcomplete(
      TFname = "Example list of genes", 
      RegulonList = gene_l()
    )
  })
 
  #Section for making barplot with TCGAvisualize_EAbarplot
  output$enrichment_plot <- renderPlot(
    TCGAvisualize_EAbarplot(
      tf = "Example list of genes",
      GOMFTab = enrich_result()$ResMF, #this will generate molecular function results
      GOBPTab = enrich_result()$ResBP, #this will generate biological process results
      GOCCTab = enrich_result()$ResCC, #this will generate cellular component results
      PathTab = enrich_result()$ResPat,#this will generate pathway term results
      nBar = 10,
      nRGTab = NULL,
      filename = NULL,
      text.size = 1,
      mfrow = c(2,2), #will give 2 rows and 2 column
      xlim = NULL,
      color = c("orange","cyan", "green", "yellow" )
    )
  )
  
}  


shinyApp(ui = ui, server = server)

