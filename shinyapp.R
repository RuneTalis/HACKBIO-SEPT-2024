#remember to call on the packages library
install.packages("Rtools")
install.packages("shinythemes")
library(Rtools)
library(shiny)
library(shinythemes)
library(shinydashboard)
library(commonmark)
#code for user interface
shinyApp(
  ui <-fluidPage(
    theme = shinytheme("cerulean"), 
               navbarPage("Serial Dilution Calculator:",
                          
                          tabPanel("Home",
                                   #I should input values
                                   sidebarPanel(
                                     HTML("<h3>Input parameters</h3>"),
                                     sliderInput("Formulaweight",
                                                 label = "Formula weight (g/mol)",
                                                 min = 1,
                                                 max = 500,
                                                 value = 58.44
                                     ),
                                     sliderInput("Volume",
                                                 label = "Desired final volume (ml)",
                                                 min = 2,
                                                 max = 2000,
                                                 value = 1000),
                                   ),
                                   sliderInput("Concentration",
                                               label = "Desired oncentration (mol/L)",
                                               min = 0.1,
                                               max = 10,
                                               value = 0.5),
                          ),
                          actionButton("calculatebutton", 
                                       "Calculate",
                                       class = "btn btn-danger")
               ),
    mainPanel(
      tags$label(h3('Status/Output')),#is a textbox
      verbatimTextOutput('contents'),
      tableOutput('tabledata') #will display results in a table
    ) #the end of mainPanel() section
  ),#tabPanel(), Home
  
  tabPanel("About,",
           titlePanel("About"),
           div(includeMarkdown("sigmamolarityfile.md"), align="justify")
  ) #tabPanel(), About
)# for navbarPage())#fluidPage()
               
#making the server based on dataprofessor script
server <- function(input, output, session) {
  #Input Data
  datasetInput <- reactive({
    mass <- input$Concentration* input$Volume * input$Formulaweight
    mass <- data.frame(mass)
    names(mass) <- "MASS"
    return(mass)
    
  })
  
  #output box
  output$contents <-renderPrint({
    if (input$calculatebutton > 0) {
      isolate("Calculation complete.")
    } else {
        return("Server is ready for calculation.")
      }
  })
# Hopefully the prediction table results
output$tabledata <-renderTable({
  if (input$calculatebutton > 0) {
    isolate(datasetInput())
    }
  })
}

shinyApp(ui = ui, server = server)

