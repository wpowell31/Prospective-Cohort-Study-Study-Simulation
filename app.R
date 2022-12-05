library(shiny)
#runApp("/Users/willpowell/Desktop/Biostat721/Project_simulation")

source('721metricfunctions.R')
source('721simfunctions.R')

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Will Powell Simulation Study"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the changes in risks ----
      
      sliderInput(inputId = 'num_nonexposure',
                  label = 'Number of Subjects in Nonexposure Group',
                  min = 10,
                  max = 1000,
                  value = 500),
      
      sliderInput(inputId = 'num_exposure',
                  label = 'Number of Subjects in Exposure Group',
                  min = 10,
                  max = 1000,
                  value = 500),
      
      sliderInput(inputId = 'fexposure',
                  label = 'risk of False Classification to Exposure Group',
                  min = 0,
                  max = 1,
                  value = .5),
      
      sliderInput(inputId = 'fnonexposure',
                  label = 'risk of False Classification to Nonexposure Group',
                  min = 0,
                  max = 1,
                  value = .5),
      
      sliderInput(inputId = 'exposure_risk',
                  label = 'True Risk in Exposure Group',
                  min = 0,
                  max = 1,
                  value = .5),
      
      sliderInput(inputId = 'nonexposure_risk',
                  label = 'True Risk in Nonexposure Group',
                  min = 0,
                  max = 1,
                  value = 0.5)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      imageOutput("dukeLogo"),
      textOutput("TrueRR"),
      textOutput("TrueRD"),
      textOutput("TrueOR"),
      plotOutput(outputId = "RRboxplot"),
      plotOutput(outputId = "RDboxplot"),
      plotOutput(outputId = "ORboxplot")
      
      
    )
  )
)




# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  output$TrueRR <- renderText({
    paste("True Relative Risk: ", RR(input$num_exposure[1], input$exposure_risk[1],
                                     input$num_nonexposure[1], input$nonexposure_risk[1]))
  })
  
  output$TrueRD <- renderText({
    paste("True Risk Difference: ", RD(input$num_exposure[1], input$exposure_risk[1],
                                       input$num_nonexposure[1], input$nonexposure_risk[1]))
  })
  
  output$TrueOR <- renderText({
    paste("True Odds Risk: ", OR(input$num_exposure[1], input$exposure_risk[1],
                                 input$num_nonexposure[1], input$nonexposure_risk[1]))
  })
  
  output$RRboxplot <- renderPlot({
    hist(simulationRR(input$num_exposure[1], input$exposure_risk[1], input$num_nonexposure[1], input$nonexposure_risk[1],
                      input$fexposure[1], input$fnonexposure[1], 50),
         main = 'Histogram of Simulated Relative Risk',
         xlab = 'RR')
    abline(v=RR(input$num_exposure[1], input$exposure_risk[1],
                input$num_nonexposure[1], input$nonexposure_risk[1]), col='blue', lwd=2)
  })
  
  output$RDboxplot <- renderPlot({
    hist(simulationRD(input$num_exposure[1], input$exposure_risk[1], input$num_nonexposure[1], input$nonexposure_risk[1],
                      input$fexposure[1], input$fnonexposure[1], 50),
         main = 'Histogram of Simulated Risk Difference',
         xlab = 'RD')
    abline(v=RD(input$num_exposure[1], input$exposure_risk[1],
                input$num_nonexposure[1], input$nonexposure_risk[1]), col='green', lwd=2)
  })
  
  output$ORboxplot <- renderPlot({
    hist(simulationOR(input$num_exposure[1], input$exposure_risk[1], input$num_nonexposure[1], input$nonexposure_risk[1],
                      input$fexposure[1], input$fnonexposure[1], 50),
         main = 'Histogram of Simulated Odds Ratios',
         xlab = 'OR')
    abline(v=OR(input$num_exposure[1], input$exposure_risk[1],
                input$num_nonexposure[1], input$nonexposure_risk[1]), col='red', lwd=2)
  })
  
  output$dukeLogo <- renderImage({
    filename <- "/Users/willpowell/Desktop/Biostat721/biostat_masters_logoblue_0.jpg"
    
    list(src = filename,
         alt = 'Duke Image')
  }, deleteFile = FALSE)
  
}



shinyApp(ui = ui, server = server)
