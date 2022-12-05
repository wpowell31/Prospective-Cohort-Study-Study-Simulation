library(shiny)
#runApp("/Users/willpowell/Desktop/Biostat721/Project_simulation")


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
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generisks a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  
  
  #output$nonexposure_risk <- 3
  
  RR <- function(num_exposure, exposure_risk, num_nonexposure, nonexposure_risk){
    num_exposure_success <- as.integer(num_exposure * exposure_risk)
    num_nonexposure_success <- as.integer(num_nonexposure * nonexposure_risk)
    r_t <- num_exposure_success/num_exposure
    r_c <- num_nonexposure_success/num_nonexposure
    return(r_t/r_c)
  }
  
  RD <- function(num_exposure, exposure_risk, num_nonexposure, nonexposure_risk){
    num_exposure_success <- as.integer(num_exposure * exposure_risk)
    num_nonexposure_success <- as.integer(num_nonexposure * nonexposure_risk)
    r_t <- num_exposure_success/num_exposure
    r_c <- num_nonexposure_success/num_nonexposure
    return(r_t - r_c)
  }
  
  OR <- function(num_exposure, exposure_risk, num_nonexposure, nonexposure_risk){
    num_exposure_success <- as.integer(num_exposure * exposure_risk)
    num_nonexposure_success <- as.integer(num_nonexposure * nonexposure_risk)
    o_t <- num_exposure_success/(num_exposure - num_exposure_success) 
    o_c <- num_nonexposure_success/(num_nonexposure - num_nonexposure_success)
    return(o_t/o_c)
  }
  
  simulationRR <- function(num_exposure, exposure_risk, num_nonexposure, nonexposure_risk, fexposure, fnonexposure, N){
    cohort_size <- num_exposure + num_nonexposure
    prob_exposure <- num_exposure/(num_exposure + num_nonexposure)
    rel_risk <- rep(0, N)
    
    for(j in 1:N){
      
      exposure_true <- rbinom(cohort_size, 1, prob_exposure)
      outcome_true <- rep(0,cohort_size)
      exposure_observed <- rep(0,cohort_size)
      indicator <- rep(1,cohort_size)
      sim_data <- data.frame(exposure_true, outcome_true, exposure_observed, indicator)
      
      for(i in 1:cohort_size){
        if(exposure_true[i] == 1){
          sim_data[i,2] <- rbinom(1, 1, exposure_risk)
          sim_data[i,3] <- rbinom(1, 1, 1-fnonexposure)
        }else{
          sim_data[i,2] <- rbinom(1, 1, nonexposure_risk)
          sim_data[i,3] <- rbinom(1, 1, fexposure)
        }
      }
      r_t <- sum(sim_data[which(sim_data[,2] == 1 & sim_data[,3] == 1),3])/sum(sim_data[,3])
      r_c <- sum(sim_data[which(sim_data[,2] == 1 & sim_data[,3] == 0),2])/sum(sim_data[which(sim_data[,3] == 0),4])
      rel_risk[j] <- r_t/r_c
      
    }
    
    return(rel_risk)
  }
  
  
  simulationRD <- function(num_exposure, exposure_risk, num_nonexposure, nonexposure_risk, fexposure, fnonexposure, N){
    cohort_size <- num_exposure + num_nonexposure
    prob_exposure <- num_exposure/(num_exposure + num_nonexposure)
    rel_risk <- rep(0, N)
    
    for(j in 1:N){
      
      exposure_true <- rbinom(cohort_size, 1, prob_exposure)
      outcome_true <- rep(0,cohort_size)
      exposure_observed <- rep(0,cohort_size)
      indicator <- rep(1,cohort_size)
      sim_data <- data.frame(exposure_true, outcome_true, exposure_observed, indicator)
      
      for(i in 1:cohort_size){
        if(exposure_true[i] == 1){
          sim_data[i,2] <- rbinom(1, 1, exposure_risk)
          sim_data[i,3] <- rbinom(1, 1, 1-fnonexposure)
        }else{
          sim_data[i,2] <- rbinom(1, 1, nonexposure_risk)
          sim_data[i,3] <- rbinom(1, 1, fexposure)
        }
      }
      r_t <- sum(sim_data[which(sim_data[,2] == 1 & sim_data[,3] == 1),3])/sum(sim_data[,3])
      r_c <- sum(sim_data[which(sim_data[,2] == 1 & sim_data[,3] == 0),2])/sum(sim_data[which(sim_data[,3] == 0),4])
      rel_risk[j] <- r_t - r_c
      
    }
    
    return(rel_risk)
  }
  
  
  simulationOR <- function(num_exposure, exposure_risk, num_nonexposure, nonexposure_risk, fexposure, fnonexposure, N){
    cohort_size <- num_exposure + num_nonexposure
    prob_exposure <- num_exposure/(num_exposure + num_nonexposure)
    rel_risk <- rep(0, N)
    
    for(j in 1:N){
      
      exposure_true <- rbinom(cohort_size, 1, prob_exposure)
      print(j)
      outcome_true <- rep(0,cohort_size)
      exposure_observed <- rep(0,cohort_size)
      indicator <- rep(1,cohort_size)
      sim_data <- data.frame(exposure_true, outcome_true, exposure_observed, indicator)
      
      for(i in 1:cohort_size){
        if(exposure_true[i] == 1){
          sim_data[i,2] <- rbinom(1, 1, exposure_risk)
          sim_data[i,3] <- rbinom(1, 1, 1-fnonexposure)
        }else{
          sim_data[i,2] <- rbinom(1, 1, nonexposure_risk)
          sim_data[i,3] <- rbinom(1, 1, fexposure)
        }
      }
      o_t <- sum(sim_data[which(sim_data[,2] == 1 & sim_data[,3] == 1),3])/sum(sim_data[which(sim_data[,2] == 0 & sim_data[,3] == 1),3])
      o_c <- sum(sim_data[which(sim_data[,2] == 1 & sim_data[,3] == 0),2])/sum(sim_data[which(sim_data[,2] == 0 & sim_data[,3] == 0),4])
      rel_risk[j] <- o_t/o_c
      
    }
    
    return(rel_risk)
  }
  
  
  
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
                         input$fexposure[1], input$fnonexposure[1], 25),
            main = 'Histogram of Simulated Relative Risk',
         xlab = 'RR')
    abline(v=RR(input$num_exposure[1], input$exposure_risk[1],
                input$num_nonexposure[1], input$nonexposure_risk[1]), col='blue', lwd=2)
  })
  
  output$RDboxplot <- renderPlot({
    hist(simulationRD(input$num_exposure[1], input$exposure_risk[1], input$num_nonexposure[1], input$nonexposure_risk[1],
                         input$fexposure[1], input$fnonexposure[1], 25),
            main = 'Histogram of Simulated Risk Difference',
         xlab = 'RD')
    abline(v=RD(input$num_exposure[1], input$exposure_risk[1],
                input$num_nonexposure[1], input$nonexposure_risk[1]), col='green', lwd=2)
  })
  
  output$ORboxplot <- renderPlot({
    hist(simulationOR(input$num_exposure[1], input$exposure_risk[1], input$num_nonexposure[1], input$nonexposure_risk[1],
                         input$fexposure[1], input$fnonexposure[1], 25),
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
