library(shiny); library(DT)

###########################
#Functions

###########################


# Define UI for FETA
fluidPage(
  
  # Application title
  titlePanel("Power Analysis"),
  
  sidebarLayout(
    
    sidebarPanel(
      sliderInput("alpha", "Probability of false positive (alpha):",  
                  min = 0, max = 0.2, value = 0.05,step=0.01),
      sliderInput("sampling", "Sampling SD (log scale):",  
                  min = 0.01, max = 1.5, value = 0.5,step=0.01),
      sliderInput("process", "Process SD (log scale):",  
                  min = 0.01, max = 1.5, value = 0.1,step=0.01),
      sliderInput("years", "Years:",  
                  min = 3, max = 15, value = 5,step=1),
      #run new scenario
      actionButton("run","Run Parameters"),
      #download graph
      downloadButton('downloadPlot',
                     'Download Plot')
    ),
    
    mainPanel(
      plotOutput("PowerPlot")
      
    )
  )
)#end Fluidpage