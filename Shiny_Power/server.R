library(shiny); library(DT)

# Define server logic to run FETA and output figures and tables
function(input, output) {
  
  #set reactivity
  datasetInput<-reactive({
    rv$update<-rv$update+1
    alpha.set<<-input$alpha
    vc$SD.sampling<<-input$sampling
    vc$SD.process<<-input$process
    i.years<<-input$years
    
  })#end reactive

  
  #run analysis through datasetInput() reactive function
  observeEvent(input$run,{
      first<<-FALSE
      datasetInput()
      rv$update<-rv$update+1
  })#end event
  
  output$PowerPlot<-renderPlot({
    rv$update
    withProgress(message='Running...',value=0.5,{
      estimate.power(vc,i.years,alpha=alpha.set)
    })
  })
  
  #Download Plot
  output$downloadPlot <- downloadHandler(
    filename="output.png" , 
    content=function(file){
      ggsave(plot=power.plot, file=file,
             h=6, w=6, dpi=300)
    }
  )
  
  
}