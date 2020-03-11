library(shiny)
ds1.c<-readRDS('./private_data/shiny_data.rds')

n.dates<-dim(ds1.c)[2]
ds1.c<-ds1.c[,-n.dates,,] #remove last time point
counties<-dimnames(ds1.c)[[3]]
ages<-dimnames(ds1.c)[[1]]
age.labels<-c('<5y', '5-17y', '18-39y', '40-64y', '65+y', 'Age unknown')
syndromes<- dimnames(ds1.c)[[4]]

server<-function(input, output){
  output$countyPlot = renderPlot({
    par(mfrow=c(2,3))  
    for(k in 1:length(ages)){
      plot(ds1.c[ages[k],,input$set.county,input$set.syndrome]/ds1.c[ages[k],,input$set.county,'all.visits'], type='l', bty='l', main=paste(input$set.syndrome,input$set.county,age.labels[k]), ylab='Proportion of visits')
    }
  },
  width = "auto", height = "auto")
}
ui<-fluidPage(
  selectInput("set.county", "County:",
              choice=counties, selected ="Fairfield" ),
  selectInput("set.syndrome", "Syndrome:",
              choice=syndromes, selected ="ili2" ),
  plotOutput("countyPlot")
)

shinyApp(ui, server)
