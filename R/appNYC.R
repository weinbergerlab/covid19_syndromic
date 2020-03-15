library(shiny)
all.glm.res<-readRDS('./nyc_shiny_data/glm.results.rds')
counties.to.test<-c("Bronx","Brooklyn", "Manhattan","Queens","Staten Island", "Citywide" )
syndromes<-c('ili','resp')
dates<-as.Date(names(all.glm.res[[1]][[1]][[1]]$resid1))
n.times<-length(dates)


server<-function(input, output){
  output$countyPlot = renderPlot({
    ili2.resid<- sapply(all.glm.res[[input$set.syndrome]], function(x) sapply(x,'[[','resid1'), simplify='array')
    dimnames(ili2.resid)[[2]]<-counties.to.test
    ili2.pred<- sapply(all.glm.res[[input$set.syndrome]], function(x) sapply(x,'[[','pred'), simplify='array')
    dimnames(ili2.pred)[[2]]<-counties.to.test
    ili2.pred.lcl<- sapply(all.glm.res[[input$set.syndrome]], function(x) sapply(x,'[[','lpi'), simplify='array')
    dimnames(ili2.pred.lcl)[[2]]<-counties.to.test
    ili2.pred.ucl<- sapply(all.glm.res[[input$set.syndrome]], function(x) sapply(x,'[[','upi'), simplify='array')
    dimnames(ili2.pred.ucl)[[2]]<-counties.to.test
    obs.ili<- sapply(all.glm.res[[input$set.syndrome]], function(x) sapply(x,'[[','y'), simplify='array')
    dimnames(obs.ili)[[2]]<-counties.to.test
    #dates<-as.Date(dimnames(ili.a)[[1]])
    age.labels = c("Ages 0-4 years", "Ages 5-17 years", "Ages 18-64 years", "Ages 65+ years", "All age groups")

    plot.min<-which(input$display.dates==dates)
    dates.select<-plot.min:n.times
    par(mfrow=c(2,3), mar=c(3,2,1,1))
    for(i in c('1','2','3','4','5')){
      for( j in input$set.borough){
        plot(dates[dates.select],obs.ili[dates.select,j,i], type='l', bty='l', ylab='Fitted', main=paste(j, age.labels[as.numeric(i)]), ylim=c(0,max(c(ili2.pred.lcl[dates.select,j,i],ili2.pred.ucl[dates.select,j,i],ili2.pred[dates.select,j,i],obs.ili[dates.select,j,i]), na.rm=T)))
        points(dates[dates.select],ili2.pred[dates.select,j,i], type='l', col='red', lty=3 )
        points(dates[dates.select],ili2.pred.lcl[dates.select,j,i], type='l', col='red', lty=3 )
        points(dates[dates.select],ili2.pred.ucl[dates.select,j,i], type='l', col='red', lty=3 )
        abline(v=790)
      }
    }
  }
    ,
  width = "auto", height = "auto")
}
ui<-fluidPage(
  titlePanel('NYC ED syndromic surveillance'),
  selectInput("set.borough", "Borough:",
              choice=counties.to.test, selected ="Citywide" ),
  selectInput("set.syndrome", "Syndrome:",
              choice=syndromes, selected ="ili" ),
  sliderInput('display.dates', 'Earliest date to display', min=min(dates), max=max(dates)-30, value=max(dates)-365),
  plotOutput("countyPlot"),
  column(8, align = 'justify',
         hr(),
         span("The black line shows the observed number of ED visits per day in the indicated stratum, and the red lines denote the mean and 95% prediction intervals for a model adjusting for seasonality, influenza activity, and RSV activity"),
         hr(),
         span("These plots summarize the NYC syndromic surveillance data, which were downloaded from the Epiquery website of the NYC Department of Health and Mental Hygiene. The models and plots were done by Dr. Dan Weinberger from Yale School of Public Health. Underlying analysis code can be found at https://github.com/weinbergerlab/covid19_syndromic"),
         hr(),
         span("CAVEATS: Syndromic data can be hard to interpret. Any increases above baseline could be due to viral activity or due to changes in healthcare seeking behavior (people might be more likely to go to the ED now with less severe symptoms because they are aware of the COVID-19 epidemic). This shows the daily count of ED visits and is not adjusted for overall ED volume (as is typically done, and mainly because a denominator is not readily available from the web interface. ")

)
)

shinyApp(ui, server)
