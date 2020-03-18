library(shiny)
all.glm.res<-readRDS('./nyc_shiny_data/glm.results.rds')
counties.to.test<-c("Bronx","Brooklyn", "Manhattan","Queens","Staten Island", "Citywide" )
syndromes<-c('ili','resp')
dates<-as.Date(names(all.glm.res[[1]][[1]][[1]]$resid1))
n.times<-length(dates)
last.date.format<-max(dates)
last.date.format<-format(last.date.format,
                         "%b %d, %Y")
server<-function(input, output){
  output$countyPlot = renderPlot({
    ili.prop<-sapply(all.glm.res[[input$set.syndrome]], function(x) sapply(x,'[[','ili.prop'), simplify='array')
    resp.prop<-sapply(all.glm.res[[input$set.syndrome]], function(x) sapply(x,'[[','resp.prop'), simplify='array')
    dimnames(ili.prop)[[2]]<-counties.to.test
    dimnames(resp.prop)[[2]]<-counties.to.test
    if(input$set.syndrome=='ili'){
      plot.prop<-ili.prop
    }else{
      plot.prop<-resp.prop
    }
      
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
        if(input$set.prop=='Count'){
          y=obs.ili[dates.select,j,i]
          pred<-ili2.pred[dates.select,j,i]
          pred.lcl<-ili2.pred.lcl[dates.select,j,i]
          pred.ucl<-ili2.pred.ucl[dates.select,j,i]
          if(input$set.axis==F){
          y.range<-c(0,max(c(ili2.pred.lcl[dates.select,j,i],ili2.pred.ucl[dates.select,j,i],ili2.pred[dates.select,j,i],obs.ili[dates.select,j,i]), na.rm=T))
          }else{
          y.range<-c(0,max(c(ili2.pred.lcl[dates.select,j,],ili2.pred.ucl[dates.select,j,],ili2.pred[dates.select,j,],obs.ili[dates.select,j,]), na.rm=T))
          }
        }else if (input$set.prop=='Proportion'){
          y=plot.prop[dates.select,j,i]
          pred<-rep(NA, length(y))
          pred.lcl<-rep(NA, length(y))
          pred.ucl<-rep(NA, length(y))
        if(input$set.axis==F){
            y.range<-c(0,max(y,na.rm=T))
          }else{
            y.range<-c(0, max(plot.prop[dates.select,j,], na.rm=T))
          }
        }else{
          y=obs.ili[dates.select,j,i]/ili2.pred[dates.select,j,i]
          pred<-obs.ili[dates.select,j,i]/ili2.pred[dates.select,j,i]
          pred.lcl<-obs.ili[dates.select,j,i]/ili2.pred.lcl[dates.select,j,i]
          pred.ucl<-obs.ili[dates.select,j,i]/ili2.pred.ucl[dates.select,j,i]
          if(input$set.axis==F){
            y.range<-range(y,na.rm=T)
          }else{
            y.range<-c(0.2, 4)
          }  
        }
        plot(dates[dates.select],y, type='l', bty='l', ylab='Fitted', main=paste(j, age.labels[as.numeric(i)]), ylim=y.range)
        points(dates[dates.select],pred, type='l', col='red', lty=3 )
        points(dates[dates.select],pred.lcl, type='l', col='red', lty=3 )
        points(dates[dates.select],pred.ucl, type='l', col='red', lty=3 )
        if(input$set.prop=='Observed/Expected'){
          abline(h=1, col='gray', lty=2)
        }
      }
    }
  }
    ,
  width = "auto", height = "auto")
}
ui<-fluidPage(
  titlePanel(paste0('NYC ED syndromic surveillance through ', last.date.format)),
  span("CAUTION: Syndromic surveillance data can be hard to interpret. Any increases above expected could be due to changes in healthcare seeking behavior (people might be more likely to go to the ED now with less severe symptoms because they are aware of the COVID-19 epidemic), or it could be due to actual viral illness, or a combination. For a deep dive of the data produced by NYC Department of Health and Mental Hygiene see https://www1.nyc.gov/assets/doh/downloads/pdf/hcp/weekly-surveillance03072020.pdf . This app shows the daily count of ED visits and is not adjusted for overall ED volume (as is typically done, and mainly because a denominator is not readily available from the web interface. "),
  selectInput("set.prop", "Proportion of ED visits or count:",
              choice=c('Proportion','Count','Observed/Expected'), selected ="Count" ),
  selectInput("set.borough", "Borough:",
              choice=counties.to.test, selected ="Citywide" ),
  selectInput("set.syndrome", "Syndrome:",
              choice=syndromes, selected ="ili" ),
  checkboxInput("set.axis", "Uniform axis for all plots?:",
               value =F ),
  sliderInput('display.dates', 'Earliest date to display', min=min(dates), max=max(dates)-30, value=max(dates)-365),
  plotOutput("countyPlot"),
  column(8, align = 'justify',
         hr(),
         span("The black line shows the observed number of ED visits per day in the indicated stratum, and the red lines denote the mean and 95% prediction intervals for a model adjusting for seasonality, influenza activity, and RSV activity"),
         hr(),
         span("These plots summarize the NYC syndromic surveillance data, which were downloaded from the Epiquery website of the NYC Department of Health and Mental Hygiene. The models and plots were done by Dr. Dan Weinberger from the Public Health Modeling Unit and Department of Epidemiology of Microbial Diseases at Yale School of Public Health, with assistance from Alyssa Amick, Kelsie Cassell, Marcus Rossi, Ernest Asare, Yu-Han Kao. Underlying analysis code can be found at https://github.com/weinbergerlab/covid19_syndromic"),

)
)

shinyApp(ui, server)
