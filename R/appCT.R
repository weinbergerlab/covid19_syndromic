library(shiny)
all.glm.res<-readRDS('./private_data/shiny_data.rds')
counties.to.test<-c("Fairfield","Hartford", "New Haven","Litchfield","New London", "Middlesex", "Windham", "Tolland" )
syndromes<-c('ili2','respiratory', 'cough', 'fever')
dates<-as.Date(names(all.glm.res[[1]][[1]][[1]][['y']]))
n.times<-length(dates)
last.date.format<-max(dates)
last.date.format<-format(last.date.format,
                         "%b %d, %Y")

server<-function(input, output){
  output$countyPlot = renderPlot({
  if(input$set.pw=='ctcovid19'){
    ili2.resid<- sapply(all.glm.res[[input$set.syndrome]], function(x) sapply(x,'[[','resid1'), simplify='array')
    dimnames(ili2.resid)[[2]]<-counties.to.test
    ili2.pred<- sapply(all.glm.res[[input$set.syndrome]], function(x) sapply(x,'[[','pred'), simplify='array')
    dimnames(ili2.pred)[[2]]<-counties.to.test
    ili2.pred.lcl<- sapply(all.glm.res[[input$set.syndrome]], function(x) sapply(x,'[[','lpi'), simplify='array')
    dimnames(ili2.pred.lcl)[[2]]<-counties.to.test
    ili2.pred.ucl<- sapply(all.glm.res[[input$set.syndrome]], function(x) sapply(x,'[[','upi'), simplify='array')
    dimnames(ili2.pred.ucl)[[2]]<-counties.to.test
    obs.ili<- sapply(all.glm.res[[input$set.syndrome]], function(x) sapply(x,'[[','y'), simplify='array')
    all.cause<- sapply(all.glm.res[[input$set.syndrome]], function(x) sapply(x,'[[','all.cause'), simplify='array')
    dimnames(all.cause)[[2]]<-counties.to.test
    
    dimnames(obs.ili)[[2]]<-counties.to.test
    #dates<-as.Date(dimnames(ili.a)[[1]])
  
    plot.prop<- obs.ili/all.cause
    dimnames(plot.prop)[[2]]<-counties.to.test

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
          pred<-ili2.pred[dates.select,j,i]/all.cause[dates.select,j,i]
          pred.lcl<-ili2.pred.lcl[dates.select,j,i]/all.cause[dates.select,j,i]
          pred.ucl<-ili2.pred.ucl[dates.select,j,i]/all.cause[dates.select,j,i]
          if(input$set.axis==F){
            y.range<-c(0,max(y,na.rm=T))
            y.range[is.na(y.range)]<-10
          }else{
            y.range<-c(0, max(plot.prop[dates.select,j,], na.rm=T))
            y.range[is.na(y.range)]<-10
            }
        }else{
          y=obs.ili[dates.select,j,i]/ili2.pred[dates.select,j,i]
          pred<-obs.ili[dates.select,j,i]/ili2.pred[dates.select,j,i]
          pred.lcl<-obs.ili[dates.select,j,i]/ili2.pred.lcl[dates.select,j,i]
          pred.ucl<-obs.ili[dates.select,j,i]/ili2.pred.ucl[dates.select,j,i]
          if(input$set.axis==F){
            y.range<-range(y,na.rm=T)
            y.range[is.na(y.range)]<-10
            }else{
            y.range<-c(0.2, 4)
          }  
        }
         plot(dates[dates.select],y, type='n', bty='l', ylab='Fitted', main=paste(j, age.labels[as.numeric(i)]), ylim=y.range)
        polygon(c(dates[dates.select],rev(dates[dates.select])), 
                c(pred.lcl, rev(pred.ucl)), col=rgb(1,0,0,alpha=0.1), border=NA)
        lines(dates[dates.select],pred, type='l', col='red', lty=1, lwd=1.5 )
        lines(dates[dates.select],y, lwd=1.5)
        if(input$set.prop=='Observed/Expected'){
          abline(h=1, col='gray', lty=2)
        }
      }
    }
  }else{
    print('Enter Correct Password')
  }
  }
  ,
  width = "auto", height = "auto")
}
ui<-
  fluidPage(
    titlePanel(paste0('CT ED syndromic surveillance through ', last.date.format)),
    sidebarLayout(
      
      sidebarPanel(
        passwordInput('set.pw', 'Enter Password:'),
        selectInput("set.prop", "Proportion of ED visits or count:",
                    choice=c('Proportion','Count','Observed/Expected'), selected ="Proportion" ),
        selectInput("set.borough", "County:",
                    choice=counties.to.test, selected ="Fairfield" ),
        selectInput("set.syndrome", "Syndrome:",
                    choice=syndromes, selected ="ili2" ),
        checkboxInput("set.axis", "Uniform axis for all plots?:",
                      value =F ),
        sliderInput('display.dates', 'Earliest date to display', min=min(dates), max=max(dates)-30, value=max(dates)-90),
      ),
      mainPanel(
        plotOutput("countyPlot"),
        column(width=10, align = 'justify',
               hr(),
               span("The black line shows the observed number of ED visits per day in the indicated stratum, and the red lines denote the mean and 95% prediction intervals for a model adjusting for seasonality, influenza activity, and RSV activity"),
               hr(),
               span("These plots summarize the CT syndromic surveillance data crom the Connecticut Department of Public Health. The models and plots were done by Dr. Dan Weinberger from the Public Health Modeling Unit and Department of Epidemiology of Microbial Diseases at Yale School of Public Health, with assistance from Alyssa Amick, Kelsie Cassell, Marcus Rossi, Ernest Asare, Yu-Han Kao. Underlying analysis code can be found at https://github.com/weinbergerlab/covid19_syndromic"),
               
        )
      )
    )
  )

shinyApp(ui, server)
