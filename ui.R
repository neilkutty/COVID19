#
#

library(shiny)
library(leaflet)
library(readr)
# Define UI for application that draws a histogram
shinyUI(fluidPage(  
br(),

           fluidRow(plotOutput("globalts")),
           
           fluidRow(leafletOutput("mymap")),
           
           
          
           br(),
         
           # sliderInput("dateselect","Select Day:",value = c$Date[length(c$Date)],
           #             min = c$Date[1], max = c$Date[length(c$Date)],
           #             animate = animationOptions(interval = 1100), width = 1000),
           # 
           # 
           # fluidRow(leafletOutput("mapts")),



a("data source: JHU CSSE COVID-19 DATA",href="https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data"),
paste(' | '),
a("author: neil kutty", href="http:/twitter.com/neilkutty"),
paste(' | '),
paste('Data Updated as of: ',Sys.Date()-2)           
           
))