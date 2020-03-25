# Current Error: leaflet map not rendering in Shiny app, but rendering standalone
# -- FiXED : daily data column names had changed and bad refer to lat long columns


library(leaflet)
library(magrittr)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(countrycode)
library(scales)
library(ggthemes)
library(shiny)
library(leaflet)

# ----------- Get Timeseries Data -----------------------------------------------

conf = read.csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv')
dead = read.csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv')
reco = read.csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv')

##  label ts datasets
conf = cbind(label="confirmed",conf)
dead = cbind(label="dead",dead)
reco = cbind(label="recovered",reco)

#bind ts datasets
all_ts = rbind(conf,dead,reco)


#   -  Get Daily Data
csv_string = paste0('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/',
                    format(Sys.Date()-1,'%m-%d-%Y'),'.csv')
daily = read.csv(csv_string)
colnames(daily) = tolower(colnames(daily))

daily = daily %>%
    mutate(latitude = lat,
           longitude = long_) %>% 
    select(everything(),-lat,-long_) %>%
    filter(confirmed > 0)



# ! -- Fix location mis-mapping in app <-- Turned out to be bad refer in popup
#       == New problem with popup on TS !! = Cannot figure out the blank popup on dataset vars
#       .... Try to remove sliderInput and have filtered dataset shown to see if popups work.
c = conf %>%
    separate(Province.State,c('City','State_dirty'),',',remove = F,fill = 'right') %>%
    mutate(State = substr(State_dirty,1,3),
           latitude = Lat,
           longitude = Long) %>%
    #replace_na(State,'XX') %>%
    select(City, State, everything(),-State_dirty,-Lat,-Long) %>%
    gather(key='Date', value='Count',X1.22.20:colnames(conf)[ncol(conf)]) %>%
    mutate(Date = as.Date(mdy(gsub(gsub(x = Date,
                                pattern = 'X', 
                                replacement = ''),
                           pattern = '\\.',replacement = '-'))),
           State = State %>% replace_na('XX'))

#   -  Analyze Daily Data
#  -- Grouped  by label and Date --- --- --- --- --- -- -- -- -- -- -- >
df1 = all_ts %>%
    select(label, everything(),-Province.State,-Country.Region,-Lat,-Long) %>%
    group_by(label) %>%
    gather(key='Date', value='Count',X1.22.20:colnames(all_ts)[ncol(all_ts)]) %>%
    mutate(Date = as.Date(mdy(gsub(gsub(x = Date,
                                        pattern = 'X', 
                                        replacement = ''),
                                   pattern = '\\.',replacement = '-')))) %>%
    group_by(Date, label) %>%
    summarise(Total = sum(Count)) %>%
    spread(label, Total)  


# ..Add button to reset map using setView.  
# ..Work out scale for circle radius.
# ..Look into infinite horizontal scroll map option in leaflet. 
shinyServer(function(input, output) {

    clean_dat <- reactive({
        data.frame(c[c$Date==input$dateselect,])
        
    })
    
    output$mymap <- renderLeaflet({
        
        leaflet(daily) %>%
            addProviderTiles("OpenStreetMap.Mapnik",
                             options = providerTileOptions(noWrap = TRUE)
            ) %>%
            addCircleMarkers(
                radius = ~(log10(daily$confirmed)*7),
                opacity = 0.078,
                fillOpacity = 0.19,
                fillColor = ~'magenta',
                popup = paste0("<strong>City: </strong>",
                               daily$admin2,
                               "<br><strong>Province/State: </strong>",
                               daily$province_state,
                               "<br><strong>Country/Region: </strong>", 
                               daily$country_region, 
                               "<br><strong>Confirmed cases: </strong>", 
                               formatC(daily$confirmed,big.mark = ','),
                               "<br><strong>Deaths: </strong>",
                               formatC(daily$deaths,big.mark = ','),
                               "<br><strong>Recovered: </strong>",
                               formatC(daily$recovered,big.mark = ','))
            ) %>%
            setView(lat=23,lng=5,zoom = 1.6) %>%
            addResetMapButton()

    })
    
    output$globalts <- renderPlot({
        # ------------  ------  ---------- All TS Plot
        ggplot(df1,aes(x=Date)) +
            geom_line(aes(y=confirmed, col="Confirmed")) +
            geom_point(aes(y=confirmed, col="Confirmed")) +
            geom_label(data=df1[df1$Date==Sys.Date()-2,],
                       label=comma(df1[df1$Date==Sys.Date()-2,]$confirmed),
                       aes(y=df1[df1$Date==Sys.Date()-2,]$confirmed,
                           col="Confirmed"),show.legend = F)+
            geom_line(aes(y=dead, col="Dead")) +
            geom_point(aes(y=dead, col="Dead")) +
            geom_label(data=df1[df1$Date==Sys.Date()-2,],
                       label=comma(df1[df1$Date==Sys.Date()-2,]$dead),
                       aes(y=df1[df1$Date==Sys.Date()-2,]$dead,
                           col="Dead"),show.legend = F)+
            geom_line(aes(y=recovered, col="Recovered")) +
            geom_point(aes(y=recovered, col="Recovered")) +
            geom_label(data=df1[df1$Date==Sys.Date()-2,],
                       label=comma(df1[df1$Date==Sys.Date()-2,]$recovered),
                       aes(y=df1[df1$Date==Sys.Date()-2,]$recovered,
                           col="Recovered"),show.legend = F)+
            ggtitle("Corona Virus Cases Globally") +
            scale_x_date(breaks = unique(df1$Date)) +
            scale_y_continuous(label = comma) +
            theme(plot.title = element_text(size=25,face = "bold",hjust = 0.5),
                  axis.title = element_text(size=10,face = "bold"),
                  axis.title.y = element_blank(),
                  axis.text.x = element_text(size = 9,face = "bold", angle = 90, hjust = 1),
                  axis.text.y = element_text(size = 10, face = "bold"),
                  panel.background = element_rect(fill = "white"),
                  strip.background = element_rect(fill = "white"),
                  panel.grid.major = element_line(colour = "gray",size = .10),
                  legend.position = "bottom",
                  legend.background = element_rect(fill=alpha('white', 0.2)),
                  legend.title = element_blank())
        
    })
    
    # output$mapts <- renderLeaflet({
    #     c[c$Date==input$dateselect,] %>%
    #     leaflet() %>%
    #         addProviderTiles("OpenStreetMap.Mapnik",
    #                          options = providerTileOptions(noWrap = TRUE)
    #         ) %>%
    #         addCircleMarkers(
    #             radius = ~(log10(c[c$Date==input$dateselect,]$Count)*7),
    #             opacity = 0.078,
    #             fillOpacity = 0.19,
    #             fillColor = ~'magenta'
    #         ) %>%
    #         setView(lat=23,lng=5,zoom = 1.6)
    #     
    # })

})
