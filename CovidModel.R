library(leaflet)
library(tidyverse)
library(ggmap)
library(leaflet.extras)
library(htmltools)
library(ggplot2)
library(maps)
library(mapproj)
library(mapdata)
library(ggthemes)
library(tmap)
library(tidycensus)
library(reshape2)
library(lubridate)

##Covid Current Cases by County
CovidT <- time_series_covid19_confirmed_US %>%
  select(Admin2, Province_State, Long_, Lat, X3.30.2020)

##Cleaning
CovidT <- CovidT %>%
  rename(Long = Long_)

#US Map for each county
mycolor <- colorNumeric(palette = 'RdYlBu',
                        domain = c(0:38000),
                        reverse = T)
CovidT %>%
  leaflet()%>%
  addProviderTiles('CartoDB') %>%
  addCircleMarkers(radius = ~0.01*X3.30.2020,
                   color = ~mycolor(X3.30.2020),
                   popup = ~paste0(Admin2, '<br/>', X3.30.2020))%>%
  addLegend(pal = mycolor,
            values = c(1:38000),
            opacity = 0.5,
            title = 'Confirmed Cases',
            position = 'topleft')
########################
##Line Chart by County##
########################

#focus data - FIPS = COunty
CountyTimeLine <- Covid3X30 %>% 
  filter(FIPS == '44001' | FIPS == '44003' | FIPS == '44009' | FIPS == '44005' | FIPS == '44007')
CountyTimeLine <- CountyTimeLine[, c(7:75, 2, 3, 6)]

#format for graphing
CountyTimeLine <- melt(CountyTimeLine, id =(c('County', 'State')))

#Cleaning
CountyTimeLine <- CountyTimeLine %>%
  rename(Date = variable,
         Cases = value)
         
CountyTimeLine$Date <-as.POSIXct(strptime(CountyTimeLine$Date, '%m/%d/%Y'))
CountyTimeLine$Cases <- as.numeric(CountyTimeLine$Cases)

#Smaller X Variable
CountyTimeLine <- CountyTimeLine %>% 
  slice(-seq(0.6 * n()))

#Line Graph
ggplot(CountyTimeLine, aes(x= Date, y = Cases, group = County, color = County))+
  geom_line(size=1)+
  xlab('Date')+
  ylab('Confirmed Cases')+
  ggtitle('Total Cases of Covid-19 in Rhode Island By County')+
  theme_economist()

#Get Daily totals
CountyPerDay <- CountyTimeLine %>%
  arrange(Date) %>%
  group_by(County) %>%
  mutate(diff = Cases - lag(Cases, default = first(Cases)))

#Per Day line Graph
ggplot(data = CountyPerDay, aes(x= Date, y = diff, group = County, color = County))+
  geom_line()+
  xlab('Date')+
  ylab('Confirmed Cases')+
  ggtitle('Confirmed Cases of Covid-19 in Rhode Island Per Day')+
  theme_economist()

################
##State Charts##
################

#focus State filter
StateData <- Covid3X30 %>%
  filter(State == 'Rhode Island')
StateData <- StateData[7:75]

#sum columns & Format
StateData <- StateData%>%
  colSums()
StateData <- as.data.frame(StateData, keep.rownames = 'id')
StateData <- rownames_to_column(StateData)

#Make R recognize dates
StateData$rowname <-as.POSIXct(strptime(StateData$rowname, '%m/%d/%Y'))

#rename Columns
StateData<- StateData%>%
  rename(Date = rowname,
         Cases = StateData)

#cut it in half-ish
StateData <- StateData %>% 
  slice(-seq(0.6 * n()))

#Line
ggplot(data = StateData, aes(x= Date, y = Cases))+
  geom_line(color = 'steelblue', size=2)+
  xlab('Date')+
  ylab('Confirmed Cases')+
  ggtitle('Total Cases of Covid-19 in Rhode Island')+
  theme_pander()

#Get Daily totals
StatePerDay <- StateData %>%
  arrange(Date) %>%
  mutate(diff = Cases - lag(Cases, default = first(Cases)))

#Per Day line Graph
ggplot(data = StatePerDay, aes(x= Date, y = diff))+
  geom_line(color = 'steelblue', size=2)+
  xlab('Date')+
  ylab('Confirmed Cases')+
  ggtitle('Confirmed Cases of Covid-19 per day in Rhode Island')+
  theme_economist()
