library(tidyverse)
library(ggplot2)
library(plotly)
library(mapview)
library(DT)
library(ggthemes)
library(gganimate)
library(maps)
library(viridis)

data=read.csv("D:/CORONA/Visualization Dataset upto 28.03.2020/novel-corona-virus-2019-dataset (1)/covid_19_data.csv")

head(data)

data = data %>% rename(
  Date = ObservationDate,
  Country = Country.Region,
  State = Province.State,
  Last_Update = Last.Update
)

data$Date = as.Date(data$Date, format = "%m/%d/%y")

confirmed = read.csv("D:/CORONA/Visualization Dataset upto 28.03.2020/novel-corona-virus-2019-dataset (1)/time_series_covid_19_confirmed.csv")
head(confirmed[1:8])
confirmed = confirmed %>% rename(
  Country = 'Country.Region',
)

recovered = read.csv("D:/CORONA/Visualization Dataset upto 28.03.2020/novel-corona-virus-2019-dataset (1)/time_series_covid_19_recovered.csv")
head(recovered[1:8])
recovered = recovered %>% rename(
  Country = `Country.Region`,
)

deaths = read.csv("D:/CORONA/Visualization Dataset upto 28.03.2020/novel-corona-virus-2019-dataset (1)/time_series_covid_19_deaths.csv")
head(deaths[1:8])
deaths = deaths %>% rename(
  Country = `Country.Region`,
)

#COUNTRY-WISE NUMBER OF CASES CHECKING______________________________________

#China
country = filter(data,Country == "Mainland China") %>% group_by(Date) %>% 
  summarise(Confirmed =  sum(Confirmed) , Deaths = sum(Deaths), Recovered = sum(Recovered)) %>% 
  gather(key = Cases, value = Count, c(Confirmed,Deaths,Recovered))

ggplot(country, aes(x= Date, y= Count )) +
   geom_line(aes(color = Cases))+geom_point(aes(color = Cases)) +
   scale_color_manual(values = c("blue","black","green")) +
   labs(x="Timeline", y="Number of Cases") +
   ggtitle("COVID-19 in china (March 28)") 
  
 
#USA 
country = filter(data,Country == "US") %>% group_by(Date) %>% 
   summarise(Confirmed =  sum(Confirmed) , Deaths = sum(Deaths), Recovered = sum(Recovered)) %>% 
   gather(key = Cases, value = Count, c(Confirmed,Deaths,Recovered))
 
ggplot(country, aes(x= Date, y= Count)) +
   geom_line(aes(color = Cases))+geom_point(aes(color = Cases))+
   scale_color_manual(values = c("blue","black","green"))+
   labs(x="Timeline", y="Number of Cases") +
   ggtitle("COVID-19 in USA (March 28)")

#Italy
country = filter(data,Country == "Italy") %>% group_by(Date) %>% 
   summarise(Confirmed =  sum(Confirmed) , Deaths = sum(Deaths), Recovered = sum(Recovered)) %>% 
   gather(key = Cases, value = Count, c(Confirmed,Deaths,Recovered))
 
ggplot(country, aes(x= Date, y= Count)) +
   geom_line(aes(color = Cases))+geom_point(aes(color = Cases)) +
   scale_color_manual(values = c("blue","black","green")) +
   labs(x="Timeline", y="Number of Cases") +
   ggtitle("COVID-19 in Italy(March 28)")

#Spain
country = filter(data,Country == "Spain") %>% group_by(Date) %>% 
   summarise(Confirmed =  sum(Confirmed) , Deaths = sum(Deaths), Recovered = sum(Recovered)) %>% 
   gather(key = Cases, value = Count, c(Confirmed,Deaths,Recovered))
 
ggplot(country, aes(x= Date, y= Count))+
   geom_line(aes(color = Cases))+geom_point(aes(color = Cases))+
   scale_color_manual(values = c("blue","black","green"))+
   labs(x="Timeline", y="Number of Cases") +
   ggtitle("COVID-19 in Spain(March 28)")
   
#India
country = filter(data,Country == "India") %>% group_by(Date) %>% 
   summarise(Confirmed =  sum(Confirmed) , Deaths = sum(Deaths), Recovered = sum(Recovered)) %>% 
   gather(key = Cases, value = Count, c(Confirmed,Deaths,Recovered))
 
ggplot(country, aes(x= Date, y= Count)) +
   geom_line(aes(color = Cases))+geom_point(aes(color = Cases)) +
   scale_color_manual(values = c("blue","black","green")) +
   labs(x="Timeline", y="Number of Cases") +
   ggtitle("COVID-19 in India(March 28)")

#_________________________________________________________________________________________________________________
 
 
 Sorted = filter(data, data$Date==max(data$Date)) %>% group_by(Country) %>% summarise(Confirmed =  sum(Confirmed) , Deaths = sum(Deaths) , Recovered = sum(Recovered))
 Sorted = Sorted[order(Sorted$Confirmed,decreasing = TRUE),]
 
 # datatable(filtered)
 
 top20_cnf = Sorted[0:20,]
 top20_cnf = top20_cnf %>% mutate(rank = rank(-Confirmed),
                              Value_rel = Confirmed/Confirmed[rank==1],
                              Value_lbl = paste0(" ",Confirmed))
 
 ggplot(top20_cnf, aes(rank, group = Country, fill = as.factor(Country), color = as.factor(Country))) +
   geom_tile(aes(y = Confirmed/2, height = Confirmed, width = 0.6), alpha = 0.6, color = 'blue') +
   geom_text(aes(y = 0, label = paste(Country, " ")),size=6, vjust = 0, hjust = 1) +
   geom_text(aes(y=Confirmed,label = Value_lbl, hjust=0),size=6) +
   coord_flip(clip = "off", expand = FALSE) +
   scale_x_reverse() +
   guides(color = FALSE, fill = FALSE) +
   labs(title='Top 20 affected Contries(March 28)')+
 theme(axis.line=element_blank(),
       axis.text.x=element_blank(),
       axis.text.y=element_blank(),
       axis.ticks=element_blank(),
       axis.title.x=element_blank(),
       axis.title.y=element_blank(),
       plot.margin = margin(0.4,2.3,0.4,6, "cm"))
 
 #______________________________________________________________________________________________________
 
 #WORLD MAPS
 
 #CONFIRMED
 
 world = ggplot() +
   borders("world", color = "gray", fill = "gray80") +
   theme_map()
 
 
 Count = as.integer(unlist(confirmed[,ncol(confirmed)]))
 # class(Count)
 # typeof(Count)
 # mode(Count)
 # is.list(confirmed[,ncol(confirmed)])
 Conf_Map = world +
   geom_point(aes(x = Long, y = Lat, size = Count, name= Country),
              data = confirmed, 
              color = 'red', alpha = .5) +
   labs(size = 'Cases',title="World Map of COVID-19 Confirmed(March 28)")
 
 ggplotly(Conf_Map, tooltip = c('Count','Country'))
 
 #DEATHS
 
 Count = as.integer(unlist(deaths[,ncol(deaths)]))
 
 Conf_Map = world +
   geom_point(aes(x = Long, y = Lat, size = Count, name= Country),
              data = deaths, 
              color = 'black', alpha = .5) +
   labs(size = 'Cases',title="World Map of COVID-19 Deaths(March 28)")
 
 ggplotly(Conf_Map, tooltip = c('Count','Country'))
 
 #RECOVERED
 
 Count = as.integer(unlist(recovered[,ncol(recovered)]))
 
 Conf_Map = world +
   geom_point(aes(x = Long, y = Lat, size = Count, name= Country),
              data = recovered, 
              color = 'purple', alpha = .5) +
   labs(size = 'Cases',title="World Map of COVID-19 Recoveries(March 28)")
 
 ggplotly(Conf_Map, tooltip = c('Count','Country'))
 
 #________________________________________________________________________________
 