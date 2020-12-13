      #Flight data summary plots/tables
      
      rm(list=ls())
      cat('\014')
      
      setwd("~/Classes/ECO2803/Research_Paper")
      data_dir <- file.path(getwd(),'data')
      plot_dir <- file.path(getwd(),'plots')
      
      library(dplyr)
      library(tidyr)
      library(ggplot2)
      library(readr)
      library(zoo)
      library(usmap)
      library(sp)
      library(rgeos)
      library(randomcoloR)
      
      flight_data <- read_csv(file.path(data_dir,"jantojun2020.csv")) %>% mutate(FL_DATE = as.Date(FL_DATE,"%m/%d/%Y"))
      covid_data <- read_csv(file.path(data_dir,"us-counties.csv"))
      county_loc_data <- read.delim(file.path(data_dir,"us-county-boundaries.csv")) %>% select(Geo.Point,NAME,NAMELSAD,STUSAB)
      airport_loc_data <- read_csv(file.path(data_dir,"us-airports.csv")) %>% select(latitude_deg,longitude_deg,iata_code) %>% unique()
      fips_data <- read_csv(file.path(data_dir,"countyfips.csv")) %>% select(c(sab,cname,fips))
      
      #Plot daily evolution of flight data from January 2020 to June 2020 in US
      
      daily_flights <- flight_data %>% 
        group_by(FL_DATE) %>% 
        summarize(Flights = n()) %>% 
        mutate(Date = FL_DATE) %>%
        arrange(Date) 
      
      flight_plot <- ggplot(data=daily_flights) + geom_line(aes(y=rollmean(Flights, 7, na.pad=TRUE),x=Date)) + ylab("Flights")
      ggsave(file.path(plot_dir,"all_flights.png"),flight_plot)

#Show how connected major airports are to different parts of the country

daily_flights <- flight_data %>% 
  group_by(DEST) %>% 
  summarize(Flights = n()) %>%
  arrange(desc(Flights)) %>%
  head(n=10)

flight_data_freq <- flight_data %>% 
  filter(DEST %in% unique(daily_flights$DEST),
         FL_DATE >= "2020-03-01") %>%
  group_by(ORIGIN_STATE_NM,DEST) %>%
  summarize(total_flights = n()) %>% 
  mutate(state=ORIGIN_STATE_NM)
  
all_airport_locations <- usmap_transform(airport_loc_data %>% select(longitude_deg,latitude_deg))
all_airport_locations$airport = airport_loc_data$iata_code

ATL_plot <- plot_usmap(data = (flight_data_freq %>% filter(DEST=="ATL")), values = "total_flights", color = "black") + 
     scale_fill_continuous(low = "white", high = "red",name = "Incoming Flights", label = scales::comma) + 
   theme(legend.position = "right") +
  geom_point(data = all_airport_locations %>% filter(airport=="ATL"), aes(x = longitude_deg.1, y = latitude_deg.1),color = "black", alpha = 1)

DEN_plot <- plot_usmap(data = (flight_data_freq %>% filter(DEST=="DEN")), values = "total_flights", color = "black") + 
  scale_fill_continuous(low = "white", high = "red",name = "Incoming Flights", label = scales::comma) + 
  theme(legend.position = "right") +
  geom_point(data = all_airport_locations %>% filter(airport=="DEN"), aes(x = longitude_deg.1, y = latitude_deg.1),color = "black", alpha = 1)

ORD_plot <- plot_usmap(data = (flight_data_freq %>% filter(DEST=="ORD")), values = "total_flights", color = "black") + 
   scale_fill_continuous(low = "white", high = "red",name = "Incoming Flights", label = scales::comma) + 
   theme(legend.position = "right") +
  geom_point(data = all_airport_locations %>% filter(airport=="ORD"), aes(x = longitude_deg.1, y = latitude_deg.1),color = "black", alpha = 1)
# 
# plot_usmap(data = (flight_data_freq %>% filter(DEST=="DFW")), values = "total_flights", color = "red") + 
#   scale_fill_continuous(low = "white", high = "red",name = "Incoming Flights", label = scales::comma) + 
#   theme(legend.position = "right")
# 
# plot_usmap(data = (flight_data_freq %>% filter(DEST=="CLT")), values = "total_flights", color = "red") + 
#   scale_fill_continuous(low = "white", high = "red",name = "Incoming Flights", label = scales::comma) + 
#   theme(legend.position = "right")

LAX_plot <- plot_usmap(data = (flight_data_freq %>% filter(DEST=="LAX")), values = "total_flights", color = "black") + 
  scale_fill_continuous(low = "white", high = "red",name = "Incoming Flights", label = scales::comma) + 
  theme(legend.position = "right") +
  geom_point(data = all_airport_locations %>% filter(airport=="LAX"), aes(x = longitude_deg.1, y = latitude_deg.1),color = "black", alpha = 1)

ggsave(file.path(plot_dir,"ATL_plot.png"),ATL_plot)
ggsave(file.path(plot_dir,"DEN_plot.png"),DEN_plot)
ggsave(file.path(plot_dir,"ORD_plot.png"),ORD_plot)
ggsave(file.path(plot_dir,"LAX_plot.png"),LAX_plot)

  # Aggregate flight info by day

daily_agg_data <- flight_data %>%
  group_by(FL_DATE,DEST,DEST_CITY_NAME,DEST_STATE_ABR,DEST_STATE_NM) %>% 
  summarize(Flights=n())

#Match counties to airports

  # Total flights

all_fight_data <- flight_data %>%
  group_by(DEST,DEST_CITY_NAME,DEST_STATE_ABR,DEST_STATE_NM) %>% 
  summarize(Flights=n())

all_airports <- all_fight_data %>% ungroup() %>% left_join(airport_loc_data,by=c("DEST"="iata_code")) %>% drop_na()

# Find shortest distance between each county and airport in data set

county_loc_data <- county_loc_data %>%
  separate(Geo.Point,into=c("latitude_deg","longitude_deg"),sep=",") %>%
  mutate(latitude_deg = as.numeric(latitude_deg),
         longitude_deg = as.numeric(longitude_deg))


set1sp <- SpatialPoints(county_loc_data %>% select(c(latitude_deg,longitude_deg)))
set2sp <- SpatialPoints(all_airports %>% select(latitude_deg,longitude_deg))
county_loc_data$nearest_in_set2 <- all_airports$DEST[apply(gDistance(set2sp, set1sp, byid=TRUE), 1, which.min)]

# Example of how this works for California

county_loc_california <- county_loc_data %>% 
  filter(STUSAB == "CA") %>% 
  left_join(all_airports,by=c("nearest_in_set2"="DEST")) %>%
  left_join(fips_data,by=c("NAME"="cname","STUSAB"="sab"))

ca_airports <- cbind.data.frame(airport = unique(county_loc_california$nearest_in_set2), 
                                num_code = c(1:(length(unique(county_loc_california$nearest_in_set2)))))

ca_fips <- county_loc_california %>%
  left_join(ca_airports,by=c("nearest_in_set2"="airport")) %>%
  select(fips,num_code) %>%
  mutate(num_code = as.factor(num_code))
  
county_loc_ca_map <- usmap_transform(county_loc_california %>% select(longitude_deg.y,latitude_deg.y))

set.seed(4)
palette <- distinctColorPalette(length(unique(county_loc_california$nearest_in_set2)))

  ca_map <- plot_usmap("counties",data=ca_fips, values = "num_code", color="black",size=0.5, include = c("CA", "OR","NV"))  + 
geom_point(data = county_loc_ca_map, aes(x = longitude_deg.y.1, y = latitude_deg.y.1),color = "red", alpha = 1) +
  theme(panel.background = element_rect(colour = "black")) + theme(legend.position = "none") +
  scale_fill_manual(values = palette)
                  
ggsave(file.path(plot_dir,"CA_example_county_match.png"),ca_map)                    
            
covid_data_date <- covid_data %>%
  group_by(date) %>%
  summarize(cases=sum(cases))

