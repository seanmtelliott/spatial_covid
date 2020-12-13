## Defining adjacency matricies

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
library(scales)


flight_data <- read_csv(file.path(data_dir,"jantojun2020.csv")) %>% mutate(FL_DATE = as.Date(FL_DATE,"%m/%d/%Y"))
county_loc_data <- read.delim(file.path(data_dir,"us-county-boundaries.csv")) %>% select(Geo.Point,NAME,NAMELSAD,STUSAB)
airport_loc_data <- read_csv(file.path(data_dir,"us-airports.csv")) %>% select(latitude_deg,longitude_deg,iata_code) %>% unique()
fips_data <- read_csv(file.path(data_dir,"countyfips.csv")) %>% select(c(sab,cname,fips))
county_adj <- read_csv(file.path(data_dir,"county_adjacency2010.csv")) %>% 
  mutate(fipscounty = as.numeric(fipscounty),
         fipsneighbor = as.numeric(fipsneighbor)) %>% 
  select(c(fipscounty,fipsneighbor))


### Group data by airport-county groups based on distance

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
  filter(!(STUSAB %in% c("AS","PR","VI","GU","MP","AK","HI"))) %>%
  separate(Geo.Point,into=c("latitude_deg","longitude_deg"),sep=",") %>%
  mutate(latitude_deg = as.numeric(latitude_deg),
         longitude_deg = as.numeric(longitude_deg),
         NAME = tolower(NAME),
         NAME = gsub("'","",NAME),
         NAME = gsub("-","",NAME),
         NAME = gsub("prince of waleshyder","Princeofwales",NAME),
         NAME = gsub("kusilvak","wadehampton",NAME),
         NAME = gsub("skagway","skagwayyakutat",NAME),
         NAME = gsub("hoonahangoon","skagwayhoonahangoon",NAME),
         NAME = gsub("doña ana","donaana",NAME)) %>%
  left_join(fips_data, by=c("STUSAB"="sab","NAME"="cname")) 

set1sp <- SpatialPoints(county_loc_data %>% select(c(latitude_deg,longitude_deg)))
set2sp <- SpatialPoints(all_airports %>% select(latitude_deg,longitude_deg))
county_loc_data$nearest_in_set2 <- all_airports$DEST[apply(gDistance(set2sp, set1sp, byid=TRUE), 1, which.min)]

# Find centroid of each group

county_loc_data <- county_loc_data %>%
  group_by(nearest_in_set2) %>%
  mutate(centroid_x = mean(latitude_deg),
            centroid_y = mean(longitude_deg)) %>%
  ungroup()

# Find distance between all centroids

group_centroids <- SpatialPoints((county_loc_data %>% select(c(centroid_x,centroid_y)) %>% unique()))

# W_dist <- 1/gDistance(group_centroids, group_centroids, byid=TRUE)
# W_dist[which(!is.finite(W_dist))] <- 0
# W_dist_norm <- W_dist/rowSums(W_dist)

W_dist <- 1/spDists(group_centroids,group_centroids,longlat = T)
W_dist[which(!is.finite(W_dist))] <- 0
W_dist_norm <- W_dist/rowSums(W_dist)

# Contiguity

county_loc_data_adj <- county_loc_data %>%
  left_join(county_adj, by=c("fips"="fipscounty")) %>% 
  arrange(nearest_in_set2)

all_groups <- cbind.data.frame(grp = unique(county_loc_data_adj$nearest_in_set2))
j=0
for(i in unique(county_loc_data_adj$nearest_in_set2)){
  j=j+1
  #Find all adjacent regions for this subgroup
  group_sub <- county_loc_data_adj %>%
    filter(nearest_in_set2 == i)
  
  unique_adj <- unique(group_sub$fipsneighbor)
  unique_grps <- unique(county_loc_data %>% filter(fips %in% unique_adj) %>% select(nearest_in_set2))
  adj_col <- cbind.data.frame(adj_grp = unique_grps,val = rep(1,length(unique_grps)))
  
  all_groups <- all_groups %>% left_join(adj_col,by=c("grp"="nearest_in_set2"))
  names(all_groups) <- c("grp",unique(county_loc_data_adj$nearest_in_set2)[1:j])
}

all_groups[is.na(all_groups)] <- 0

W_adj <- as.matrix(all_groups[,2:ncol(all_groups)])
diag(W_adj) <- 0
W_adj_norm <- W_adj/rowSums(W_adj)

# Bilateral Flight Intensity

flight_data_freq <- flight_data %>% 
  filter(FL_DATE >= "2020-03-01") %>%
  group_by(ORIGIN,DEST) %>%
  summarize(total_flights = n())

flight_data_freq_comb <- flight_data_freq %>% left_join(flight_data_freq,by=c("DEST"="ORIGIN","ORIGIN"="DEST"))
flight_data_freq_comb[is.na(flight_data_freq_comb)] <- 0

flight_freq <- flight_data_freq_comb %>%
  mutate(intensity = total_flights.x+total_flights.y) %>%
  select(c(ORIGIN,DEST,intensity)) %>%
  ungroup()

flight_grps <- unique(county_loc_data_adj$nearest_in_set2)[-324]

all_groups <- cbind.data.frame(grp = flight_grps)
j=0
for(i in flight_grps){
  j=j+1
  group_sub <- flight_freq %>%
    filter(ORIGIN==i) %>%
    select(c(DEST,intensity))
  if(nrow(group_sub)==0){
    NULL
  }else{
  all_groups <- all_groups %>% left_join(group_sub,by=c("grp"="DEST"))

  names(all_groups) <- c("grp",flight_grps[1:j])
  }
}

all_groups[is.na(all_groups)] <- 0

W_FI <- as.matrix(all_groups[,2:ncol(all_groups)])
W_FI_norm <- W_FI/rowSums(W_FI)
W_FI_norm[which(is.nan(W_FI_norm))] <- 0

county_loc_data <- county_loc_data %>% select(c(NAME,STUSAB,fips,nearest_in_set2))

rm(list=ls()[! ls() %in% c("W_dist_norm","W_adj_norm","W_FI_norm","county_loc_data")])

save.image(file = file.path(file.path(getwd(),'data'),"adj_matrix.RData"))
