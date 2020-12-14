rm(list=ls())
cat('\014')

setwd("~/Classes/ECO2803/Research_Paper")
data_dir <- file.path(getwd(),'data')
plot_dir <- file.path(getwd(),'plots')

library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(plm)
library(splm)
library(sp)
library(maps)
library(maptools)

covid_data <- read_csv(file.path(data_dir,"us-counties.csv")) %>% 
  mutate(fips=as.numeric(fips)) %>%
  filter(!(state %in% c("Alaska","Puerto Rico","Virgin Islands","Hawaii","Northern Mariana Islands",
                        "Guam"))) %>%
  filter(!(is.na(fips)))
load(file.path(data_dir,"adj_matrix.RData"))

#Need to manually map a few of these since the fips codes don't match

county_loc_data <- county_loc_data %>%
  mutate(fips = ifelse(NAME=="fairfax",51059,fips),
         fips = ifelse(NAME=="baltimore",24005,fips))

### Demographic data

# Pop data to use for weights

pop_data <- read_csv(file.path(data_dir,"county_pop.csv"))[,c(1,20)] %>% mutate(fips=as.numeric(FIPStxt)) %>% select(c(fips,POP_ESTIMATE_2019))

# Education

educ_data <- read_csv(file.path(data_dir,"county_education.csv"))
educ_data <- educ_data[,c(1,2,3,44,45,46,47)] 
names(educ_data) <- c("fips","state","county_nm","pct_no_hs","pct_hs","pct_some_col","pct_degree")
educ_data <- educ_data %>% 
  mutate(fips=as.numeric(fips)) %>% 
  left_join(county_loc_data,by=c("fips")) %>%
  filter(!(is.na(nearest_in_set2))) %>% 
  left_join(pop_data,by=c("fips")) %>%
  group_by(nearest_in_set2) %>%
  mutate(weight = POP_ESTIMATE_2019/sum(POP_ESTIMATE_2019)) %>%
  summarize(pct_no_hs = sum(pct_no_hs*weight),
            pct_hs = sum(pct_hs*weight),
            pct_some_col = sum(pct_some_col*weight),
            pct_degree = sum(pct_degree*weight))

# Unemployment

unemp_data <- read_csv(file.path(data_dir,"county_unemp.csv"))[,c(1,86,87,88)] %>%
  mutate(fips=as.numeric(FIPStxt)) %>% select(c(fips,Unemployment_rate_2019,Median_Household_Income_2018,Med_HH_Income_Percent_of_State_Total_2018))
names(unemp_data) <- c("fips","urate_2019","medhhinc_2018","medhhinc_pctst_2018")

unemp_data <- unemp_data %>% 
  mutate(fips=as.numeric(fips)) %>% 
  left_join(county_loc_data,by=c("fips")) %>%
  filter(!(is.na(nearest_in_set2))) %>% 
  left_join(pop_data,by=c("fips")) %>%
  group_by(nearest_in_set2) %>%
  mutate(weight = POP_ESTIMATE_2019/sum(POP_ESTIMATE_2019)) %>%
  summarize(urate_2019 = sum(urate_2019*weight),
            medhhinc_2018 = sum(medhhinc_2018*weight),
            medhhinc_pctst_2018 = sum(medhhinc_pctst_2018*weight))

# Poverty

poverty_data <- read_csv(file.path(data_dir,"county_poverty.csv"))[,c(1,8)] %>%
  mutate(fips=as.numeric(FIPStxt)) %>% select(c(fips,POVALL_2018))
names(poverty_data) <- c("fips","pov_2018")

poverty_data <- poverty_data %>% 
  mutate(fips=as.numeric(fips)) %>% 
  left_join(county_loc_data,by=c("fips")) %>%
  filter(!(is.na(nearest_in_set2))) %>% 
  left_join(pop_data,by=c("fips")) %>%
  group_by(nearest_in_set2) %>%
  mutate(weight = POP_ESTIMATE_2019/sum(POP_ESTIMATE_2019)) %>%
  summarize(pov_2018 = sum(pov_2018*weight))


#Group covid data by airport/county group

covid_data_lag <- covid_data %>% 
  left_join(county_loc_data,by=c("fips")) %>%
  group_by(nearest_in_set2,date) %>%
  summarize(cases=sum(cases)) #%>%
  # mutate(change = cases - dplyr::lag(cases),
  #        lag_change = dplyr::lag(change) )

data_comb <- covid_data_lag %>%
  left_join(unemp_data,by="nearest_in_set2") %>%
  left_join(educ_data,by="nearest_in_set2") %>%
  left_join(poverty_data,by="nearest_in_set2") %>%
  filter(date >= "2020-03-15" & date < "2020-07-01") %>%
  arrange(nearest_in_set2,date) %>%
  na.omit()


#balance the panel
ndays <- length(unique(data_comb$date))
all_dates <-  unique(data_comb$date)
missing_dfs <- list()
j=0
for(i in unique(data_comb$nearest_in_set2)){
  grp_sub <- data_comb %>% filter(nearest_in_set2==i)
  
  if(nrow(grp_sub) < ndays){
    j=j+1
    missing_dts <- as.Date(setdiff(all_dates, unique(grp_sub$date)))
    num_mis <- length(missing_dts)
    missing_df <- cbind.data.frame(rep(i,num_mis),missing_dts,rep(0,num_mis),grp_sub$urate_2019[1:num_mis],
                                   grp_sub$medhhinc_2018[1:num_mis],grp_sub$medhhinc_pctst_2018[1:num_mis],
                                   grp_sub$pct_no_hs[1:num_mis],grp_sub$pct_hs[1:num_mis],
                                   grp_sub$pct_some_col[1:num_mis],grp_sub$pct_degree[1:num_mis],
                                   grp_sub$pov_2018[1:num_mis])
    names(missing_df) <- names(grp_sub)
    missing_dfs[[j]] <- missing_df
  }

}
  
data_comb_balance <- rbind.data.frame( data_comb,do.call(rbind.data.frame,missing_dfs)) %>%
  arrange(nearest_in_set2,date) %>%
  mutate(change = cases - dplyr::lag(cases),
        lag_change = dplyr::lag(change),
        lag_changesq = lag_change^2)

# Add in a dummy for airport hub

hub_codes <- c("PHX","LAX","SFO","DEN","MIA","ORD","BWI","BOS","DEN","MCO","ATL","MDW","MSP","LAS","STL",
               "EWR","JFK","LGA","CLT","PHL","BNA","DFW","IAH","SLC","IAD","SEA")

data_comb_balance <- data_comb_balance %>%
  mutate(hub = ifelse(nearest_in_set2 %in% hub_codes,1,0))

data_plm <- pdata.frame(data_comb_balance,index=c("nearest_in_set2","date")) %>% na.omit()

rm(list=ls()[! ls() %in% c("data_plm","W_dist_norm","W_adj_norm","W_FI_norm","county_loc_data")])

save.image(file = file.path(file.path(getwd(),'data'),"spdm_input.RData"))

