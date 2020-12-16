rm(list=ls())
cat('\014')
options(scipen = 999)

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
library(spdep)

load("data/spdm_input.RData")

data_plm <- data_plm %>% ungroup()
#Simple autoregressive model

ar_model <- lm(change ~ lag_change + lag_changesq + urate_2019 + pct_no_hs + pov_2018 + hub,data=data_plm)
summary(ar_model)

#AR model with exogenous factors (fixed effects)

ar_ex_fixed_model <-lm(change ~ lag_change + lag_changesq + urate_2019 + pct_no_hs + pov_2018 + hub + factor(nearest_in_set2) - 1,data=data_plm)
summary(ar_ex_fixed_model)

ar_ex_loctime_fixed_model <-lm(change ~ lag_change + lag_changesq + urate_2019 + pct_no_hs + pov_2018 + hub + factor(nearest_in_set2) + factor(date) - 1,data=data_plm)
summary(ar_ex_loctime_fixed_model)

# Spatial models

W_adj <- mat2listw(W_adj_norm)
W_dist <- mat2listw(W_dist_norm)
W_FI <- mat2listw(W_FI_norm)

adj_model <- spml(formula = change ~ lag_change + lag_changesq, data = data_plm , index = c("nearest_in_set2", "date"), 
     listw = W_adj_norm, model = "within", effect="twoway", lag = TRUE, spatial.error = "b")
summary(adj_model)

dist_model <- spml(formula = change ~ lag_change + lag_changesq, data = data_plm, index = c("nearest_in_set2", "date"), 
                  listw = W_dist_norm, model = "within", effect="twoway", lag = TRUE, spatial.error = "b")
summary(dist_model)

fi_model <- spml(formula = change ~ lag_change + lag_changesq, data = data_plm %>% filter(nearest_in_set2!="UIN"), index = c("nearest_in_set2", "date"), 
                  listw = W_FI_norm, model = "within", effect="twoway", lag = TRUE, spatial.error = "b")
summary(fi_model)

          