# Comparing different R_0 Calculations

library(tidyr)
library(dplyr)
library(ggplot2)
library(yaml)
library(data.table)
library(EpiEstim)
library(nleqslv)
library(zoo)

rm(list=ls())
cat('\014')
set.seed(0)


setwd("~/Classes/ECO2803/Research_Paper")
data_dir <- file.path(getwd(),'data')
plot_dir <- file.path(getwd(),'plots')
config <- read_yaml("config.yaml")

# Simulate a path of infections/recoveries

N1_init <- config$parameters$initial_counts$N1_init
N2_init <- c(5,10,25,50,75,100)
N3_init <- config$parameters$initial_counts$N3_init
n_tot <- sum(N1_init,N2_init,N3_init)
n_periods <- config$parameters$sim_parameters$time_periods
recovery_intensity <- config$parameters$disease_parameters$c
contagion_effect <- config$parameters$disease_parameters$a
j=0
n2_list=list()
for(n2 in N2_init){
j=j+1
N_list_init <- cbind.data.frame(time = 0, N1 = N1_init, N2 = n2, N3 = N3_init)
N_list_evolution <- N_list_init
migration_list=list()
for(i in 1:n_periods){
  
  N_12 <- rbinom(1,N_list_evolution$N1[i],(contagion_effect*N_list_evolution$N2[i]/n_tot))
  N_23 <- rbinom(1,N_list_evolution$N2[i],recovery_intensity)
  updated_counts <- c(time=i,N1 = N_list_evolution$N1[i] - N_12,N2 = N_list_evolution$N2[i] + N_12 - N_23,N3 = N_list_evolution$N3[i] + N_23)
  N_list_evolution <- rbind.data.frame(N_list_evolution,updated_counts)
  migration_list[[i]] <- c(time=i,N_12=N_12,N_23=N_23)
  
}

migration_list <- suppressWarnings( rbind_list(migration_list) )

N_list_long <- N_list_evolution %>%
  pivot_longer(c(N1, N2, N3), names_to = "Group", values_to = "Count")

n2_rep <- cbind.data.frame(time=N_list_evolution$time,N2=N_list_evolution$N2) %>% mutate(`Initial Infected` = as.character(n2))

n2_list[[j]] <- n2_rep
}

n2_all <- do.call(rbind.data.frame,n2_list)
n2_all$`Initial Infected` <- factor(n2_all$`Initial Infected`, levels = unique(n2_all$`Initial Infected`))
count_evolution <- ggplot(data = n2_all) + aes(x=time,y=N2,color=`Initial Infected`) + geom_line() + xlab("") + ylab("")
