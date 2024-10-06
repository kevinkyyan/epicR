library(epicR)
library(tidyverse)

# load census validation targets
USSimulation <-read_csv("data-raw/USSimulation.csv")

# run epic
init_session()
settings<- get_default_settings()
n_sim <- settings$n_base_agents
inputs <- Cget_inputs()
time_horizon <- inputs$global_parameters$time_horizon
run()
output <- Cget_output_ex()

# create dataframe of EPIC population size by age
epic_popsize_age<- data.frame(1:time_horizon,output$n_alive_by_ctime_age)
colnames(epic_popsize_age)[1] <- "year"
colnames(epic_popsize_age)[2:ncol(epic_popsize_age)] <- 1:(ncol(epic_popsize_age) - 1)
epic_popsize_age <- epic_popsize_age[, -(2:40)]
epic_popsize_age$year <- seq(1, nrow(epic_popsize_age))

# notes transform horizontal into vertical

# create dataframe of EPIC population size by sex
epic_popsize_sex<- data.frame(1:time_horizon,output$n_alive_by_ctime_sex)
names(epic_popsize_sex) <- c("year", "male", "female")
epic_popsize_sex$overall<- epic_popsize_sex$male + epic_popsize_sex$female
epic_popsize_sex$year <- seq(1, nrow(epic_popsize_sex))








