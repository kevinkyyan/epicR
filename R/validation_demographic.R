library(epicR)
library(tidyverse)

# load census validation targets
USSimulation <-read_csv("data-raw/USSimulation.csv")

# run epic
init_session()
settings<- get_default_settings()
n_sim <- setting$n_base_agents
inputs <- Cget_inputs()
time_horizon <- inputs$global_parameters$time_horizon
run()
output <- Cget_output_ex()

epic_popsize<- data.frame(1:time_horizon,output$n_alive_by_ctime_sex)
names(epic_popsize) <- c("year", "male", "female")
epic
