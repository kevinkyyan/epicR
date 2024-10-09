library(epicR)
library(tidyverse)

# load census validation targets
USSimulation <-read_csv("data-raw/USSimulation.csv") #check NAs

# run epic
settings<- get_default_settings()
settings$record_mode <- 0
n_sim <- settings$n_base_agents
init_session(settings = settings)

input <- Cget_inputs()
time_horizon <- 55
input$values$global_parameters$time_horizon <- time_horizon

run(input=input$values)

inputs <- Cget_inputs()
output <- Cget_output_ex()

terminate_session()

# create dataframe of EPIC population size by age
epic_popsize_age<- data.frame(1:time_horizon,output$n_alive_by_ctime_age)
colnames(epic_popsize_age)[1] <- "year"
colnames(epic_popsize_age)[2:ncol(epic_popsize_age)] <- 1:(ncol(epic_popsize_age) - 1)
epic_popsize_age <- epic_popsize_age[, -(2:40)]
epic_popsize_age$year <- seq(2015, by = 1, length.out = nrow(epic_popsize_age))

epic_popsize_age_long <- epic_popsize_age %>%
                              pivot_longer(!year, names_to = "age", values_to = "EPIC_popsize") %>%
                                mutate(age=as.integer(age))


# create dataframe of EPIC population size by sex
epic_popsize_sex<- data.frame(1:time_horizon,output$n_alive_by_ctime_sex)
names(epic_popsize_sex) <- c("year", "male", "female")
epic_popsize_sex$overall<- epic_popsize_sex$male + epic_popsize_sex$female
epic_popsize_sex$year <- seq(1, nrow(epic_popsize_sex))

# merge US sim and EPIC pop outputs
validate_pop_size <-  USSimulation %>%
                            rename(US_popsize=value) %>%
                            left_join(epic_popsize_age_long, by=c("year","age"))









