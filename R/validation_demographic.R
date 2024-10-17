library(epicR)
library(tidyverse)

# load census validation targets
USSimulation <-read_csv("data-raw/USSimulation.csv")

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
# merge US sim and EPIC pop outputs
validate_pop_size <-  USSimulation %>%
  rename(US_popsize=value) %>%
  left_join(epic_popsize_age_long, by=c("year","age"))

# scaled population graph
library(ggplot2)
library(ggthemes)

# Define the 'year' and 'savePlots' variables as needed

savePlots <- TRUE  # Set this to TRUE if you want to save the plot, FALSE if you don't
year <- 2035  # Example year, adjust as needed

# Prepare the data for plotting
dfPredicted <- data.frame(population = USpop_scaled$US_popsize_scaled, age = USpop_scaled$age)
dfSimulated <- data.frame(population = USpop_scaled$EPIC_popsize_scaled, age = USpop_scaled$age)

# Make the simulated population negative for the pyramid effect
dfSimulated$population <- dfSimulated$population * (-1)

# Create the plot
p <- ggplot(NULL, aes(x = age, y = population)) +
  theme_tufte(base_size = 14, ticks = FALSE) +  # Minimal theme
  geom_bar(aes(fill = "Simulated"), data = dfSimulated, stat = "identity", alpha = 0.5) +  # Simulated population in blue
  geom_bar(aes(fill = "Predicted"), data = dfPredicted, stat = "identity", alpha = 0.5) +  # Predicted population in red
  theme(axis.title = element_blank()) +  # Remove axis titles
  ggtitle("Simulated vs. Predicted Population Pyramid") +  # Add plot title
  theme(legend.title = element_blank()) +  # Remove legend title
  scale_y_continuous(name = "Population", labels = scales::comma) +  # Format y-axis with commas
  scale_x_continuous(name = "Age", labels = scales::comma)  # Format x-axis for age
# Save the plot if 'savePlots' is TRUE
if (savePlots) {
  ggsave(paste0("Population_", year, ".tiff"), plot = p, device = "tiff", dpi = 300)
}

# Display the plot
plot(p)


# create dataframe of EPIC population size by sex
epic_popsize_sex<- data.frame(1:time_horizon,output$n_alive_by_ctime_sex)
names(epic_popsize_sex) <- c("year", "male", "female")
epic_popsize_sex$overall<- epic_popsize_sex$male + epic_popsize_sex$female
epic_popsize_sex$year <- seq(1, nrow(epic_popsize_sex))










