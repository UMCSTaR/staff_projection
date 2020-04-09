
# Load packages

library(tidyverse)

# Start UI

# chime <- readr::read_csv("data/2020-04-02_projected_census.csv")
chime <- readr::read_csv("data/2020-04-08_projected_census.csv")
team  <- readxl::read_xlsx("data/staff_table.xlsx") %>% 
  mutate(role = if_else(role == 'Pharn', 'Pharm', role))

# R.utils::sourceDirectory('function/')
source('function/chart_data.R')
# source('function/chart_data_original.R')
source('function/plot_chart_data.R')

# debugonce(chart_data)
chime_lookup = chart_data(chime, team)

# debugonce(plot_chart_data)
# plot_chart_data(chime_lookup, interactive = F)
plot_chart_data(chime_lookup, mode = 'Crisis', interactive = T)
plot_chart_data(chime_lookup, mode = 'Normal', interactive = T)

