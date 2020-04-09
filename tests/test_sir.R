
# Preliminaries -----------------------------------------------------------

library(tidyverse)

# source('function/sir.R')
R.utils::sourceDirectory('function')

# debugonce(sir_model)
test = sir_model()

# # debugonce(plot_sir)
# plot_sir(test)
sir_model(plot = T)
sir_model(400, 100, 0, plot = T)
