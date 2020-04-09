#' Basic SIR model
#' 
#' @description Given the input parameters, produce the result of a SIR model.
#' 
#' @param population Regional population. Default is 10000.
#' @param infected Number infected on `start_date`. Default is 1.
#' @param recovered Number recovered on `start_date`. Default is 0.
#' @param doubling_time  The time it takes for the (infected) population to
#'   double in size. Default is 7 days.
#' @param recovery_time How long it takes an infected person to recover. Default
#'   is 14 days.
#' @param start_date When to start the simulation.
#' @param n_days Number of days from start to solve for.
#' @param R0 The basic reproduction number. Default is NULL.
#' @param plot Plot the basic result? Default is `FALSE`. Mostly for testing
#'   purposes.
#'
#' @details This calculates the result of a basic SIR model where S is the
#'   susceptible population, I is the infected population and R is the recovered
#'   population.  These are initially calculated based on the population number
#'   provided.
#'
#'   `R0` is not required and if not provided, it will be calculated as a
#'   function of doubling days and recovery time. Current suggestions are 1.4 -
#'   3.9.  The defaults would put R0 at 2.46.
#'   
#' @note `deSolve` package required, as the main workhorse is the `ode` function
#'   therein, and `dplyr` is used for some data processing.
#'   
#' @references https://pennchime.herokuapp.com/
#' http://en.wikipedia.org/wiki/Compartmental_models_in_epidemiology
#' 
#' @return a tibble of time points, and the S, I, and R values
#'
#'  
#' @examples
#' infection = sir_model(plot = T)
#' infection 
#' 
#' library(tidyverse)
#' 
#' infection %>%
#'   pivot_longer(-date, names_to = 'type', values_to = 'count') %>%
#'   ggplot(aes(date, count)) +
#'   geom_path(aes(color = type)) +
#'   scale_color_viridis_d(begin = .25, end = .75) +
#'   theme_minimal()
#' 
#' @export
sir_model = function(
  population = 10000,
  infected = 1,
  recovered = 0,
  doubling_time = 7, 
  recovery_time = 14,
  n_days = 100,
  R0 = NULL,
  start_date = Sys.Date(), 
  plot = FALSE
) {
  
  if (!require(deSolve)) {
    stop('deSolve package required.')
  }
  
  S = population - 1
  I = infected
  R = recovered
  
  if (R + I > population)
    stop('Population is less than recovered plus infected. Check inputs.')
  
  
  ### Set parameters
  intrinsic_growth_rate  = 2^(1/doubling_time) - 1
  
  if (is.null(R0) | is.character(R0)) 
    R0 = (intrinsic_growth_rate + 1/recovery_time) / (1/recovery_time)
  
  parameters = c(
    dt = doubling_time,
    N  = sum(S, I, R),
    R0 = R0
  )
  
  ## Create a SIR function
  
  sir = function(t, y, parms) {
    
    gamma = 1/parms[['dt']]
    beta  = parms[['R0']] * gamma / parms[['N']]
    
    dS = -beta * y[['S']] * y[['I']] 
    dI =  beta * y[['S']] * y[['I']]  - gamma * y[['I']]
    dR =                                gamma * y[['I']]
    
    list(c(dS, dI,  dR))
  }
  
  out =
    deSolve::ode(
      y = c(S = S, I = I, R = R),
      times = 1:n_days,
      func  = sir,
      parms = parameters
    )
  

  
  require(dplyr)
  
  sir_result = as_tibble(out) %>% 
    rename(susceptible = S, infected = I, recovered = R) %>% 
    mutate_all(as.integer) %>% 
    mutate(date = start_date + 0:(n_days-1)) %>% 
    select(date, everything(), -time)
  
  if (plot) {
    print(plot_sir(sir_result))
    invisible(return(sir_result))
  }
  else {
    sir_result
  }
  
}



