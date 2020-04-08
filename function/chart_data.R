chart_data <- function(chime, ratio_table) {
  
  require(tidyverse)
  
  chime_long = chime %>% 
    filter(day >= 0) %>% 
    rename(General = hospitalized, ICU = icu) %>%
    select(-ventilated) %>%
    pivot_longer(c(ICU, General), names_to = 'team_type', values_to = 'n')
  
  ratio_table_long = ratio_table %>%
    pivot_longer(
      c(n_bed_per_person, n_bed_per_person_crisis),
      names_to = 'crisis_mode',
      values_to = 'n_bed_per_person'
    ) %>%
    mutate(crisis_mode = if_else(str_detect(crisis_mode, 'crisis'),'Crisis','Normal'))
  
  all = left_join(chime_long, ratio_table_long, by = 'team_type')
  
  all %>% 
    mutate(projected_bed_per_person = n / n_bed_per_person) %>% 
    mutate_if(is.numeric, function(x) if_else(is.infinite(x), NA_real_, x))
}

