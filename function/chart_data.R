chart_data <- function(chime, ratio_table, capacity) {
  require(tidyverse)
  
  chime_long = chime %>% 
    filter(day >= 0) %>% 
    rename(General = hospitalized, ICU = icu) %>%
    pivot_longer(c(ICU, General), names_to = 'team_type', values_to = 'n')
  
  ratio_table_long = ratio_table %>%
    pivot_longer(
      c(n_bed_per_person, n_bed_per_person_crisis),
      names_to = 'crisis_mode',
      values_to = 'n_bed_per_person'
    ) %>%
    mutate(crisis_mode = if_else(str_detect(crisis_mode, 'crisis'),'Crisis','Normal'))

  
  all = left_join(left_join(chime_long, ratio_table_long, by = 'team_type'), capacity, by = c("role"))
  print(all)
  
  all %>% 
    mutate(projected_bed_per_person = n / n_bed_per_person,
           projected_bed_per_person = 
             if_else(
               is.infinite(projected_bed_per_person),
               0,
               projected_bed_per_person
             ))
}