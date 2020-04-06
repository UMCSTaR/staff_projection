
# Load packages

library(tidyverse)
library(plotly)
library(readxl)

# Start UI

chime <- read_csv("./data/2020-04-02_projected_census.csv")
team <- read_xlsx("./data/staff_table.xlsx")

# Clean up CHIME 
chime <- chime %>%
  filter(day >= 0)

# Get rows after clean
chime_row <- nrow(chime)

# Create four plots: ICU / General; Normal / Crisis

icu_ratio <- team %>% 
  filter(team_type == "ICU") 

icu_ratio_row <- nrow(icu_ratio) + 2

icu_norm <- icu_ratio %>%
  select(role, n_bed_per_person) %>%
  mutate(day = 0,
         team_type = "ICU Normal") %>%
  pivot_wider(., id_cols = c("day", "team_type"), names_from = role, values_from = n_bed_per_person) %>%
  add_row(day = 1:chime_row) %>%
  fill(., 2:all_of(icu_ratio_row))

icu_crisis <- icu_ratio %>%
  select(role, n_bed_per_person_crisis) %>%
  mutate(day = 0,
         team_type = "ICU Crisis") %>%
  pivot_wider(., id_cols = c("day", "team_type"), names_from = role, values_from = n_bed_per_person_crisis) %>%
  add_row(day = 1:chime_row) %>%
  fill(., 2:all_of(icu_ratio_row))

gen_ratio <- team %>% 
  filter(team_type == "General") 

gen_ratio_row <- nrow(gen_ratio) + 2

gen_norm <- gen_ratio %>%
  select(role, n_bed_per_person) %>%
  mutate(day = 0,
         team_type = "General Normal") %>%
  pivot_wider(., id_cols = c("day", "team_type"), names_from = role, values_from = n_bed_per_person) %>%
  add_row(day = 1:chime_row) %>%
  fill(., 2:all_of(gen_ratio_row))

gen_crisis <- gen_ratio %>%
  select(role, n_bed_per_person_crisis) %>%
  mutate(day = 0,
         team_type = "General Crisis") %>%
  pivot_wider(., id_cols = c("day", "team_type"), names_from = role, values_from = n_bed_per_person_crisis) %>%
  add_row(day = 1:chime_row) %>%
  fill(., 2:all_of(gen_ratio_row)) 

# Append tables

lookup <- bind_rows(icu_norm, icu_crisis, gen_norm, gen_crisis)

look_col <- ncol(lookup)

lookup <- lookup %>%
  pivot_longer(., 3:all_of(look_col), names_to = "role", values_to = "n_bed_per_person")  %>%
  filter(!is.na(n_bed_per_person))

# Create visualization data frame

chime_lookup <- full_join(chime, lookup, by = "day")

chime_lookup <- chime_lookup %>%
  mutate(projected_bed_icu =  icu / n_bed_per_person,
         projected_bed_hosp =  hospitalized / n_bed_per_person,
         projected_bed_per_person = ifelse(team_type == "General Normal" | team_type == "General Crisis",
                                           projected_bed_hosp,
                                           ifelse(team_type == "ICU Normal" | team_type == "ICU Crisis",
                                                  projected_bed_icu, NA)),
         projected_bed_per_person = ifelse(is.infinite(projected_bed_per_person), 0, projected_bed_per_person))

