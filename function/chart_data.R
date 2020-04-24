chart_data <- function(chime, ratio_table, capacity,
                       total_bed,
                       icu_perc,
                       capacity_perc,
                       advanced = TRUE) {
  require(tidyverse)

  chime_long <- chime %>%
    filter(date >= Sys.Date()) %>%
    rename(General = hospitalized, ICU = icu) %>%
    pivot_longer(c(ICU, General), names_to = "team_type", values_to = "n")

  ratio_table_long <- ratio_table %>%
    pivot_longer(
      c(n_bed_per_person, n_bed_per_person_crisis),
      names_to = "crisis_mode",
      values_to = "n_bed_per_person"
    ) %>%
    mutate(crisis_mode = if_else(str_detect(crisis_mode, "crisis"), "Crisis", "Normal"))


  all <- left_join(left_join(chime_long, ratio_table_long, by = "team_type"), capacity, by = c("role"))

  all_cov <- all %>%
    mutate(
      projected_bed_per_person = n / n_bed_per_person,
      projected_bed_per_person =
        if_else(
          is.infinite(projected_bed_per_person),
          0,
          projected_bed_per_person
        ),
      n_staff_day = projected_bed_per_person * (24 / shift_length_hr),
      n_staff_week = n_staff_day * 7 / shift_per_week
    )


  all_cov_and_non_cov <- all_cov %>%
    mutate(
      non_cov_pt = ifelse(
        team_type == "General",
        (1 - icu_perc) * capacity_perc * total_bed - n,
        icu_perc * capacity_perc * total_bed - n
      ),
      # non-negative non-cov_pt
      # non_cov_pt = abs(non_cov_pt),
      n_staff_non_covid = non_cov_pt / n_bed_per_person,
      n_staff_non_covid =
        if_else(is.infinite(n_staff_non_covid),
          0,
          n_staff_non_covid
        ),
      n_staff_non_covid_day = n_staff_non_covid * (24 / shift_length_hr),
      n_staff_non_covid_week = n_staff_non_covid_day * 7 / shift_per_week,
    )

  if (advanced) {
    all_cov_and_non_cov
  } else {
    all_cov
  }
}
