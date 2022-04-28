max_table_under_plot <-
  function(data,
           mode = "Normal",
           total_staff_value = quo(`Accounting for Staff Reduction`)) {
    data %>%
      filter(crisis_mode == mode) %>%
      select(
        day,
        date,
        team_type,
        role,
        total_employees_at_full_capacity,
        !!total_staff_value
      ) %>%
      pivot_wider(
        names_from = team_type,
        values_from = !!total_staff_value
      ) %>%
      # mutate(all = rowSums(c(General, ICU), na.rm = TRUE)) %>% 
      rowwise() %>% mutate(all = sum(c(General, ICU))) %>% 
      mutate(all = as.integer(all)) %>%
      transmute(
        Role = role,
        `Max Needed (ICU and Non-ICU)` = all,
        "Total Employees" = total_employees_at_full_capacity,
        day
      ) %>%
      arrange(desc(`Total Employees`))
  }
