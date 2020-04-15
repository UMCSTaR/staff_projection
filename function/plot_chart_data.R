plot_chart_data <- function(.data, staff_needs =quo(`Accounting For Staff Reduction`) ,mode = 'Normal', digits = 1, interactive = TRUE) {
  require(tidyverse)
  require(glue)
  
  d_processed = .data %>%
    filter(crisis_mode == mode) %>% 
    mutate_if(is.numeric, round, digits = digits) %>% 
    rename_all(stringr::str_to_title) %>% 
    rename_all(stringr::str_replace_all, pattern = '_', replacement =' ') %>% 
    rename('Projected Number of Staff' = `Projected bed per person`,
           'Staff Needed'= !!staff_needs) %>%
    select(Date, `Team type`, Role, `Staff Needed`, `Total employees at full capacity`)
  
  d_processed <- as.data.frame(rbind(d_processed,
    d_processed %>% group_by(Date, Role) %>% 
      summarize(`Staff Needed` = sum(`Staff Needed`),
                `Total employees at full capacity` = unique(`Total employees at full capacity`)) %>%
      mutate(`Team type` = 'Total') %>% ungroup()
  )) %>%
    mutate(`Team type` = factor(`Team type`, levels = c("Total", "General", "ICU"), ordered = T))

  p = d_processed %>%
    ggplot(
      aes(
        x = Date,
        # y = `Projected Number of Staff`,
        y = `Staff Needed`,
        group = Role,
      )
    ) +
    geom_line(aes(col = Role), show.legend = FALSE) +
    labs(
      title   = paste("Projected Staffing Needs:", mode),
      x       = "",
      y       = "",
      colour  = "Roles",
      # linetype = "Capacity",
      caption = "Estimates from CHIME and user-inputted ratios"
    ) +
    geom_hline(
      data = subset(d_processed, `Team type` == "Total"),
      aes(yintercept = `Total employees at full capacity`, linetype = Role, col = Role),
      size = 0.5, alpha = 0.8, show.legend = FALSE, linetype = "dashed"
    ) +
    scico::scale_color_scico_d() + # change if needed
    facet_wrap(~ `Team type`, scales = "free", nrow = 3) +
    theme_minimal() +
    theme(
      title = element_text(size = 10)
    )
  
  if (interactive)  
    plotly::ggplotly(p, height = 450, width = 800)
  else
    p
}
