plot_chart_data <- function(.data, mode = 'Normal', digits = 1, interactive = TRUE) {
  require(tidyverse)
  
  d_processed = .data %>%
    filter(crisis_mode == mode) %>% 
    mutate_if(is.numeric, round, digits = digits) %>% 
    rename_all(stringr::str_to_title) %>% 
    rename_all(stringr::str_replace_all, pattern = '_', replacement =' ') %>% 
    rename('Projected Number of Staff' = `Projected bed per person`,
           'Staff Needed'= `Count Staff Reduction`)
  
  p = d_processed %>%  
    ggplot(
      aes(
        x = Date,
        # y = `Projected Number of Staff`,
        y = `Staff Needed`,
        colour = Role
      )
    ) +
    geom_line() +
    labs(
      title   = paste("Projected Staffing Needs:", mode),
      x       = "",
      y       = "",
      colour  = "Roles",
      caption = "Estimates from CHIME and user-inputted ratios"
    ) +
    scico::scale_color_scico_d() +        # change if needed
    facet_wrap(~ `Team type`, scales = "free", ncol = 4) +
    theme_minimal() +
    theme(
      title = element_text(size = 10)
    )
  
  if (interactive)  
    plotly::ggplotly(p)
  else
    p
}
