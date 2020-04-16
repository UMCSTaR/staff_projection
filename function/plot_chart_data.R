plot_chart_data <- function(.data, staff_needs =quo(`Accounting For Staff Reduction`) ,
                            mode = 'Normal', digits = 1, highchart = TRUE){
                            # team_type = 'General') {
  require(tidyverse)
  require(glue)
  
  d_processed = .data %>%
    filter(crisis_mode == mode) %>% 
    mutate_if(is.numeric, round, digits = digits) %>% 
    rename_all(stringr::str_to_title) %>% 
    rename_all(stringr::str_replace_all, pattern = '_', replacement =' ') %>% 
    rename('Projected Number of Staff' = `Projected bed per person`,
           'Staff Needed'= !!staff_needs)
  
  
  # highchart
  high_chart_p = highchart() %>%
    hc_xAxis(type = "datetime", dateTimeLabelFormats = list(day = '%d of %b')) %>%
    hc_yAxis_multiples(create_yaxis(naxis = 2, heights = c(1, 1))) %>% 
    hc_add_series(
      yAxis = 0,
    d_processed %>%
      filter(`Team type` == "General"),
    type = 'line',
    hcaes(y = `Staff Needed`, group = Role, x = Date)
  )  %>% 
    hc_add_series(
      yAxis = 1,
      d_processed %>%
        filter(`Team type` == "ICU"),
      type = 'line',
      hcaes(y = `Staff Needed`, group = Role, x = Date)
    ) %>%
    hc_chart(backgroundColor = "white") %>%
    # hc_colors(scico::scico(10, palette = 'batlow')) %>%
    hc_title(text = "mytitle",
             style = list(fontSize = "15px")) 
    
  

# ggplot
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
    # geom_hline(
    #   # aes(yintercept = `Total employees at full capacity`, linetype = Role, col = Role),
    #   # size = 0.5, alpha = 0.8, show.legend = FALSE
    #   aes(yintercept = `Total employees at full capacity`, col = Role),
    #   size = 0.3, alpha = 0.8, show.legend = FALSE, linetype = "dashed"
    # ) +
    scico::scale_color_scico_d() + # change if needed
    facet_wrap(~ `Team type`, scales = "free", nrow = 2) +
    theme_minimal() +
    theme(
      title = element_text(size = 10)
    )
  
  
  
  if (highchart)
    high_chart_p 
  else
    plotly::ggplotly(p, height = 450, width = 800)
}
