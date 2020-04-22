plot_chart_data <- function(.data, digits = 1, interactive = TRUE) {
  require(tidyverse)

  d_processed = .data %>%
    filter(team_type %in% c("General Crisis", "ICU Crisis")) %>%
    mutate_if(is.numeric, round, digits = digits) %>%
    rename_all(stringr::str_to_title) %>%
    rename_all(stringr::str_replace_all, pattern = '_', replacement =' ')

  p = d_processed %>%
    ggplot(
      aes(
        x = Date,
        y = `Projected bed per person`,
        colour = Role
      )
    ) +
    geom_line() +
    labs(
      title   = "Projected Staffing Needs: Crisis",
      x       = "",
      y       = "",
      colour  = "Roles",
      caption = "Estimates from CHIME and user-inputted ratios"
    ) +
    scico::scale_color_scico_d() +
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
