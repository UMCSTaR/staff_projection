plot_chart_data <- function(.data, staff_needs = quo(`Accounting For Staff Reduction`), mode = "Normal", digits = 1, highcharter = TRUE) {
  require(tidyverse)
  require(glue)

  d_processed <- .data %>%
    filter(crisis_mode == mode) %>%
    mutate_if(is.numeric, round, digits = digits) %>%
    rename_all(stringr::str_to_title) %>%
    rename_all(stringr::str_replace_all, pattern = "_", replacement = " ") %>%
    rename(
      "Projected Number of Staff" = `Projected bed per person`,
      "Staff Needed" = !!staff_needs
    ) %>%
    select(Date, `Team type`, Role, `Staff Needed`, `Total employees at full capacity`)

  d_processed <- as.data.frame(rbind(
    d_processed,
    d_processed %>% group_by(Date, Role) %>%
      summarize(
        `Staff Needed` = sum(`Staff Needed`),
        `Total employees at full capacity` = unique(`Total employees at full capacity`)
      ) %>%
      mutate(`Team type` = "Total") %>% ungroup()
  )) %>%
    mutate(`Team type` = factor(`Team type`, levels = c("Total", "General", "ICU"), ordered = T))

  # plotly
  team_type_name <- c(
    `Total` = "Total",
    `ICU` = "ICU",
    `General` = " Non-ICU"
  )


  p <- d_processed %>%
    ggplot(
      aes(
        x = Date,
        y = `Staff Needed`
        # group = Role,
      )
    ) +
    geom_line(aes(col = Role), show.legend = FALSE) +
    labs(
      title = paste("Projected Staffing Needs:", mode),
      x = "",
      y = "",
      colour = "Roles"
      # caption = "Estimates from CHIME and user-inputted ratios"
    ) +
    geom_hline(
      data = subset(d_processed, `Team type` == "Total"),
      aes(
        yintercept = `Total employees at full capacity`,
        col = Role
      ),
      size = 0.5,
      alpha = 0.8,
      show.legend = FALSE,
      linetype = "dashed"
    ) +
    scale_color_brewer(palette = "Paired") + # change if needed
    facet_wrap(~`Team type`, scales = "free", nrow = 3, labeller = as_labeller(team_type_name)) +
    theme_minimal() +
    theme(
      title = element_text(size = 10)
    )


  # highcharter
  # cols <- scico::scico(palette = "hawaii",begin = 0.1, end = 0.8, n = 10)
  cols <- RColorBrewer::brewer.pal(10, "Paired")
  cols <- substr(cols, 0, 7)

  high_chart_p <- hchart(
    d_processed %>%
      filter(`Team type` == "Total"),
    type = "line",
    hcaes(y = `Staff Needed`, group = Role, x = Date),
  ) %>%
    hc_title(
      text = "Total Staffing Needs (ICU and Non-ICU)",
      margin = 20, align = "left", style = list(fontWeight = "bold", fontSize = "1.5em")
    ) %>%
    hc_subtitle(
      text = "Hover over the plot to see your staffing needs in details.
                Click the dropdown menu to the right for data/plot export.",
      margin = 20, align = "left", style = list(fontSize = "1em")
    ) %>%
    hc_chart(backgroundColor = "white") %>%
    # hc_legend(align = "right", verticalAlign = "top",
    #           layout = "vertical", x = 0, y = 100) %>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_xAxis(title = "") %>%
    hc_yAxis(title = "") %>%
    hc_legend(enabled = F) %>%
    hc_colors(cols) %>%
    hc_tooltip(table = TRUE, sort = TRUE, style = list(fontSize = "1em")) %>%
    # positioner = JS("function () { return { x: 300, y: 45 }; }"))  %>%
    hc_exporting(enabled = TRUE) # enable exporting option




  if (highcharter) {
    high_chart_p
  } else {
    plotly::ggplotly(p, height = 450, width = 800)
  }
}
