plot_sir <- function(sir_result) {
  
  sir_long = sir_result %>% 
    tidyr::pivot_longer(-date, names_to = 'Type', values_to = 'Count') %>% 
    mutate(Type = stringr::str_to_title(Type))
  
  # sensible breaks
  mx = 
    sort(
      c(
        10, 
        seq(100, 900, 100),
        seq(1000, 9000, 1000),
        10000, 
        max(sir_long$Count)
      )
    )
  mx = mx[which(mx == max(sir_long$Count)) + 1]
  
  brks = seq(0, mx, by = mx/10)
  ylims = c(0, mx)
  
  max_infected = sir_long %>% 
    filter(Type == 'Infected', Count == max(sir_result$infected)) %>% 
    slice(1)  # sometimes might be more than one date
  
  ggplot(sir_long, aes(date, Count)) +
    geom_path(aes(color = Type), size = 1, alpha = .5) +
    geom_point(aes(color = Type), size = 3, data = max_infected) +
    geom_hline(aes(yintercept = Count), alpha = .05, data = max_infected) +
    scico::scale_color_scico_d(end = .75) +
    scale_x_date(
      date_breaks = '1 week',
      labels = function(x) stringr::str_sub(x, start=6)
    ) +
    scale_y_continuous(
      breaks = brks
    ) +
    labs(x = '',  y='') +
    guides(x = guide_axis(n.dodge = 2)) +
    theme_clean() +
    theme(
      axis.ticks = element_blank()
    )
}