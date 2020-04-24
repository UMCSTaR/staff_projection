#!/usr/bin/env Rscript
wd <- getwd()

lintr::lint(file.path(wd, "ui.R"))
lintr::lint(file.path(wd, "server.R"))
lintr::lint(file.path(wd, "function", "chart_data.R"))
lintr::lint(file.path(wd, "function", "max_table_under_plot.R"))
lintr::lint(file.path(wd, "function", "plot_chart_data.R"))