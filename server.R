library(shiny)
library(tidyverse)
library(highcharter)
library(rhandsontable)
library(shinyjs)
library(plotly)
library(readxl)


# read data
chime <- read_csv("./data/2020-04-08_projected_census.csv") %>% 
  mutate(date = seq(Sys.Date(),Sys.Date()+117,by = 1)) %>% 
  tidyr::replace_na(list(icu = 0, hospitalized =0, ventilated = 0)) 
team <- read_xlsx("./data/staff_table.xlsx", sheet = "updated_4_16") %>%
  mutate_if(is.numeric, as.integer)

# read team ratio
capacity_def <- team %>%
  select("role", "total_employees_at_full_capacity") %>%
  mutate(total_employees_at_full_capacity = as.integer(total_employees_at_full_capacity)) %>%
  distinct()

# ICU
team_icu <- team %>%
  filter(team_type == "ICU") %>%
  transmute(role,
    ratio = n_bed_per_person, ratio_s = n_bed_per_person_stretch,
    shift_length_hr, shift_per_week
  )

# non-icu
team_gen <- team %>%
  filter(team_type == "General") %>%
  transmute(role,
    ratio = n_bed_per_person, ratio_s = n_bed_per_person_stretch,
    shift_length_hr, shift_per_week
  )


# data procssing for plot
source("function/chart_data.R")
source("function/plot_chart_data.R")
source("function/max_table_under_plot.R")


# Define server
shinyServer(
  function(input, output, session) {

    # default vs. upload --------
    # Team ratio ---
    team_table <- reactive({
      if (is.null(input$team_in)) {
        team
      } else {
        read_xlsx(input$team_in$datapath)
      }
    })

    
    # capacity_table <- reactive({
    #     if (is.null(input$team_in)) {
    #         capacity
    #     } else {
    #         read_xlsx(input$team_in$datapath)
    #     }
    # })

    capacity_table <- reactive(capacity_def)

    # ICU
    team_icu_react <- reactive({
      team_table() %>%
        filter(team_type == "ICU") %>%
        transmute(role,
          ratio = n_bed_per_person, ratio_s = n_bed_per_person_stretch,
          shift_length_hr, shift_per_week
        )
    })

    # non-icu
    team_gen_react <- reactive({
      team_table() %>%
        filter(team_type == "General") %>%
        transmute(role,
          ratio = n_bed_per_person, ratio_s = n_bed_per_person_stretch,
          shift_length_hr, shift_per_week
        )
    })

    # capacity
    capacity_gen <- reactive({
      capacity_table() %>% select("role", "total_employees_at_full_capacity")
    })


    # capacity toggle --------
    observe({
      toggle(id = "total_bed", condition = input$advanced_census_input)
      toggle(id = "icu_perc", condition = input$advanced_census_input)
      toggle(id = "capacity_perc", condition = input$advanced_census_input)
      toggle(id = "advanced_input_help", condition = input$advanced_census_input)
    })


    # CHIME ----
    chime_table <- reactive({
      if (is.null(input$chime_up)) {
        chime %>%
          select(-ventilated)
      } else {
        read_csv(input$chime_up$datapath) %>%
          select(day, date, hospitalized, icu)
      }
    })

    # editable projected census ------
    output$prejected_census <- renderRHandsontable({
      rhandsontable(
        chime_table() %>%
          filter(date >= Sys.Date()) %>%
          mutate(day = as.integer(row_number() - 1)),
        rowHeaders = FALSE, width = 470, stretchH = "all", height = 300, highlightRow = TRUE
      )
    })

    # chime default  -------
    observeEvent(
      input$default_chime,
      output$prejected_census <- renderRHandsontable({
        rhandsontable(
          chime_table() %>%
            filter(date >= Sys.Date()) %>%
            mutate(day = as.integer(row_number() - 1)),
          rowHeaders = FALSE,
          width = 470,
          stretchH = "all",
          height = 300,
          highlightRow = TRUE
        )
      })
    )
    

    # reset Chime table -----
    observeEvent(
      input$reset_census,
      output$prejected_census <- renderRHandsontable({
        rhandsontable(
          tibble(
            day = c(0:4),
            date = c(Sys.Date():Sys.Date() + 4),
            hospitalized = 0,
            icu = 0
          ) %>%
            mutate(
              date = lubridate::as_date(day, origin = Sys.Date()),
              day = as.integer(day),
              hospitalized = as.integer(hospitalized),
              icu = as.integer(icu)
            ),
          rowHeaders = FALSE, width = 470, stretchH = "all", height = 300, highlightRow = TRUE
        )
      })
    )

    # Chime editable tables
    values <- reactiveValues()

    # Chime table ---------
    chime_edit <- reactive({
      if (is.null(input$prejected_census)) {
        return(chime)
      }

      values$df_chime <- hot_to_r(input$prejected_census)
    })


    # Staff ratio editable tables -------

    # reset reference table -------
    reset_table <- tibble(
      role = c("Role1", "Role2", "Role3"),
      ratio = as.numeric(rep(0, 3)),
      ratio_s = as.numeric(rep(0, 3)),
      total_employees_at_full_capacity = as.integer(rep(0, 3)),
      shift_length_hr = rep(0, 3),
      shift_per_week = rep(0, 3)
    )

    # reset to clear table
    observeEvent(input$reset, {
      output$x1 <- renderRHandsontable({
        rhandsontable(
          reset_table %>%
            transmute(
              Role = role,
              "Ratio (Normal)" = ratio,
              "Ratio (Stretch)" = ratio_s,
              "Shift Length(hours)" = shift_length_hr,
              "Number of Shifts/week" = shift_per_week
            ) %>%
            mutate_if(is.numeric, as.integer),
          rowHeaders = FALSE,
          width = 650,
          stretchH = "all",
          highlightRow = TRUE
        ) %>%
          hot_cols(colWidths = 100)
      })

      output$x2 <- renderRHandsontable({
        rhandsontable(
          reset_table %>%
            transmute(
              Role = role,
              "Ratio (Normal)" = ratio,
              "Ratio (Stretch)" = ratio_s,
              "Shift Length(hours)" = shift_length_hr,
              "Number of Shifts/week" = shift_per_week
            ) %>%
            mutate_if(is.numeric, as.integer),
          rowHeaders = FALSE, width = 650, stretchH = "all", highlightRow = TRUE
        ) %>%
          hot_cols(colWidths = 100)
      })
    })

    observeEvent(input$clear_capacity, {
      output$x3 <- renderRHandsontable({
        rhandsontable(
          reset_table %>%
            select(role, total_employees_at_full_capacity) %>%
            mutate(total_employees_at_full_capacity = as.integer(total_employees_at_full_capacity)) %>%
            rename(
              "Total employees (Max)" = total_employees_at_full_capacity,
              Role = role
            ),
          rowHeaders = FALSE, width = 570, stretchH = "all", highlightRow = TRUE
        ) %>%
          hot_cols(colWidths = 100)
      })
    })

    # reset to default --------
    observeEvent(input$reset_to_ori, {
      output$x1 <- renderRHandsontable({
        rhandsontable(
          team_icu_react() %>%
            rename(
              "Ratio (Normal)" = ratio,
              "Ratio (Stretch)" = ratio_s,
              Role = role,
              "Shift Length(hours)" = shift_length_hr,
              "Number of Shifts/week" = shift_per_week
            ) %>%
            mutate_if(is.numeric, as.integer),
          rowHeaders = FALSE, width = 650, stretchH = "all", highlightRow = TRUE
        ) %>%
          hot_cols(colWidths = 100)
      })

      output$x2 <- renderRHandsontable({
        rhandsontable(
          team_gen_react() %>%
            rename(
              "Ratio (Normal)" = ratio,
              "Ratio (Stretch)" = ratio_s,
              Role = role,
              "Shift Length(hours)" = shift_length_hr,
              "Number of Shifts/week" = shift_per_week
            ) %>%
            mutate_if(is.numeric, as.integer),
          rowHeaders = FALSE, width = 650, stretchH = "all", highlightRow = TRUE
        ) %>%
          hot_cols(colWidths = 100)
      })
    })

    observeEvent(input$reset_default_capacity, {
      output$x3 <- renderRHandsontable({
        rhandsontable(
          capacity_def %>%
            mutate(total_employees_at_full_capacity = as.integer(total_employees_at_full_capacity)) %>%
            rename(
              "Total employees (Max)" = total_employees_at_full_capacity,
              Role = role
            ),
          rowHeaders = FALSE, width = 570, stretchH = "all", highlightRow = TRUE
        ) %>%
          hot_cols(colWidths = 100)
      })
    })

    # editable ratios table -----
    output$x1 <- renderRHandsontable({
      rhandsontable(
        team_icu_react() %>%
          rename(
            "Ratio (Normal)" = ratio,
            "Ratio (Stretch)" = ratio_s,
            Role = role,
            "Shift Length(hours)" = shift_length_hr,
            "Number of Shifts/week" = shift_per_week
          ) %>%
          mutate_if(is.numeric, as.integer),
        rowHeaders = FALSE, width = 650, stretchH = "all", highlightRow = TRUE
      ) %>%
        hot_cols(colWidths = 100)
    })

    output$x2 <- renderRHandsontable({
      rhandsontable(
        team_gen_react() %>%
          rename(
            "Ratio (Normal)" = ratio,
            "Ratio (Stretch)" = ratio_s,
            Role = role,
            "Shift Length(hours)" = shift_length_hr,
            "Number of Shifts/week" = shift_per_week
          ) %>%
          mutate_if(is.numeric, as.integer),
        rowHeaders = FALSE, width = 650, stretchH = "all", highlightRow = TRUE
      ) %>%
        hot_cols(colWidths = 100)
    })

    output$x3 <- renderRHandsontable({
      rhandsontable(
        capacity_gen() %>%
          mutate(total_employees_at_full_capacity = as.integer(total_employees_at_full_capacity)) %>%
          rename(
            "Total employees (Max)" = total_employees_at_full_capacity,
            Role = role
          ),
        rowHeaders = FALSE, width = 570, stretchH = "all", highlightRow = TRUE
      ) %>%
        hot_cols(colWidths = 100)
    })


    # calculations happen here ------
    icu_ratio_table <- reactive({
      if (is.null(input$x1)) {
        return(
          team_icu %>%
            mutate(team_type = "ICU") %>%
            rename(
              n_bed_per_person = ratio,
              n_bed_per_person_crisis = ratio_s
            ) %>%
            select(team_type, everything())
        )
      }

      values$df <- hot_to_r(input$x1) %>%
        mutate(team_type = "ICU") %>%
        rename(
          role = Role,
          n_bed_per_person = "Ratio (Normal)",
          n_bed_per_person_crisis = "Ratio (Stretch)",
          shift_length_hr = "Shift Length(hours)",
          shift_per_week = "Number of Shifts/week"
        ) %>%
        select(team_type, everything())
    })


    gen_ratio_table <- reactive({
      if (is.null(input$x2)) {
        return(
          team_gen %>%
            mutate(team_type = "General") %>%
            rename(
              n_bed_per_person = ratio,
              n_bed_per_person_crisis = ratio_s
            ) %>%
            select(team_type, everything())
        )
      }

      values$df <- hot_to_r(input$x2) %>%
        mutate(team_type = "General") %>%
        rename(
          role = Role,
          n_bed_per_person = "Ratio (Normal)",
          n_bed_per_person_crisis = "Ratio (Stretch)",
          shift_length_hr = "Shift Length(hours)",
          shift_per_week = "Number of Shifts/week"
        ) %>%
        select(team_type, everything())
    })

    capacity_edit_table <- reactive({
      if (is.null(input$x3)) {
        return(
          capacity_def # () %>%
          # mutate(total_employees_at_full_capacity = as.integer(total_employees_at_full_capacity)) %>%
          # rename(
          #     "Total employees (Max)" = total_employees_at_full_capacity,
          #     Role = role
          # )
        )
      }

      values$df <- hot_to_r(input$x3) %>%
        rename(
          total_employees_at_full_capacity = "Total employees (Max)",
          role = "Role"
        )
    })


    # Compute ratio table -- combines ICU and General ratio tables into one
    ratio_table <- reactive({
      rbind(gen_ratio_table(), icu_ratio_table())
    })


    # (1) Puts together other datasets into one dataframe -- for example, CHIME table, Ratio table (from above),
    # .. and Capacity table.
    # (2) Sends the data to `chart_data` function for processing -- also make a few column changes
    # (3) Result is saved in `display_table`
    display_table <- reactive({
      chart_data(chime_edit(), ratio_table(), capacity_edit_table(),
        total_bed = input$total_bed,
        capacity_perc = input$capacity_perc / 100,
        icu_perc = input$icu_perc / 100
      ) %>%
        mutate(
          `Accounting for Staff Reduction` = as.integer(n_staff_week * (1 + input$reduction / 100)),
          all_covd_non_covid_staff = as.integer(ceiling(`Accounting for Staff Reduction` + n_staff_non_covid_week))
        )
    })

    # advanced inputs cal---------

    # output$test <- renderTable(display_table() %>%
    #                                select(n_staff_non_covid_week, non_cov_pt, everything()) %>%
    #                                filter(n_staff_non_covid_week<0))



    # plots -------

    observeEvent(input$advanced_census_input, {
      # advanced inputs including non covid
      if (input$advanced_census_input == TRUE) {
        # crisis
        output$plot_crisis <- renderUI({
          if (input$show_icu_non_icu_plots == TRUE) {
            output$crisis_icu_non_icu_adv <- renderPlotly({
              plot_chart_data(
                display_table(),
                mode = "Crisis",
                staff_needs = quo(`All covd non covid staff`),
                highcharter = F
              )
            })
            plotlyOutput("crisis_icu_non_icu_adv")
          }

          else if (input$show_icu_non_icu_plots == FALSE) {
            output$crisis_total_adv <- renderHighchart({
              plot_chart_data(
                display_table(),
                mode = "Crisis",
                staff_needs = quo(`All covd non covid staff`)
              )
            })
            highchartOutput("crisis_total_adv")
          }
        })

        # normal
        output$plot_norm <- renderUI({
          if (input$show_icu_non_icu_plots == TRUE) {
            output$norm_icu_non_icu_adv <- renderPlotly({
              plot_chart_data(
                display_table(),
                mode = "Normal",
                staff_needs = quo(`All covd non covid staff`),
                highcharter = F
              )
            })
            plotlyOutput("norm_icu_non_icu_adv")
          }

          else if (input$show_icu_non_icu_plots == FALSE) {
            output$norm_total_adv <- renderHighchart({
              plot_chart_data(
                display_table(),
                mode = "Normal",
                staff_needs = quo(`All covd non covid staff`)
              )
            })
            highchartOutput("norm_total_adv")
          }
        })
      }

      # no advanced inputs
      else if (input$advanced_census_input == FALSE) {
        # crisis
        output$plot_crisis <- renderUI({
          if (input$show_icu_non_icu_plots == TRUE) {
            output$crisis_icu_non_icu <- renderPlotly({
              plot_chart_data(display_table(),
                mode = "Crisis",
                highcharter = FALSE
              )
            })
            plotlyOutput("crisis_icu_non_icu")
          }

          else if (input$show_icu_non_icu_plots == FALSE) {
            output$crisis_total <- renderHighchart({
              plot_chart_data(display_table(), mode = "Crisis")
            })
            highchartOutput("crisis_total")
          }
        })

        # normal
        output$plot_norm <- renderUI({
          if (input$show_icu_non_icu_plots == TRUE) {
            output$norm_icu_non_icu <- renderPlotly({
              plot_chart_data(display_table(),
                mode = "Normal",
                highcharter = FALSE
              )
            })
            plotlyOutput("norm_icu_non_icu")
          }

          else if (input$show_icu_non_icu_plots == FALSE) {
            output$norm_total <- renderHighchart({
              plot_chart_data(display_table(), mode = "Normal")
            })
            highchartOutput("norm_total")
          }
        })
      }
    })


    # table underneath the plots-------
    capacity_stats <- capacity_def %>%
      select(role, total_employees_at_full_capacity)

    max_date <- reactive({
      display_table() %>%
        filter(
          crisis_mode == "Normal",
          n == max(n)
        ) %>%
        select(day) %>%
        unique() %>%
        pull()
    })


    observeEvent(input$advanced_census_input, {
      # cat(file=stderr(), "drawing histogram with", as.character(display_table()), "bins", "\n")
      
      if (input$advanced_census_input == TRUE) {
        # advanced inputs including non coivd

        output$table_result_normal <- renderTable({

          # validate(
          #     need(display_table() %>%
          #              filter(n_staff_non_covid_week == min(n_staff_non_covid_week)) %>%
          #              distinct(n_staff_non_covid_week) %>%
          #              pull()> 0, "Your current inputs are not valid")
          # )

        max_table_under_plot(
            display_table(),
            mode = "Normal",
            total_staff_value = quo(all_covd_non_covid_staff)
          ) %>%
            filter(day == max_date()) %>%
            select(-day)
        })

        output$table_result_crisis <- renderTable({
          max_table_under_plot(
            display_table(),
            mode = "Crisis",
            total_staff_value = quo(all_covd_non_covid_staff)
          ) %>%
            filter(day == max_date()) %>%
            select(-day)
        })
      } else {
        output$table_result_normal <- renderTable({
          max_table_under_plot(
            display_table(),
            mode = "Normal",
            total_staff_value = quo(`Accounting for Staff Reduction`)
          ) %>%
            filter(day == max_date()) %>%
            select(-day)
        })

        output$table_result_crisis <- renderTable({
          max_table_under_plot(
            display_table(),
            mode = "Crisis",
            total_staff_value = quo(`Accounting for Staff Reduction`)
          ) %>%
            filter(day == max_date()) %>%
            select(-day)
        })
      }
    })



    # buttons ------
    observeEvent(input$generateButton, {
      updateTabsetPanel(session, "inTabset", selected = "Normal")
    })


    observeEvent(input$update_gen, {
      updateTabsetPanel(session, "inTabset", selected = "edit_ratio_table")
    })

    observeEvent(input$update_capacity, {
      updateTabsetPanel(session, "inTabset", selected = "capacity_table")
    })


    observeEvent(input$prejected_cesus, {
      updateTabsetPanel(session, "inTabset", selected = "census")
    })

    output$downloadData_combine_file <- downloadHandler(
      filename = function() {
        paste("chime_ratio_combined", ".csv", sep = "")
      },
      content = function(file) {
        write.csv(display_table(), file, row.names = FALSE)
      }
    )

    

    # output$downloadData_icu_ratio <- downloadHandler(
    #   filename = function() {
    #     paste('ICU_Staffing_role_and_ratio', Sys.Date(), '.csv', sep='')
    #   },
    #   content = function(con) {
    #     finalDF <- hot_to_r(input$x1)
    #     write.csv(finalDF, con)
    #   }
    # )
    #
    # output$downloadData_norm <- downloadHandler(
    #   filename = function() {
    #     paste('staffing_normal', Sys.Date(), '.csv', sep='')
    #   },
    #   content = function(con) {
    #     finalDF <- hot_to_r(input$x2)
    #     write.csv(norm_staff_table(), con)
    #   }
    # )

    output$downloadData_all_ratio <- downloadHandler(
      filename = function() {
        paste("Staffing_role_and_ratio", Sys.Date(), ".xlsx", sep = "")
      },
      content = function(con) {
        finalDF <- hot_to_r(input$x1) %>%
          mutate(team_type = "ICU") %>%
          rename(
            role = Role,
            n_bed_per_person = "Ratio (Normal)",
            n_bed_per_person_crisis = "Ratio (Stretch)"
          ) %>%
          select(team_type, everything())

        finalDF_non_icu <- hot_to_r(input$x2) %>%
          mutate(team_type = "General") %>%
          rename(
            role = Role,
            n_bed_per_person = "Ratio (Normal)",
            n_bed_per_person_crisis = "Ratio (Stretch)"
          ) %>%
          select(team_type, everything())


        all_ratio <- rbind(finalDF, finalDF_non_icu)

        writexl::write_xlsx(all_ratio, path = con)
        
        
      }
    )
  }
)
