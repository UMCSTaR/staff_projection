library(shiny)
library(tidyverse)
library(rhandsontable)
library(shinyjs)
library(plotly)
library(readxl)

# read data -------
chime <- read_csv("./data/2020-04-08_projected_census.csv")

team <- read_xlsx("./data/staff_table.xlsx") 
capacity <- read_xlsx("./data/staffing_normal2020-04-08_other_additional input.xlsx", skip = 1) %>%
    filter(!is.na(Role)) %>% select(-1) %>%
    select_if(function(x) !(all(is.na(x)) | all(x==""))) %>%
    rename_all(function(x) tolower(str_replace_all(x, " ", "_")))


# read team ratio 
team_ratio = readxl::read_xlsx("./data/team_ratio_shift.xlsx") %>% 
    mutate_if(is.numeric, as.integer)   


capacity_def = capacity %>% 
    select("role", "total_employees_at_full_capacity") %>%
    mutate(total_employees_at_full_capacity = as.integer(total_employees_at_full_capacity))
    
               
# ICU 
team_icu = team_ratio %>%
    filter(team_type == "ICU") %>%
    transmute(role, ratio = n_bed_per_person, ratio_s = n_bed_per_person_stretch,
              shift_length_hr, shift_per_week)

# non-icu
team_gen = team_ratio %>%
    filter(team_type == "General") %>%
    transmute(role, ratio = n_bed_per_person, ratio_s = n_bed_per_person_stretch,
              shift_length_hr, shift_per_week)


# data procssing for plot
source("function/chart_data.R")
source("function/plot_chart_data.R")


# Define server 
shinyServer(
    function(input, output, session) {
        
        # default vs. upload --------
        # Team ratio ---
        team_table <- reactive({
            if (is.null(input$team_in)) {
                team_ratio
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
        
        capacity_table <- reactive(capacity)

       
        
        # ICU
        team_icu_react = reactive({
            team_table() %>%
                filter(team_type == "ICU") %>%
                transmute(role, ratio = n_bed_per_person, ratio_s = n_bed_per_person_stretch,
                          shift_length_hr, shift_per_week)
        })
        
        # non-icu
        team_gen_react = reactive({
            team_table() %>%
                filter(team_type == "General") %>%
                transmute(role, ratio = n_bed_per_person, ratio_s = n_bed_per_person_stretch,
                          shift_length_hr, shift_per_week)        
            })
        
        # capacity
        capacity_gen = reactive({
            capacity_table() %>% select("role", "total_employees_at_full_capacity")
        })
        
        
        # CHIME ---
        chime_table <- reactive({
            if (is.null(input$chime_up)) {
                chime %>% 
                    select(-ventilated) 
                    
            } else {
                read_csv(input$chime_up$datapath)  %>% 
                    select(day, date, hospitalized, icu) 
            }
        }) 
        
        # editable projected census ------
        output$prejected_census <- renderRHandsontable({
            rhandsontable(
                chime_table() %>% 
                    filter(day>=0) %>% 
                    mutate(day = as.integer(day)),
                rowHeaders = FALSE, width = 470, stretchH = "all", height = 300)
            
        })
        
        # chime default  -------
        observeEvent(input$default_chime,
                     output$prejected_census <- renderRHandsontable({
                         rhandsontable(
                             chime_table() %>%
                                 filter(day >= 0) %>%
                                 mutate(day = as.integer(day)),
                             rowHeaders = FALSE,
                             width = 470,
                             stretchH = "all",
                             height = 300
                         )
                         
                     }))
        
        
        # reset Chime table
        observeEvent(input$reset_census,
                     output$prejected_census <- renderRHandsontable({
                         rhandsontable(
                             tibble(day = c(1:5),
                                    date = NA,
                                    hospitalized = 0,
                                    icu = 0) %>% 
                                 mutate(date = lubridate::as_date(day, origin = "2020-03-22"),
                                        day = as.integer(day),
                                        hospitalized = as.integer(hospitalized),
                                        icu = as.integer(icu)),
                             rowHeaders = FALSE, width = 470, stretchH = "all", height = 300)
                     })
        )
        
        # Chime editable tables 
        values <- reactiveValues()
        
        # Chime table ---------
        chime_edit <- reactive({
            if(is.null(input$prejected_census))
                return(chime)
            
            values$df_chime = hot_to_r(input$prejected_census) 
        })
        
        
        # Staff ratio editable tables -------

        # reset reference table -------
        reset_table = tibble(role = c("Role1", 'Role2', "Role3"),
                             ratio = as.numeric(rep(0, 3)),
                             ratio_s = as.numeric(rep(0, 3)),
                             total_employees_at_full_capacity = as.integer(rep(0, 3)),
                             shift_length_hr = rep(0,3),
                             shift_per_week = rep(0,3)
                             )
        
        # reset to clear table
        observeEvent(input$reset, {
            output$x1 <- renderRHandsontable({
                rhandsontable(
                    reset_table %>% 
                        transmute(
                            Role = role,
                            "Ratio (Normal)" = ratio,
                            "Ratio (Crisis)" = ratio_s,
                            "Shift Length(hour)" = shift_length_hr,
                            "Number of Shifts/week" = shift_per_week
                        ) %>%
                        mutate_if(is.numeric, as.integer),
                    rowHeaders = FALSE,
                    width = 650,
                    stretchH = "all"
                ) %>%
                    hot_cols(colWidths = 100)
            })
            
            output$x2 <- renderRHandsontable({
                rhandsontable(
                    reset_table %>% 
                        transmute(
                            Role = role,
                            "Ratio (Normal)" = ratio,
                            "Ratio (Crisis)" = ratio_s,
                            "Shift Length(hour)" = shift_length_hr,
                            "Number of Shifts/week" = shift_per_week
                        ) %>%
                        mutate_if(is.numeric, as.integer), rowHeaders = FALSE, width = 650, stretchH = "all"
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
                    rowHeaders = FALSE, width = 570, stretchH = "all"
                ) %>% 
                    hot_cols(colWidths = 100) 
            })
        })
        
        # reset to default --------
        observeEvent(input$reset_to_ori,{
            output$x1 <- renderRHandsontable({
                rhandsontable(
                    team_icu_react() %>%
                        rename(
                            "Ratio (Normal)" = ratio,
                            "Ratio (Crisis)" = ratio_s,
                            Role = role,
                            "Shift Length(hour)" = shift_length_hr,
                            "Number of Shifts/week" = shift_per_week
                        ) %>%
                        mutate_if(is.numeric, as.integer), rowHeaders = FALSE, width = 650, stretchH = "all"
                ) %>% 
                    hot_cols(colWidths = 100) 
            })
            
            output$x2 <- renderRHandsontable({
                rhandsontable(
                    team_gen_react() %>% 
                        rename(
                            "Ratio (Normal)" = ratio,
                            "Ratio (Crisis)" = ratio_s,
                            Role = role,
                            "Shift Length(hour)" = shift_length_hr,
                            "Number of Shifts/week" = shift_per_week
                        ) %>%
                        mutate_if(is.numeric, as.integer), rowHeaders = FALSE, width = 650, stretchH = "all"
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
                        ), rowHeaders = FALSE, width = 570, stretchH = "all"
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
                        "Ratio (Crisis)" = ratio_s,
                        Role = role,
                        "Shift Length(hour)" = shift_length_hr,
                        "Number of Shifts/week" = shift_per_week
                    ) %>%
                    mutate_if(is.numeric, as.integer), rowHeaders = FALSE, width = 650, stretchH = "all"
            ) %>% 
                hot_cols(colWidths = 100) 
            
        })
        
        output$x2 <- renderRHandsontable({
            rhandsontable(
                team_gen_react() %>% 
                    rename(
                        "Ratio (Normal)" = ratio,
                        "Ratio (Crisis)" = ratio_s,
                        Role = role,
                        "Shift Length(hour)" = shift_length_hr,
                        "Number of Shifts/week" = shift_per_week
                    ) %>%
                    mutate_if(is.numeric, as.integer), rowHeaders = FALSE, width = 650, stretchH = "all"
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
                    ), rowHeaders = FALSE, width = 570, stretchH = "all"
            ) %>% 
                hot_cols(colWidths = 100) 
            
        })
   
        
        # calculations happen here ------
        icu_ratio_table <- reactive({
            if(is.null(input$x1))
                return(
                    team_icu %>%
                        mutate(team_type = "ICU") %>%
                        rename(n_bed_per_person = ratio ,
                               n_bed_per_person_crisis = ratio_s) %>%
                        select(team_type, everything())
                )
            
            values$df = hot_to_r(input$x1) %>% 
                mutate(team_type = "ICU") %>% 
                rename(role = Role,
                       n_bed_per_person = "Ratio (Normal)" ,
                       n_bed_per_person_crisis = "Ratio (Crisis)",
                       shift_length_hr = "Shift Length(hour)",
                       shift_per_week = "Number of Shifts/week") %>% 
                select(team_type, everything())
        })
        
        
        gen_ratio_table <- reactive({
            if(is.null(input$x2))
                return(
                    team_gen %>%
                        mutate(team_type = "General") %>% 
                        rename(n_bed_per_person = ratio ,
                               n_bed_per_person_crisis = ratio_s) %>% 
                        select(team_type, everything())
                )
            
            values$df = hot_to_r(input$x2) %>% 
                mutate(team_type = "General") %>% 
                rename(role = Role,
                       n_bed_per_person = "Ratio (Normal)" ,
                       n_bed_per_person_crisis = "Ratio (Crisis)",
                       shift_length_hr = "Shift Length(hour)",
                       shift_per_week = "Number of Shifts/week") %>% 
                select(team_type, everything())
        })
        
        capacity_edit_table <- reactive({
            if(is.null(input$x3))
                return(
                    capacity_def #() %>%
                        # mutate(total_employees_at_full_capacity = as.integer(total_employees_at_full_capacity)) %>%
                        # rename(
                        #     "Total employees (Max)" = total_employees_at_full_capacity,
                        #     Role = role
                        # )
                )
            
            values$df = hot_to_r(input$x3) %>%
                rename(
                    total_employees_at_full_capacity = "Total employees (Max)",
                    role = "Role"
                )
        })
        
        
        ratio_table <- reactive({
            rbind(gen_ratio_table(), icu_ratio_table())
        })
        
        display_table <- reactive({
            chart_data(chime_edit(),ratio_table(), capacity_edit_table()) %>% 
                mutate(`Accounting for Staff Reduction` = as.integer(n_staff_week* (1+input$reduction/100)))
        })
        
        output$test = renderTable(display_table())
        
        
        # plots -------
        output$plot_crisis <- renderPlotly({
            plot_chart_data(display_table(), mode = "Crisis")
        })
        
        
        output$plot_norm <- renderPlotly({
            plot_chart_data(display_table(), mode = "Normal")
        })
        
        # table underneath the plots-------
        capacity_stats =  capacity %>% 
            select(role, total_employees_at_full_capacity)
        
        output$table_result_normal <- renderTable(
            display_table() %>% 
                filter(crisis_mode == "Normal") %>%
                select(
                    day,
                    date,
                    team_type,
                    role,
                    n,
                    total_employees_at_full_capacity,
                    `Accounting for Staff Reduction`
                ) %>%
                pivot_wider(names_from = team_type,
                            values_from = `Accounting for Staff Reduction`) %>%
                tidyext::row_sums(General, ICU, varname = "all", na_rm = TRUE) %>%
                mutate(all = as.integer(all)) %>% 
                filter(n == max(n)) %>%
                transmute(Role = role, `Max Needed` = all,
                          "Total employees" = total_employees_at_full_capacity)
        )
        
        
        output$table_result_crisis <- renderTable(
            display_table() %>% 
                filter(crisis_mode == "Crisis") %>%
                select(
                    day,
                    date,
                    team_type,
                    role,
                    n,
                    total_employees_at_full_capacity,
                    `Accounting for Staff Reduction`
                ) %>%
                pivot_wider(names_from = team_type,
                            values_from = `Accounting for Staff Reduction`) %>%
                tidyext::row_sums(General, ICU, varname = "all", na_rm = TRUE) %>%
                mutate(all = as.integer(all)) %>% 
                filter(n == max(n)) %>%
                transmute(Role = role, `Max Needed` = all,
                          "Total employees" = total_employees_at_full_capacity)
            
        )
        
        
        
        # buttons ------
        observeEvent(input$generateButton, {
            updateTabsetPanel(session, "inTabset", selected = "Normal")
        })
        
        
        observeEvent(input$update_gen, {
            updateTabsetPanel(session, "inTabset", selected = "edit_ratio_table")
        })
        
        observeEvent(input$prejected_cesus, {
            updateTabsetPanel(session, "inTabset", selected = "census")
        })
        
        output$downloadData_combine_file <- downloadHandler( 
            filename = function(){
                paste("chime_ratio_combined", ".csv", sep = "")
            }, 
            
            content = function(file) {
                write.csv(display_table(), file, row.names = FALSE)
            })
        
        
        
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
                paste('Staffing_role_and_ratio', Sys.Date(), '.xlsx', sep='')
            },
            content = function(con) {
                finalDF <- hot_to_r(input$x1) %>% 
                    mutate(team_type = "ICU") %>% 
                    rename(role = Role,
                           n_bed_per_person = "Ratio (Normal)" ,
                           n_bed_per_person_crisis = "Ratio (Crisis)") %>% 
                    select(team_type, everything())
                
                finalDF_non_icu <- hot_to_r(input$x2) %>% 
                    mutate(team_type = "General") %>% 
                    rename(role = Role,
                           n_bed_per_person = "Ratio (Normal)" ,
                           n_bed_per_person_crisis = "Ratio (Crisis)") %>% 
                    select(team_type, everything())
                
                
                all_ratio = rbind(finalDF, finalDF_non_icu)
                
                writexl::write_xlsx(all_ratio, path = con)
            }
        )
        
        
    }
    
)
