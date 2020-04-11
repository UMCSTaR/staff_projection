library(shiny)
library(tidyverse)
library(rhandsontable)
library(shinyjs)
library(plotly)
library(readxl)

# read data -------
chime <- read_csv("./data/2020-04-08_projected_census.csv")
# team <- read_xlsx("./data/staff_table.xlsx")  

# read team ratio 
team_ratio = readxl::read_xlsx("./data/team_ratio_shift.xlsx") %>% 
    mutate_if(is.numeric, as.integer)   

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
        
        
        
        # ICU
        team_icu = reactive({
            team_table() %>%
                filter(team_type == "ICU") %>%
                transmute(role, ratio = n_bed_per_person, ratio_s = n_bed_per_person_stretch,
                          shift_length_hr, shift_per_week)
        })
        
        # non-icu
        team_gen = reactive({
            team_table() %>%
                filter(team_type == "General") %>%
                transmute(role, ratio = n_bed_per_person, ratio_s = n_bed_per_person_stretch,
                          shift_length_hr, shift_per_week)        
            })
        
        # CHIME ---
        chime_table <- reactive({
            if (is.null(input$chime_up)) {
                chime %>% 
                    select(-ventilated) 
                    
            } else {
                read_csv(input$chime_up$datapath) %>% 
                    select(-ventilated) 
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
        reset_table = tibble(role = c("Role1", NA, NA),
                             ratio = as.numeric(rep(0, 3)),
                             ratio_s = as.numeric(rep(0, 3)),
                             shift_length_hr = rep(0,3),
                             shift_per_week = rep(0,3))
        
        
        # reset to clear table
        observeEvent(input$reset,{
            output$x1 <- renderRHandsontable({
                rhandsontable(
                    reset_table %>%
                        rename(
                            "Ratio (Normal)" = ratio,
                            "Ratio (Crisis)" = ratio_s,
                            Role = role,
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
        
        # reset to default --------
        observeEvent(input$reset_to_ori,{
            output$x1 <- renderRHandsontable({
                rhandsontable(
                    team_icu() %>%
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
                    team_gen() %>% 
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
        
        # editable ratios table -----
        output$x1 <- renderRHandsontable({
            rhandsontable(
                team_icu() %>%
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
                team_gen() %>% 
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
        
        ratio_table <- reactive({
            rbind(gen_ratio_table(), icu_ratio_table())
        })
        
        display_table <- reactive({
            chart_data(chime_edit(),ratio_table()) %>% 
                mutate(`Count Staff Reduction` = as.integer(n_staff_week* (1+input$reduction/100)))
        })
        

        
        # plots -------
        output$plot_crisis <- renderPlotly({
            plot_chart_data(display_table(), mode = "Crisis")
        })
        
        
        output$plot_norm <- renderPlotly({
            plot_chart_data(display_table(), mode = "Normal")
        })
        
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
                paste("chime_ratio_combined", ".csv", sep="")
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
