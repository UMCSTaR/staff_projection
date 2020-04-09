
# Load packages

library(shiny)
library(tidyverse)
library(rhandsontable)
library(shinyjs)
library(plotly)
library(readxl)







# Server ---------------------
server <- function(input, output, session) {
  
  # default team ratios vs. upload --------
  # Team ratio ---
  team_table <- reactive({
    if (is.null(input$team_in)) {
      team
    } else {
      read_xlsx(input$team_in$datapath)
    }
  }) 

  # ICU
  team_icu = reactive({
    team_table() %>%
      filter(team_type == "ICU") %>%
      transmute(role, ratio = n_bed_per_person, ratio_s = n_bed_per_person_crisis)
  })

  # non-icu
  team_gen = reactive({
    team_table() %>%
      filter(team_type == "General") %>%
      transmute(role, ratio = n_bed_per_person, ratio_s = n_bed_per_person_crisis)
  })
  
  # CHIME ---
  chime_table <- reactive({
    if (is.null(input$chime_up)) {
      chime
    } else {
      read_csv(input$chime_up$datapath)
    }
  }) 
  
  


  # editable tables -------
    values <- reactiveValues()
  
  # reset reference table -------
  reset_table = tibble(role = c("Role1", "Role2", "Role3"),
                       ratio = as.numeric(rep(0, 3)),
                       ratio_s = as.numeric(rep(0, 3)))
  
  # reset to clear table
  observeEvent(input$reset,{
    output$x1 <- renderRHandsontable({
      rhandsontable(
        reset_table %>%
          rename(
            "Ratio (Normal)" = ratio,
            "Ratio (Crisis Mode)" = ratio_s,
            Role = role
          ) %>%
          mutate_if(is.numeric, as.integer), rowHeaders = FALSE, width = 570, stretchH = "all"
      ) %>% 
        hot_cols(colWidths = 100) 
    })
    
    output$x2 <- renderRHandsontable({
      rhandsontable(
        reset_table %>% 
          rename(
            "Ratio (Normal)" = ratio,
            "Ratio (Crisis Mode)" = ratio_s,
            Role = role
          ) %>%
          mutate_if(is.numeric, as.integer), rowHeaders = FALSE, width = 570, stretchH = "all"
      ) %>% 
        hot_cols(colWidths = 100) 
    })
  })
  
  # reset to default --------
  observeEvent(input$reset_to_ori,{
    output$x1 <- renderRHandsontable({
      rhandsontable(
        team_icu_def %>%
          rename(
            "Ratio (Normal)" = ratio,
            "Ratio (Crisis Mode)" = ratio_s,
            Role = role
          ) %>%
          mutate_if(is.numeric, as.integer), rowHeaders = FALSE, width = 570, stretchH = "all"
      ) %>%
        hot_cols(colWidths = 100)
    })

    output$x2 <- renderRHandsontable({
      rhandsontable(
        team_gen_def %>%
          rename(
            "Ratio (Normal)" = ratio,
            "Ratio (Crisis Mode)" = ratio_s,
            Role = role
          ) %>%
          mutate_if(is.numeric, as.integer), rowHeaders = FALSE, width = 570, stretchH = "all"
      ) %>%
        hot_cols(colWidths = 100)
    })
  })

    
    output$x1 <- renderRHandsontable({
      rhandsontable(
        team_icu() %>%
          rename(
            "Ratio (Normal)" = ratio,
            "Ratio (Crisis Mode)" = ratio_s,
            Role = role
          ) %>%
          mutate_if(is.numeric, as.integer), rowHeaders = FALSE, width = 570, stretchH = "all"
      ) %>% 
        hot_cols(colWidths = 100) 
      
    })
    
    output$x2 <- renderRHandsontable({
      rhandsontable(
        team_gen() %>% 
          rename(
            "Ratio (Normal)" = ratio,
            "Ratio (Crisis Mode)" = ratio_s,
            Role = role
          ) %>%
          mutate_if(is.numeric, as.integer), rowHeaders = FALSE, width = 570, stretchH = "all"
      ) %>% 
        hot_cols(colWidths = 100) 
      
    })
    
    
    
    # calculations happen here ------
    
    icu_ratio_table <- reactive({
      if(is.null(input$x1))
        return(
          team_icu_def %>%
            mutate(team_type = "ICU") %>% 
            rename(n_bed_per_person = ratio ,
                   n_bed_per_person_crisis = ratio_s) %>% 
            select(team_type, everything())
        )
      
      values$df = hot_to_r(input$x1) %>% 
        mutate(team_type = "ICU") %>% 
        rename(role = Role,
               n_bed_per_person = "Ratio (Normal)" ,
               n_bed_per_person_crisis = "Ratio (Crisis Mode)") %>% 
        select(team_type, everything())
    })
    
    gen_ratio_table <- reactive({
      if(is.null(input$x2))
        return(
          team_gen_def %>%
            mutate(team_type = "General") %>% 
            rename(n_bed_per_person = ratio ,
                   n_bed_per_person_crisis = ratio_s) %>% 
            select(team_type, everything())
        )
      
      values$df = hot_to_r(input$x2) %>% 
        mutate(team_type = "General") %>% 
        rename(role = Role,
               n_bed_per_person = "Ratio (Normal)" ,
               n_bed_per_person_crisis = "Ratio (Crisis Mode)") %>% 
        select(team_type, everything())
    })
    
    # here ------
    ratio_table <- reactive({
      rbind(gen_ratio_table(), icu_ratio_table())
    })
    
    display_table <- reactive({
         chart_data(chime_table(),ratio_table())
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
                 n_bed_per_person_crisis = "Ratio (Crisis Mode)") %>% 
          select(team_type, everything())
        
        finalDF_non_icu <- hot_to_r(input$x2) %>% 
          mutate(team_type = "General") %>% 
          rename(role = Role,
                 n_bed_per_person = "Ratio (Normal)" ,
                 n_bed_per_person_crisis = "Ratio (Crisis Mode)") %>% 
          select(team_type, everything())
        
        
        all_ratio = rbind(finalDF, finalDF_non_icu)
        
        writexl::write_xlsx(all_ratio, path = con)
      }
    )
    
    
}

shinyApp(ui, server)
