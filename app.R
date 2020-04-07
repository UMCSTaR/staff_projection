
# Load packages

library(shiny)
library(tidyverse)
library(rhandsontable)
library(shinyjs)
library(plotly)
library(readxl)

# read data -------
chime <- read_csv("./data/2020-04-02_projected_census.csv")
team <- read_xlsx("./data/staff_table.xlsx")


team_icu_def = team %>%
  filter(team_type == "ICU") %>%
  transmute(role, ratio = n_bed_per_person, ratio_s = n_bed_per_person_crisis)

team_gen_def = team %>%
  filter(team_type == "General") %>%
  transmute(role, ratio = n_bed_per_person, ratio_s = n_bed_per_person_crisis)


# data procssing for plot
source("function/chart_data.R")


# UI ----------------
ui <- fluidPage(
  titlePanel("Projected Staffing Demand"),
  ("This application is intended to work with a"),
  strong("projected census"),
  ("file from CHIME (e.g. 2020-04-02_projected_census.csv)"),
  ("along with a"),
  strong("staffing tables"),
  (
    "file (e.g., Staffing_role_and_ratio2020-04-02.xlsx) template."
  ),
  em(
    "The tools at the top-right of the figure can help you navigate the resulting graph."
  ),
  
  hr(),
  
  sidebarLayout(
    

    column(
      3,

      h4("Inputs"),
      
      fileInput(
        "chime_up",
        "CHIME (.csv)",
        multiple = FALSE,
        accept = c("text/csv",
                   "text/comma-separated-values,text/plain",
                   ".csv")
      ),
      
      hr(),
      
      h4("Staffing Ratio"),
      
      p(strong("Option1: Edit Staffing Ratios")),
      actionButton("update_gen", "Edit Staffing Ratios",
                   icon("user-md"),
                   style = "color: #fff; background-color: #228B22; border-color: #2e6da4"),
      
      
      br(),
      br(),

      p(strong("Option2: Upload Staffing Ratios File")),
      
      fileInput(
        "team_in",
        "Staffing Ratios (.xlsx)",
        multiple = FALSE,
        accept = c(".xlsx")
      ),
      #hr(),
      # actionButton("generateButton", label = "Generate Plot"),
      
      # hr(),
      
      # downloadButton("downloadData", "Download Combined File")
    ),
    
    mainPanel(
      
      
      tags$style(HTML("
        .tabbable > .nav > li[class=active]    > a[data-value='Normal'] {background-color: #9dc183; color:black}
        .tabbable > .nav > li[class=active]    > a[data-value='Crisis'] {background-color: #8D021F; color:white}
  ")),
      
      tabsetPanel(
        id = "inTabset",
        
        type = "tabs",
        
        # plot tabs
        tabPanel("Normal", plotlyOutput("plot_norm")),
        tabPanel("Crisis", plotlyOutput("plot_crisis")),
        
        
        # editable tables -------
        tabPanel(
          value = "edit_ratio_table",
          title = "Patient-to-Staff Ratios",
          br(),
          
          helpText(
            strong("Important note:"),
            "These estimates are designed to give a sense of general staffing needs, but your needs may vary based on local conditions."
          ),
          
          
          actionButton("reset", "Clear Table", icon("table"),
                       style = "color: #fff; background-color: #228B22; border-color: #2e6da4"),
          actionButton("reset_to_ori", "Reset to Default", icon("undo"),
                       style = "color: #fff; background-color: #228B22; border-color: #2e6da4"),
          br(),
          br(),
          actionButton(
            "generateButton",
            label = "Generate Plot",
            icon("chart-line"),
            style = "color: #fff; background-color: #228B22; border-color: #2e6da4"
          ),
          
          
          
          
          br(),
          br(),
          
          p(
            strong("Right click"),
            "in a cell to add and delete row;",
            "select cell and type the new value",
            style = "font-size:16px"
          ),
          
          h4("ICU"),
          
          div(rHandsontableOutput("x1"), style = "font-size: 120%"),
          
          br(),
          
          downloadButton("downloadData_icu_ratio", "Download ICU Staffing Ratios",
                         style = "color: #fff; background-color: #228B22; border-color: #2e6da4"),
          
          
          h4("Non-ICU"),
          
          div(rHandsontableOutput("x2"), style = "font-size: 120%"),
          
          br(),
          
          
          downloadButton("downloadData_non_icu_ratio", "Download Non-ICU Staffing Ratios",
                        style = "color: #fff; background-color: #228B22; border-color: #2e6da4"),
          
           
          
          column(
            12,
            br(),
            tags$div(
              "Role: List of possible staff roles",
              tags$br(),
              "Ratio (normal) = the patient:staff ratio (i.e. how many patients each staff member cares for)",
              tags$br(),
              "Ratio (Crisis Mode) = the patient:staff ratio during a ‘crisis mode’ (ie. the maximum number patients each staff member can care for)",
              tags$br(),
              tags$br(),
              tags$br(),
              "* Default patient-to-staff ratios are based on real staffing ratios at a collaborating academic medical center that has undertaken extensive emergency preparedness work for this pandemic.",
              br(),
              br()
            )
          ),
        )
        
      ))
  )
)



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
      
      
    display_table <- reactive({
      rbind(gen_ratio_table(), icu_ratio_table()) %>% 
        chart_data(chime = chime_table())
    })
    
    # plots -------

    
    output$plot_crisis <- renderPlotly({
      p <-  ggplot(display_table() %>%
                     filter(team_type == "ICU Crisis" | team_type == "General Crisis"), 
                   aes(x = date, y = projected_bed_per_person, colour = role, group = role)) +
        geom_line() +
        labs(title = "Projected Staffing Needs: Crisis",
             x = "Date",
             y = "Projected staff",
             colour = "Roles",
             caption = "Estimates from CHIME and user-inputted ratios") +
        theme_bw() +
        # facet_wrap(~ team_type, scales = "free", ncol = 4)
        facet_wrap(~ team_type, ncol = 4) +
        scale_y_continuous(trans = "log", breaks = c(50,100,200, 300))
      
      fig <- ggplotly(p)
      
      fig
      
    })
    
    
    output$plot_norm <- renderPlotly({
        p <-  ggplot(display_table() %>%
                       filter(team_type == "ICU Normal" | team_type == "General Normal"),
                     aes(x = date, y = projected_bed_per_person, colour = role, group = role)) +
          geom_line() +
          labs(title = "Projected Staffing Needs: Normal",
               x = "Date",
               y = "Projected staff",
               colour = "Roles",
               caption = "Estimates from CHIME and user-inputted ratios") +
          theme_bw() +
          # facet_wrap(~ team_type, scales = "free", ncol = 4)
          facet_wrap(~ team_type, ncol = 4) +
          scale_y_continuous(trans = "log", , breaks = c(5, 10, 50,100,200, 300))
        

        fig <- ggplotly(p)

        fig

      })
    
    # buttons ------
    observeEvent(input$generateButton, {
      updateTabsetPanel(session, "inTabset", selected = "Normal")
    })
    
    observeEvent(input$update_gen, {
      updateTabsetPanel(session, "inTabset", selected = "edit_ratio_table")
    })
    
    
   
    output$downloadData_icu_ratio <- downloadHandler(
      filename = function() {
        paste('ICU_Staffing_role_and_ratio', Sys.Date(), '.csv', sep='')
      },
      content = function(con) {
        finalDF <- hot_to_r(input$x1)
        write.csv(finalDF, con)
      }
    )
    
    output$downloadData_norm <- downloadHandler(
      filename = function() {
        paste('staffing_normal', Sys.Date(), '.csv', sep='')
      },
      content = function(con) {
        finalDF <- hot_to_r(input$x2)
        write.csv(norm_staff_table(), con)
      }
    )
    
    
}

shinyApp(ui, server)
