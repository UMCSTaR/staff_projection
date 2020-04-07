
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
      #hr(),
      fileInput(
        "team_in",
        "Staffing Ratios (.xlsx)",
        multiple = FALSE,
        accept = c(".xlsx")
      ),
      #hr(),
      actionButton("generateButton", label = "Generate Plot"),
      
      hr(),
      
      downloadButton("downloadData", "Download Combined File")
    ),
    
    mainPanel(tabsetPanel(
      type = "tabs",
      tabPanel("Crisis", plotlyOutput("plot_crisis")),
      tabPanel("Normal", plotlyOutput("plot_norm")),
      
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
        
        p(
          strong("Right click"),
          "in a cell to add and delete row;",
          "select cell and type the new value",
          style = "font-size:16px"
        ),
        
        h4("ICU"),
        
        div(rHandsontableOutput("x1"), style = "font-size: 120%"),
        
        br(),
        
        h4("Non-ICU"),
        
        div(rHandsontableOutput("x2"), style = "font-size: 120%"),
        
        br(),
        
        
        downloadButton("downloadData_all_ratio", "Download Staffing Ratios",
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
server <- function(input, output) {
  
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
    
    display_table <- reactive({
      values$df = hot_to_r(input$x1) %>% 
        mutate(team_type = "General") %>% 
        rename(role = Role,
               n_bed_per_person = "Ratio (Normal)" ,
               n_bed_per_person_crisis = "Ratio (Crisis Mode)") %>% 
        select(team_type, everything())
      
      values$df_gen = hot_to_r(input$x2) %>% 
        mutate(team_type = "ICU") %>% 
        rename(role = Role,
               n_bed_per_person = "Ratio (Normal)" ,
               n_bed_per_person_crisis = "Ratio (Crisis Mode)") %>% 
        select(team_type, everything())
      
      rbind(values$df, values$df_gen) %>% 
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
        facet_wrap(~ team_type, scales = "free", ncol = 4)
      
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
          facet_wrap(~ team_type, scales = "free", ncol = 4)

        fig <- ggplotly(p)

        fig

      })
    
    
}

shinyApp(ui, server)
