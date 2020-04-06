
# Load packages

library(shiny)
library(tidyverse)
library(shinyjs)
library(plotly)
library(readxl)

# Start UI

chime <- read_csv("./data/2020-04-02_projected_census.csv")
team <- read_xlsx("./data/staff_table.xlsx")

ui <- fluidPage(
    
    titlePanel("Projected Staffing Demand"),
    ("This application is intended to work with a"), strong("projected census"), ("file from CHIME (e.g. 2020-04-02_projected_census.csv)"),
    ("along with a"), strong("staffing tables"), ("file (e.g., staff_tabel.xlsx) template."), em("The tools at the top-right of the figure can help you navigate the resulting graph."),

    hr(),
    
    sidebarLayout(
        column(3,
               h4("Inputs"),
               
               fileInput("chime_up", "CHIME (.csv)",
                         multiple = FALSE,
                         accept = c("text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".csv")),
               #hr(),
               fileInput("team_in", "Staffing Ratios (.xlsx)",
                         multiple = FALSE,
                         accept = c(".xlsx")),
               #hr(),
               actionButton("generateButton", label = "Generate Plot"),
               
               hr(),
               
               downloadButton("downloadData", "Download Combined File")
        ),
        
      mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("Crisis", plotlyOutput("plot_crisis")),
                    tabPanel("Normal", plotlyOutput("plot_norm"))

      )
    )
  )
)


# Server
server <- function(input, output) {

  chart_data <- function(chime) {

    # Clean up CHIME 
    chime <- chime %>%
        filter(day >= 0)
    
    # Get rows after clean
    chime_row <- nrow(chime)
    
    # Create four plots: ICU / General; Normal / Crisis
    icu_ratio <- data2() %>% 
        filter(team_type == "ICU") 
    
    icu_ratio_row <- nrow(icu_ratio) + 2
    
    icu_norm <- icu_ratio %>%
        select(role, n_bed_per_person) %>%
        mutate(day = 0,
               team_type = "ICU Normal") %>%
        pivot_wider(., id_cols = c("day", "team_type"), names_from = role, values_from = n_bed_per_person) %>%
        add_row(day = 1:chime_row) %>%
        fill(., 2:all_of(icu_ratio_row))
    
    icu_crisis <- icu_ratio %>%
        select(role, n_bed_per_person_crisis) %>%
        mutate(day = 0,
               team_type = "ICU Crisis") %>%
        pivot_wider(., id_cols = c("day", "team_type"), names_from = role, values_from = n_bed_per_person_crisis) %>%
        add_row(day = 1:chime_row) %>%
        fill(., 2:all_of(icu_ratio_row))
    
    gen_ratio <- data2() %>% 
        filter(team_type == "General") 
    
    gen_ratio_row <- nrow(gen_ratio) + 2
    
    gen_norm <- gen_ratio %>%
        select(role, n_bed_per_person) %>%
        mutate(day = 0,
               team_type = "General Normal") %>%
        pivot_wider(., id_cols = c("day", "team_type"), names_from = role, values_from = n_bed_per_person) %>%
        add_row(day = 1:chime_row) %>%
        fill(., 2:all_of(gen_ratio_row))
    
    gen_crisis <- gen_ratio %>%
        select(role, n_bed_per_person_crisis) %>%
        mutate(day = 0,
               team_type = "General Crisis") %>%
        pivot_wider(., id_cols = c("day", "team_type"), names_from = role, values_from = n_bed_per_person_crisis) %>%
        add_row(day = 1:chime_row) %>%
        fill(., 2:all_of(gen_ratio_row)) 
    
    # Append tables
    lookup <- bind_rows(icu_norm, icu_crisis, gen_norm, gen_crisis)
    
    look_col <- ncol(lookup)
    
    lookup <- lookup %>%
      pivot_longer(., 3:all_of(look_col), names_to = "role", values_to = "n_bed_per_person")  %>%
      filter(!is.na(n_bed_per_person))
    
    # Create visualization data frame
    chime_lookup <- full_join(chime, lookup, by = "day")
    
    chime_lookup <- chime_lookup %>%
      mutate(projected_bed_icu =  icu / n_bed_per_person,
             projected_bed_hosp =  hospitalized / n_bed_per_person,
             projected_bed_per_person = ifelse(team_type == "General Normal" | team_type == "General Crisis",
                                               projected_bed_hosp,
                                               ifelse(team_type == "ICU Normal" | team_type == "ICU Crisis",
                                                      projected_bed_icu, NA)),
             projected_bed_per_person = ifelse(is.infinite(projected_bed_per_person), 0, projected_bed_per_person))
}

    data <- reactiveVal(NULL)

    observe({
        inFile <- input$chime_up
        
        if (is.null(inFile))
            return(NULL)
        
        enable("generateButton")
        
        tmpdf <- read_csv(
            inFile$datapath,
            col_names = T)
        
        data(tmpdf)
    
    })
    
    
    data2 <- reactiveVal(NULL)
    
    observe({
        inFile <- input$team_in
        
        if (is.null(inFile))
            return(NULL)
        
        enable("generateButton")

        tmpdf2 <- read_xlsx(
            inFile$datapath)
        
        data2(tmpdf2)
        
    })
   
    display <- eventReactive(input$generateButton, 
                               chart_data(data())) 
  
    output$plot_crisis <- renderPlotly({

    p <-  ggplot(display() %>%
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
      
      p <-  ggplot(display() %>%
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
    
    output$downloadData <- downloadHandler( 
      filename = function(){
        paste("chime_ratio_combined", ".csv", sep="")
      }, 
      
      content = function(file) {
        write.csv(display(), file, row.names = FALSE)
      })
}

shinyApp(ui, server)
