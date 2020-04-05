
# Load packages

library(shiny)
library(tidyverse)
library(shinyjs)
library(plotly)
library(readxl)

# Start UI

ui <- fluidPage(
    
    titlePanel("Projected Staffing Demand"),
    
    h4("Inputs: CHIME-based projections and staffing ratios"),
    ("This application only works with two specific file types. The first is the"), strong("projected census"), ("file that you downloaded from CHIME (e.g. 2020-04-02_projected_census.csv)"),
    ("The second the"), strong("staffing tables"), ("file (e.g., staff_tabel.xlsx), which you can download from above."),
    
    plotlyOutput("plot"),
    
    fluidRow(
        column(3,
               h4("Inputs"),

               fileInput("chime_up", "CHIME (.csv)",
                         multiple = FALSE,
                         accept = c("text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".csv")),
               
               hr(),
               
               fileInput("team_in", "Staffing Ratios (.xlsx)",
                         multiple = FALSE,
                         accept = c(".xlsx")),
               
               hr(),
               
               actionButton("generateButton", label = "Generate Plot")
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
    
    icu_norm <- icu_ratio %>%
        select(role, n_bed_per_person) %>%
        mutate(day = 0,
               team_type = "ICU Normal") %>%
        pivot_wider(., id_cols = c("day", "team_type"), names_from = role, values_from = n_bed_per_person) %>%
        add_row(day = 1:chime_row) %>%
        fill(., 2:12)
    
    icu_crisis <- icu_ratio %>%
        select(role, n_bed_per_person_crisis) %>%
        mutate(day = 0,
               team_type = "ICU Crisis") %>%
        pivot_wider(., id_cols = c("day", "team_type"), names_from = role, values_from = n_bed_per_person_crisis) %>%
        add_row(day = 1:chime_row) %>%
        fill(., 2:12)
    
    gen_ratio <- data2() %>% 
        filter(team_type == "General") 
    
    gen_norm <- gen_ratio %>%
        select(role, n_bed_per_person) %>%
        mutate(day = 0,
               team_type = "General Normal") %>%
        pivot_wider(., id_cols = c("day", "team_type"), names_from = role, values_from = n_bed_per_person) %>%
        add_row(day = 1:chime_row) %>%
        fill(., 2:10)
    
    gen_crisis <- gen_ratio %>%
        select(role, n_bed_per_person_crisis) %>%
        mutate(day = 0,
               team_type = "General Crisis") %>%
        pivot_wider(., id_cols = c("day", "team_type"), names_from = role, values_from = n_bed_per_person_crisis) %>%
        add_row(day = 1:chime_row) %>%
        fill(., 2:10) 
    
    # Append tables
    
    lookup <- bind_rows(icu_norm, icu_crisis, gen_norm, gen_crisis)
    
    lookup <- lookup %>%
        pivot_longer(., 3:14, names_to = "role", values_to = "n_bed_per_person")  %>%
        filter(!is.na(n_bed_per_person))
    
    # Create visualization data frame
    
    chime_lookup <- full_join(chime, lookup, by = "day")
    
    chime_lookup <- chime_lookup %>%
        mutate(projected_bed_per_person =  icu / n_bed_per_person )
    
    chime_lookup <- chime_lookup %>%
        filter(!is.infinite(projected_bed_per_person)) # %>%
        #group_by(team_type, day) %>%
        #mutate(rnk = rank(n_bed_per_person, ties.method = "first")) %>%
        #ungroup() %>%
        #mutate(ord_new = fct_reorder2(role, rnk, projected_bed_per_person))
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
        
        #tmpdf2 <- read_csv(
        #    inFile$datapath,
        #    col_names = T,
        #    local = locale(encoding = "latin1"))
        
        
        tmpdf2 <- read_xlsx(
            inFile$datapath)
        
        data2(tmpdf2)
        
    })
   
    display <- eventReactive(input$generateButton, 
                               chart_data(data())) 
    
    output$plot <- renderPlotly({
        
    p <-  ggplot(display(), aes(x = date, y = projected_bed_per_person, colour = role, group = role)) +
        geom_line() +
        labs(title = "Projected Staffing Needs",
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
