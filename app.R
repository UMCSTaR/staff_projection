
library(shiny)
library(tidyverse)
library(shinyjs)
library(plotly)

ui <- fluidPage(
    
    titlePanel("Projected Staffing Demand"),
    
    h4("Inputs: CHIME-based projections and staffing ratios"),
    p("Before using this tool, be sure to work through the CHIME application. Once you've completed your projections,
      download and modify the staffing ratios. You can uses this tool, to run simulations <>."),
    
    strong("Plot 1:"), ("ICU Normal,"), strong("Plot 2:"), ("ICU Crisis,"), 
    strong("Plot 3:"), ("Non-ICU," ), strong("Plot 4:"), ("Non-ICU Crisis,"),
    
    #textOutput("b"),
    
    plotlyOutput("plotY"),
    
    fluidRow(
        column(3,
               h4("Inputs"),

               fileInput("chime_up", "CHIME (.csv)",
                         multiple = FALSE,
                         accept = c("text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".csv")),
               
               hr(),
               
               fileInput("team_in", "Staffing Ratios (.csv)",
                         multiple = FALSE,
                         accept = c("text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".csv")),
               
               hr(),
               
               actionButton("generateButton", label = "Generate Plot")
        )
    )

)

# Server
server <- function(input, output) {

    #team <- input$team_in
    
    #team <- read_csv(
    #    team$datapath,
    #    col_names = T)    
    
chart_data <- function(chime) {
    
    #team <- read_rds("./data/team_ratio.rds") # pull from modifiable data
    
    # Clean up CHIME 
    chime <- chime %>%
        filter(day >= 0)
    
    # Get rows after clean
    chime_row <- nrow(chime)
    
    # Create four plots: ICU / General; Normal / Stretch
    
    icu_ratio <- data2() %>% 
        filter(team_type == "ICU") 
    
    icu_norm <- icu_ratio %>%
        select(role, n_bed_per_person) %>%
        mutate(day = 0,
               team_type = "1 ICU Normal") %>%
        pivot_wider(., id_cols = c("day", "team_type"), names_from = role, values_from = n_bed_per_person) %>%
        add_row(day = 1:chime_row) %>%
        fill(., 2:12)
    
    icu_stretch <- icu_ratio %>%
        select(role, n_bed_per_person_stretch) %>%
        mutate(day = 0,
               team_type = "2 ICU Stretch") %>%
        pivot_wider(., id_cols = c("day", "team_type"), names_from = role, values_from = n_bed_per_person_stretch) %>%
        add_row(day = 1:chime_row) %>%
        fill(., 2:12)
    
    gen_ratio <- data2() %>% 
        filter(team_type == "General") 
    
    gen_norm <- gen_ratio %>%
        select(role, n_bed_per_person) %>%
        mutate(day = 0,
               team_type = "3 General Normal") %>%
        pivot_wider(., id_cols = c("day", "team_type"), names_from = role, values_from = n_bed_per_person) %>%
        add_row(day = 1:chime_row) %>%
        fill(., 2:10)
    
    gen_stretch <- gen_ratio %>%
        select(role, n_bed_per_person_stretch) %>%
        mutate(day = 0,
               team_type = "4 General Stretch") %>%
        pivot_wider(., id_cols = c("day", "team_type"), names_from = role, values_from = n_bed_per_person_stretch) %>%
        add_row(day = 1:chime_row) %>%
        fill(., 2:10) 
    
    # Append tables
    
    lookup <- bind_rows(icu_norm, icu_stretch, gen_norm, gen_stretch)
    
    lookup <- lookup %>%
        pivot_longer(., 3:14, names_to = "role", values_to = "n_bed_per_person")  %>%
        filter(!is.na(n_bed_per_person))
    
    # Create visualization data frame
    
    chime_lookup <- full_join(chime, lookup, by = "day")
    
    chime_lookup <- chime_lookup %>%
        mutate(projected_bed_per_person =  icu / n_bed_per_person )
    
    chime_lookup <- chime_lookup %>%
        filter(!is.infinite(projected_bed_per_person)) %>%
        group_by(team_type, day) %>%
        mutate(rnk = rank(n_bed_per_person, ties.method = "first")) %>%
        ungroup() %>%
        mutate(ord_new = fct_reorder2(role, rnk, projected_bed_per_person))
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
        
        tmpdf2 <- read_csv(
            inFile$datapath,
            col_names = T,
            local = locale(encoding = "latin1"))
        
        data2(tmpdf2)
        
    })
   
    display <- eventReactive(input$generateButton, 
                               chart_data(data())) 
    
    output$plot <- renderPlot({
        
    ggplot(display(), aes(x = date, y = projected_bed_per_person, colour = ord_new, group = role)) +
        geom_line() +
        labs(title = "Projected Staffing Needs",
             x = "Date",
             y = "Projected staff",
             colour = "Roles",
             caption = "Estimates from CHIME and user-inputted ratios") +
        theme_bw() +
        facet_wrap(~ team_type, scales = "free")
    
    })    
        
    output$plotY <- renderPlotly({
        
        display() %>% 
            split(display()$team_type) %>% 
            lapply(function(x) {
                plot_ly(data = x, 
                        x = ~date, 
                        y = ~projected_bed_per_person, 
                        type= "scatter", 
                        mode = 'lines',
                        color=~ord_new)
            }) %>% 
            
            subplot(margin = .05)

        })
        
  #  output$b <- renderText({
  #      unique(icu_norm$team_type)
  #  })
    
}

# Create Shiny app ----
shinyApp(ui, server)
