
library(shiny)
library(tidyverse)

# Define UI for data upload app ----
ui <- fluidPage(
    
    # App title ----
    titlePanel("Upload CHIME file"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            
            # Input: Select a file ----
            fileInput("uploadedCSV", "CSV File",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            # Horizontal line ----
            tags$hr(),
            
            # Input: Checkbox if file has header ----
            actionButton("generateButton", label = "Generate Plot"),
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("plot")
            
        )
        
    )
)

# Define server logic to read selected file ----
server <- function(input, output) {

    
    
chart_data <- function(chime) {
    
    team <- read_rds("./data/team_ratio.rds") # pull from modifiable data
    
    # Clean up CHIME 
    chime <- chime %>%
        filter(day >= 0)
    
    # Get rows after clean
    chime_row <- nrow(chime)
    
    # Create four plots: ICU / General; Normal / Stretch
    
    icu_ratio <- team %>% 
        filter(team_tpye == "ICU") 
    
    icu_norm <- icu_ratio %>%
        select(role, n_bed_per_person) %>%
        mutate(day = 0,
               team_type = "ICU Normal") %>%
        pivot_wider(., id_cols = c("day", "team_type"), names_from = role, values_from = n_bed_per_person) %>%
        add_row(day = 1:chime_row) %>%
        fill(., 2:12)
    
    icu_stretch <- icu_ratio %>%
        select(role, n_bed_per_person_stretch) %>%
        mutate(day = 0,
               team_type = "ICU Stretch") %>%
        pivot_wider(., id_cols = c("day", "team_type"), names_from = role, values_from = n_bed_per_person_stretch) %>%
        add_row(day = 1:chime_row) %>%
        fill(., 2:12)
    
    gen_ratio <- team %>% 
        filter(team_tpye == "General") 
    
    gen_norm <- gen_ratio %>%
        select(role, n_bed_per_person) %>%
        mutate(day = 0,
               team_type = "General Normal") %>%
        pivot_wider(., id_cols = c("day", "team_type"), names_from = role, values_from = n_bed_per_person) %>%
        add_row(day = 1:chime_row) %>%
        fill(., 2:10)
    
    gen_stretch <- gen_ratio %>%
        select(role, n_bed_per_person_stretch) %>%
        mutate(day = 0,
               team_type = "General Stretch") %>%
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
    
}

data <- reactiveVal(NULL)

    observe({
        inFile <- input$uploadedCSV
        
        if (is.null(inFile))
            return(NULL)
        
        enable("generateButton")
        
        tmpdf <- read_csv(
            inFile$datapath,
            col_names = T)
        
        data(tmpdf)
    
    })
   
    display <- eventReactive(input$generateButton, 
                               chart_data(data())) 
    
    output$plot <- renderPlot({
        
        ggplot(display(), aes(x = date, y = projected_bed_per_person, colour = role, group = role)) +
        geom_line() +
        labs(title = "Projected Staffing Needs",
             x = "Date",
             y = "Projected staff",
             colour = "Roles",
             caption = "Estimates from CHIME and user-inputted ratios") +
        theme_bw() +
        facet_wrap(~ team_type, scales = "free")
        
    })
}

# Create Shiny app ----
shinyApp(ui, server)

