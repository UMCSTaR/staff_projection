library(shiny)
library(tidyverse)
library(rhandsontable)
library(shinyjs)
library(plotly)

# Define UI for application that draws a histogram
shinyUI(
    fluidPage(
        titlePanel("Projected Staffing Demand"),
        ("This application is intended to work with a"),
        strong("projected census"),
        ("file from CHIME (e.g. 2020-04-02_projected_census.csv)"),
        ("along with a"),
        strong("staffing tables"),
        (
            "file (e.g., Staffing_role_and_ratio2020-04-02.xlsx) template or your edited staffing ratios."
        ),
        em(
            "The tools at the top-right of the figure can help you navigate the resulting graph."
        ),
        
        hr(),
        
        # sidebar -------
        sidebarLayout(
            sidebarPanel(width = 3,
                         
                         h4("Input Projected Census"),
                         
                         p(strong("Option1: Upload projected census from CHIME")),
                         
                         actionButton(
                           "prejected_cesus",
                           label = "Input Projected Census",
                           icon("database"),
                           style = "color: #fff; background-color: #228B22; border-color: #2e6da4"
                         ),
                         
                         br(),
                         br(),
                         
                         
                         
                         p(strong("Option2: Input your own projected census")),
                         
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
                         
                         hr(),
                         
                         p("Download CSV files that produce the plots."),
                         
                         downloadButton("downloadData_combine_file", "Download Combined File",
                                        style = "color: #fff; background-color: #228B22; border-color: #2e6da4")
            ),
            
            # mainPanel --------
            mainPanel(
                
                h3("Project Your Staffing Needs"),
                
                tags$style(HTML("
        .tabbable > .nav > li[class=active]    > a[data-value='Normal'] {background-color: #9dc183; color:black}
        .tabbable > .nav > li[class=active]    > a[data-value='Crisis'] {background-color: #8D021F; color:white}
  ")),
                
                tabsetPanel(
                    id = "inTabset",
                    
                    type = "tabs",
                    
                    # plot tabs
                    tabPanel("Normal", 
                             br(),
                             plotlyOutput("plot_norm")
                    ),
                    tabPanel("Crisis", 
                             br(),
                             plotlyOutput("plot_crisis")),
                    
                    
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
                        
                        # downloadButton("downloadData_icu_ratio", "Download ICU Staffing Ratios",
                        #                style = "color: #fff; background-color: #228B22; border-color: #2e6da4"),
                        # 
                        
                        h4("Non-ICU"),
                        
                        div(rHandsontableOutput("x2"), style = "font-size: 120%"),
                        
                        br(),
                        
                        
                        # downloadButton("downloadData_non_icu_ratio", "Download Non-ICU Staffing Ratios",
                        #               style = "color: #fff; background-color: #228B22; border-color: #2e6da4"),
                        # 
                        
                        downloadButton("downloadData_all_ratio", "Download Staffing Ratios Tables",
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
                        )
                    ),
                    
                    # prejected census ------
                    tabPanel(value = "census",
                             title = "Projected Census",

                             p(strong("Right click"), "in a cell to add and delete row;", "select cell and type the new value",
                               style = "font-size:16px"),
                            
                             
                             actionButton("reset_census", "Clear Table", icon("table"),
                                          style = "color: #fff; background-color: #228B22; border-color: #2e6da4"),
                             
                             actionButton(
                               "generateButton_2",
                               label = "Generate Plot",
                               icon("chart-line"),
                               style = "color: #fff; background-color: #228B22; border-color: #2e6da4"
                             ),
                             
                             br(),
                             br(),
                             
                             rHandsontableOutput("prejected_census"),
                             
                             br(),
                             
                             helpText(strong("hospitalized"), ": n of patients that are hospitalized in", em("Non-ICU"), "units; ",
                                      strong("icu"), ": n of patients that are in", em("ICU"), "units")
                             
                             
                             
                    )
                    
                ))
        )
    )
    )
