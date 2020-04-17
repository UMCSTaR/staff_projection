library(shiny)
library(tidyverse)
library(rhandsontable)
library(highcharter)
library(shinyjs)
library(plotly)

# Define UI for application that draws a histogram
shinyUI(
    fluidPage(
        
        # sidebar -------
        sidebarLayout(
            sidebarPanel(width = 3,
                         # step1---------
                         h4("Step 1: Input Projected Census"),
                         
                         actionButton(
                           "prejected_cesus",
                           label = "Input Projected Census Data",
                           width = "240px",
                           icon("database"),
                           style = "color: #fff; background-color: #228B22; border-color: #2e6da4"
                         ),
                         
                         br(),
                         br(),
                         
                         useShinyjs(),
                         shinyWidgets::materialSwitch(inputId="advanced_census_input", label = strong("Advanced"), value = FALSE, status = "success"),
                         helpText(id = "advanced_input_help" ,"Estimate staffing for COVID and non-COVID patients"),
                         
                         numericInput(
                             "total_bed",
                             "Total number of beds",
                             1000,
                             min = 0,
                             max = 1000,
                             step = 10,
                             width = "70%"
                         ),
                         
                         shinyWidgets::setSliderColor("#404040", c(3)),
                         
                         sliderInput("icu_perc", label="Proportion of all beds allocated to ICU", min = 0, max = 100, post  = " %", value = 30),
                         
                         sliderInput("capacity_perc",label="Bed Occupancy", min = 0, max = 100, post  = " %", value = 91),
                         
                         
                         # staff ratio
                         hr(),
                         
                         h4("Step 2: Edit Staffing Ratios"),
                         
                         actionButton("update_gen", "Edit Staffing Ratios",
                                      icon("user-md"),
                                      width = "240px",
                                      style = "color: #fff; background-color: #228B22; border-color: #2e6da4"),
                         
                         br(),
                         br(),
                         
                         actionButton("update_capacity", "Enter Your Total Employees",
                                      icon("clipboard-list"),
                                      width = "240px",
                                      style = "color: #fff; background-color: #228B22; border-color: #2e6da4"),
                         
                         
                         br(),

                         
                         hr(),
                         h4("Step 3: Generate Plots"),
                      
                         actionButton(
                           "generateButton",
                           label = "Generate Plot",
                           width = "240px",
                           icon("chart-line"),
                           style = "color: #fff; background-color: #228B22; border-color: #2e6da4"
                         ),
                         br(),
                         br(),
                         useShinyjs(),
                         shinyWidgets::materialSwitch(inputId="show_icu_non_icu_plots", label = strong("Show ICU and Non-ICU plots"), value = FALSE, status = "success"),

                         
                         hr(),
                         
                         h4("Step 4: Download Tables"),
                         
                         downloadButton("downloadData_combine_file", "Download Combined File",
                                        width = "240px",
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
                    
                    # tabPanel("test", tableOutput("test")),

                    # plots ------
                    tabPanel(
                        "Normal",
                        column(width = 10, 
                               div(uiOutput("plot_norm"), style = "height:450px")),
                        
                        div(tableOutput("table_result_normal"), style = "font-size:120%")
                    ), 
                    tabPanel("Crisis", 
                             column(width = 10,
                                    div(uiOutput("plot_crisis"), style = "height:450px")),
                             
                             div(tableOutput("table_result_crisis"), style = "font-size:120%")
                    ),
                    
                    
                    # projected census ------
                    tabPanel(value = "census",
                             title = "Projected Census",
                             
                             h4("Option 1: Upload your projected patient census (from ",
                                a("CHIME", target="_blank", href= "https://penn-chime.phl.io/"), "or using our ", 
                                a("template", href='data/projected_census_template.csv',target="_blank", download = 'projected_census_template.csv'),
                                ")"
                              ),
                             

                             fileInput(
                               "chime_up",
                               "Click browse to select file (.csv)",
                               multiple = FALSE,
                               accept = c("text/csv",
                                          "text/comma-separated-values,text/plain",
                                          ".csv")
                             ),
                             
                             hr(),
                             
                             h4("Option 2: Input your projected patient census manually"),
                             
                             p(strong("Right click"), "in a cell to add and delete row;", "select cell and type the new value",
                               style = "font-size:13px"),
                             
                             actionButton("reset_census", "Clear Table", icon("table"),
                                          style = "color: #fff; background-color: #228B22; border-color: #2e6da4"),
                             
                             actionButton("default_chime", "Reset to Default", icon("undo"),
                                          style = "color: #fff; background-color: #228B22; border-color: #2e6da4"),
                             
                             br(),
                             br(),
                             
                             rHandsontableOutput("prejected_census"),
                             
                             br(),
                             
                             helpText(strong("hospitalized:"), "Number of patients that are hospitalized in", em("Non-ICU"), "units; ", br(),
                                      strong("icu:"), "Number of patients that are in", em("ICU"), "units")
                             
                             
                             
                    ),
                    
                    # editable tables -------
                    tabPanel(
                      value = "edit_ratio_table",
                      title = "Patient-to-Staff Ratios",
                      
                     # h4("Option 1: Upload Staffing Ratios File (using our", 
                     #     a("template", href='data/Staffing_role_and_ratio_template.xlsx',target="_blank", download = 'Staffing_role_and_ratio_template.xlsx'),
                     #     ")"),
                     #  
                     #  fileInput(
                     #    "team_in",
                     #    "Click browse to select file (.xlsx)",
                     #    multiple = FALSE,
                     #    accept = c(".xlsx")
                     #  ),
                      
                     # hr(),
                     
                      h4("Option 1: Edit Staffing Ratio Table Below"),
                      
                      
                      helpText(
                        strong("Important note:"),
                        "These estimates are designed to give a sense of general staffing needs, but your needs may vary based on local conditions."
                      ),

                      sliderInput("reduction",label="Expected staffing reduction (due to sickness, quarantine restrictions, or other)", min = 0, max = 100, post  = " %", value = 30, width = "600px"),
                      
                     br(),
                     
                     p(
                       strong("Right click"),
                       "in a cell to add and delete row;",
                       "select cell and type the new value",
                       style = "font-size:13px"
                     ),
                     
                      actionButton("reset", "Clear Table", icon("table"),
                                   style = "color: #fff; background-color: #228B22; border-color: #2e6da4"),
                      actionButton("reset_to_ori", "Reset to Default", icon("undo"),
                                   style = "color: #fff; background-color: #228B22; border-color: #2e6da4"),
                      
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
                        helpText(
                          strong("Role: "),
                           "List of possible staff roles",
                           br(),
                           strong("Ratio (Normal): "),
                           "The patient:staff ratio (i.e. how many patients each staff member cares for)",
                           br(),
                           strong("Ratio (Crisis): "),
                           "The patient:staff ratio during a ‘crisis mode’ (i.e. the maximum number patients each staff member can care for)",
                           br(),
                           br(),
                           strong("*"), em("Default patient-to-staff ratios are based on real staffing ratios at a collaborating academic medical center that has undertaken extensive emergency preparedness work for this pandemic.")
                        )
                      )
                    ),
                    
                    # Capacity tab UI code here
                    tabPanel(
                      value = "capacity_table",
                      title = "Total Employees",
                      h4("Option 1: Edit Staff Capacity Table Below"),
                      
                      p(
                        strong("Right click"),
                        "in a cell to add and delete row;",
                        "select cell and type the new value",
                        style = "font-size:13px"
                      ),

                      actionButton("clear_capacity", "Clear Table", icon("table"),
                                   style = "color: #fff; background-color: #228B22; border-color: #2e6da4"),
                      actionButton("reset_default_capacity", "Reset to Default", icon("undo"),
                                   style = "color: #fff; background-color: #228B22; border-color: #2e6da4"),
                      
                      br(),
                      br(),
                      
                      div(rHandsontableOutput("x3"), style = "font-size: 120%"),
                    )
                ))
        )
    )
    )

