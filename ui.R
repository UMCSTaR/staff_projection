library(shiny)
library(tidyverse)
library(rhandsontable)
library(highcharter)
library(shinyjs)
library(plotly)

# Define UI for application that draws a histogram
shinyUI(
  fluidPage(
    includeCSS("styles.css"),
    titlePanel("Project Your Staffing Needs"),
    # Start - Sidebar
    sidebarLayout(
      sidebarPanel(
        width = 4,
        # step1 - Census
        h4("Step 1: Input Projected Census"),
        actionButton(
          "prejected_cesus",
          label = "Input Projected Census",
          width = "100%",
          icon("database"),
          class = "main-button margin-bottom20"
        ),

        # step1 - Advanced Section
        useShinyjs(),
        shinyWidgets::materialSwitch(inputId = "advanced_census_input", label = strong("Advanced"), value = FALSE, status = "success"),
        helpText(id = "advanced_input_help", "Estimate staffing for COVID and non-COVID patients"),
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
        sliderInput("icu_perc", label = "Proportion of all beds allocated to ICU", min = 0, max = 100, post = " %", value = 30),
        sliderInput("capacity_perc", label = "Bed Occupancy", min = 0, max = 100, post = " %", value = 91),
        hr(),

        # step2 - Staff ratio
        h4("Step 2: Edit Staffing Ratios"),
        actionButton(
          "update_gen",
          "Edit Staffing Ratios",
          icon("user-md"),
          width = "100%",
          class = "main-button margin-bottom10"
        ),
        actionButton(
          "update_capacity",
          "Enter Total Employees",
          icon("clipboard-list"),
          width = "100%",
          class = "main-button margin-bottom10"
        ),
        hr(),

        # step3 - Plots
        h4("Step 3: Generate Plots"),
        actionButton(
          "generateButton",
          label = "Generate Plot",
          width = "100%",
          icon("chart-line"),
          class = "main-button margin-bottom20"
        ),
        useShinyjs(),
        shinyWidgets::materialSwitch(inputId = "show_icu_non_icu_plots", label = strong("Show ICU and Non-ICU plots"), value = FALSE, status = "success"),
        hr(),

        # step4 - Download
        h4("Step 4: Download Tables"),
        downloadButton(
          "downloadData_combine_file",
          "Download Combined File",
          class = "main-button",
          style = "width: 100%;"
        )
      ),
      # End sidebar

      # Start mainPanel --------
      mainPanel(
        width = 8,
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
            width = 8,
            div(uiOutput("plot_norm"), class = "plot"),
            div(tableOutput("table_result_normal"), class = "font-size")
          ),
          tabPanel(
            value = "Crisis",
            title = "Stretch",
            div(uiOutput("plot_crisis"), class = "plot"),
            div(tableOutput("table_result_crisis"), class = "font-size")
          ),

          # projected census ------
          tabPanel(
            value = "census",
            title = "Projected Census",
            h4(
              "Option 1: Upload your projected patient census (from ",
              a("CHIME", target = "_blank", href = "https://penn-chime.phl.io/"), "or using our ",
              a("template", href = "data/projected_census_template.csv", target = "_blank", download = "projected_census_template.csv"),
              ")"
            ),
            fileInput(
              "chime_up",
              "Click browse to select file (.csv)",
              multiple = FALSE,
              accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv"
              )
            ),
            hr(),
            h4("Option 2: Input your projected patient census manually"),
            p(strong("Right click"), "in a cell to add and delete row;", "select cell and type the new value.",
              class = "font-size13"
            ),
            actionButton(
              "reset_census",
              "Clear Table",
              icon("table"),
              class = "main-button margin-bottom20"
            ),
            actionButton(
              "default_chime",
              "Reset to Default",
              icon("undo"),
              class = "main-button margin-bottom20"
            ),
            div(rHandsontableOutput("prejected_census"), class = "font-size margin-bottom10"),
            helpText(
              class = "text-margin margin-top10", strong("hospitalized:"), "Number of patients that are hospitalized in", em("Non-ICU"), "units; ", br(),
              strong("icu:"), "Number of patients that are in", em("ICU"), "units"
            )
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
              class = "text-margin",
              strong("Important note:"),
              "These estimates are designed to give a sense of general staffing needs, but your needs may vary based on local conditions."
            ),
            sliderInput("reduction", label = "Expected staffing reduction (due to sickness, quarantine restrictions, or other)", min = 0, max = 100, post = " %", value = 30, width = "600px"),
            p(
              strong("Right click"),
              "in a cell to add and delete row;",
              "select cell and type the new value",
              class = "font-size13 margin-top20 margin-bottom10"
            ),
            actionButton(
              "reset",
              "Clear Table",
              icon("table"),
              class = "main-button margin-bottom10"
            ),
            actionButton(
              "reset_to_ori",
              "Reset to Default",
              icon("undo"),
              class = "main-button margin-bottom10"
            ),
            h4("ICU"),
            div(rHandsontableOutput("x1"), class = "font-size margin-bottom20"),
            h4("Non-ICU"),
            div(rHandsontableOutput("x2"), class = "font-size margin-bottom20"),
            downloadButton(
              "downloadData_all_ratio",
              "Download Staffing Ratios Tables",
              class = "main-button"
            ),
            helpText(
              class = "text-margin margin-top10",
              strong("Role: "),
              "List of possible staff roles",
              br(),
              strong("Ratio (Normal): "),
              "The patient:staff ratio (i.e. how many patients each staff member cares for)",
              br(),
              strong("Ratio (Stretch): "),
              "The patient:staff ratio during a ‘Stretch mode’ (i.e. the maximum number patients each staff member can care for)",
              br(),
              br(),
              strong("*"), em("Default patient-to-staff ratios are based on real staffing ratios at a collaborating academic medical center that has undertaken extensive emergency preparedness work for this pandemic.")
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
              class = "font-size13"
            ),
            actionButton("clear_capacity", "Clear Table", icon("table"),
              class = "main-button margin-bottom20"
            ),
            actionButton("reset_default_capacity", "Reset to Default", icon("undo"),
              class = "main-button margin-bottom20"
            ),
            div(rHandsontableOutput("x3"), class = "font-size"),
          )
        )
      )
    )
  )
)
