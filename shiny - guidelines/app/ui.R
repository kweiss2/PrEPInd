library(shiny)
library(shinydashboard)
library(wesanderson)
library(plotly)
#source("fx.R")

shinyUI(dashboardPage(
    dashboardHeader(title = "PrEP Indications"),
    dashboardSidebar(
        width = 200,
        sidebarMenu(
            menuItem("Introduction", tabName = "Introduction", icon = icon("book")),
            menuItem("Estimation Tool", tabName = "Model", icon = icon("line-chart"))
        )


        ), # End dashboardSidebar
    dashboardBody(
        tabItems(

            ## Introduction tab ##
            tabItem(tabName = "Introduction",
                    column(10, offset = 1,
                           h3("Estimates of Persons with Indications for Preexposure Prophylaxis",
                              style = "color: #193556;"),
                           h4("A Web-Based Modeling Tool for Public Health Practice", style = "color: #2E619E;"),
                           hr(),
                           p("This software tool provides additional opportunities to explore the
                             estimates from the paper:",
                             tags$blockquote("Smith DK, Van Handel M, Grey J",
                                             "Estimates of Persons with Indications for Preexposure Prophylaxis by Jurisdiction Transmission Risk Group, and Race/Ethnicity, United States, 2015.", em("In Clearance."), "2017."),
                            "This webtool provides users the ability to estimate the number of individuals indicated for PrEP."),
                           p("To get started, enter an estimate of the MSM population size in your jurisdiction of interest, and select a jurisdiction on which to base the model assumptions. By changing the jurisdiction, you can see how the assumptions about the model change."),
                           tags$ul(
                               tags$li(strong("Estimates by transmission risk group:"), "this model will produce estimates of persons with indications for preexposure prophylaxis among men who have sex with men (MSM), heterosexuals (HET), and persons who inject drugs (PWID)."),
                               tags$li(strong("Estimates by race and transmission risk group:"), "this model will also produce race-stratified estimates of persons with indications for preexposure prophylaxis among men who have sex with men (MSM), heterosexuals (HET), and persons who inject drugs (PWID).")
                               ),
                           p("After selecting the parameters set in each model, the model will
                             automatically update the values and summary data tables. These data should be interpreted with caution"),
                           hr(),
                           p(em("We acknowledge support from the CDC/NCHHSTP Epidemiological and Economic Modeling Agreement (5U38PS004646).
                                The findings and conclusions used to build this tool are solely the responsibility of the authors and do not
                                necessarily represent the official views of the Centers for Disease Control and Prevention or the Department
                                of Health and Human Services."))
                           )

                           ),
        tabItem(
            ## Model Scenarios Tab ##
            tabName = "Model",
            tags$head(
                tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
                tags$script(type = "text/javascript", src = "busy.js")
            ),

            div(class = "busy",
                img(src = "ajax-loader.gif")
            ),

            fluidRow(
                column(width = 3,
                box(width = NULL,
                    title = "1. Population Size and Jurisdiction", status = "primary", solidHeader = TRUE,
                    numericInput("msmpopsize",
                                 "Jurisdiction MSM Population Size",
                                 min = 0,
                                 value = NA),
                    selectInput("jurisdiction", label = "1. Jurisdiction Assumptions",
                                c("Total" = 1, "Alabama" = 2,
                                  "Alaska" = 3, "Arizona" = 4,
                                  "Arkansas" = 5, "California" = 6,
                                  "Colorado" = 7, "Connecticut" = 8,
                                  "Delaware" = 9, "Florida" = 10,
                                  "Georgia" = 11, "Hawaii" = 12,
                                  "Idaho" = 13, "Illinois" = 14,
                                  "Indiana" = 15, "Iowa" = 16,
                                  "Kansas" = 17, "Kentucky" = 18,
                                  "Louisiana" = 19, "Maine" = 20,
                                  "Maryland" = 21, "Massachusetts" = 22,
                                  "Michigan" = 23, "Minnesota" = 24,
                                  "Mississippi" = 25, "Missouri" = 26,
                                  "Montana" = 27, "Nebraska" = 28,
                                  "Nevada" = 29, "New Hampshire" = 30,
                                  "New Jersey" = 31, "New Mexico" = 32,
                                  "New York" = 33, "North Carolina" = 34,
                                  'North Dakota' = 35, "Ohio" = 36,
                                  "Oklahoma" = 37, "Oregon" = 38,
                                  "Pennsylvania" = 39, "Rhode Island" = 40,
                                  "South Carolina" = 41, "South Dakota" = 42,
                                  "Tennessee" = 43, "Texas" = 44,
                                  "Utah" = 45, "Vermont" = 46,
                                  "Virginia" = 47, "Washington" = 48,
                                  "West Virginia" = 49, "Wisconsin" = 50,
                                  "Wyoming" = 51, "Washington, D.C." = 52),
                                selected = 1)
                ),
                box(width = NULL,
                    title = "2. Assumptions about New Diagnoses", status = "primary", solidHeader = TRUE,
                    HTML("<h5 style=\"color:green\">Of all those <u>newly diagnosed with HIV</u>:</h3>"),
                    numericInput("msmdiagpct",
                                 "% of new diagnoses attributed to MSM",
                                 min = 0, value = NA),

                    HTML("<h5 style=\"color:green\">Of all those <u>newly diagnosed with HIV</u>:</h3>"),
                    numericInput("hetdiagpct",
                                 "% of new diagnoses attributed to Heterosexuals",
                                 min = 0, value = NA),

                    HTML("<h5 style=\"color:green\">Of all those <u>newly diagnosed with HIV</u>:</h3>"),
                    numericInput("pwiddiagpct",
                                 "% of new diagnoses attributed to PWID",
                                 min = 0, value = NA)#,
                )
                    ),

                column(width = 4,
                box(width = NULL,
                title = "3. Race-specific Assumptions about New Diagnoses" , status = "primary", solidHeader = TRUE,
                  column(width = 4,
                    numericInput("customblackmsmdiagpct", "Proportion of new diagnoses among African-Americans attributed to MSM",
                                 min = 0, value = NA),
                    numericInput("customblackhetdiagpct", "Proportion of new diagnoses among African-Americans attributed to HET",
                                 min = 0, value = NA),
                    numericInput("customblackpwiddiagpct", "Proportion of new diagnoses among African-Americans attributed to PWID",
                                 min = 0, value = NA)),
                  column(width = 4,
                    numericInput("customhispmsmdiagpct", "Proportion of new diagnoses among Hispanics attributed to MSM",
                                 min = 0, value = NA),
                    numericInput("customhisphetdiagpct", "Proportion of new diagnoses among Hispanics attributed to HET",
                                 min = 0, value = NA),
                    numericInput("customhisppwiddiagpct", "Proportion of new diagnoses among Hispanics attributed to PWID",
                                 min = 0, value = NA )),
                  column(width = 4,
                         numericInput("customwhitemsmdiagpct", "Proportion of new diagnoses among Non-Hispanic Whites attributed to MSM",
                                      min = 0, value = NA),
                         numericInput("customwhitehetdiagpct", "Proportion of new diagnoses among Non-Hispanic Whites attributed to HET",
                                      min = 0, value = NA),
                         numericInput("customwhitepwiddiagpct", "Proportion of new diagnoses among Non-Hispanic Whites attributed to PWID",
                                      min = 0, value = NA))
                ) # End box
                ), # End column
                column(width = 5,
                       box(width = NULL,
                           title = "4. PrEP Indications by Transmission Category", status = "success", solidHeader = TRUE,
                           numericInput("totalprep",
                                        "Total indicated for PrEP (#)",
                                        min = 0, value = NA),
                           numericInput("msmprep",
                                        "MSM indicated for PrEP (#)",
                                        min = 0, value = NA),
                           numericInput("hetprep",
                                        "HET indicated for PrEP (#)",
                                        min = 0, value = NA),
                           numericInput("pwidprep",
                                        "PWID indicated for PrEP (#)",
                                        min = 0, value = NA)
                       ) # End box
                ), # End column
                column(width = 5,
                       box(width = NULL,
                           title = "5. PrEP Indications by Race and Transmission Category", status = "success", solidHeader = TRUE,
                           HTML("<font color=\"red\"><b>"), textOutput("warningText1"), HTML("</font></b>"),
                           br(),
                           column(width = 4,
                             numericInput("blackmsmprep",
                                          "Black MSM indicated for PrEP (#)",
                                          min = 0, value = NA),
                             numericInput("hispmsmprep",
                                          "Hispanic MSM indicated for PrEP (#)",
                                          min = 0, value = NA),
                             numericInput("whitemsmprep",
                                          "White MSM indicated for PrEP (#)",
                                          min = 0, value = NA)),
                           column(width = 4,
                             numericInput("blackhetprep",
                                          "Black HET indicated for PrEP (#)",
                                          min = 0, value = NA),
                             numericInput("hisphetprep",
                                          "Hispanic HET indicated for PrEP (#)",
                                          min = 0, value = NA),
                             numericInput("whitehetprep",
                                          "White HET indicated for PrEP (#)",
                                          min = 0, value = NA)),
                           column(width = 4,
                             numericInput("blackpwidprep",
                                          "Black PWID indicated for PrEP (#)",
                                          min = 0, value = NA),
                             numericInput("hisppwidprep",
                                          "Hispanic PWID indicated for PrEP (#)",
                                          min = 0, value = NA),
                             numericInput("whitepwidprep",
                                          "White PWID indicated for PrEP (#)",
                                          min = 0, value = NA))
                       ) # End box
                ) # End column
                ), # End fluidRow

            fluidRow(
                box(width = 12, height = 300, status = "info", solidHeader = TRUE,
                    tabsetPanel(
                      tabPanel("Current Scenario",
                               tabsetPanel(
                                 tabPanel("Report",
                                          mainPanel(
                                            textInput("reportTitle", label = "Report Title"),
                                            downloadButton("downloadReport",
                                                           label = "Generate Report")
                                          )))),

                      tabPanel("All Scenarios",
                               tabsetPanel(
                                 tabPanel("Save",
                                          textInput("reportTitle", label = "Scenario Name"),
                                          column(2,
                                                 actionButton("saveButton", "Save Scenario")
                                          ),
                                          uiOutput("scenarioSummaryTable")
                                 ),
                                 tabPanel("Remove",
                                          column(1.2, selectInput("removeScenarioNum", "Select scenario #", choices = 0)),
                                          column(2, actionButton("removeButton", "Remove Scenario"))
                                 ),
                                 tabPanel("Compare",
                                          column(1.5, selectInput("compareScenario1", "Select scenario", choices = 0)),
                                          column(1.5, selectInput("compareScenario2", "Select scenario", choices = 0)),
                                          column(1.5, actionButton("compareScenarios", "Compare")),
                                          column(5, plotOutput("compareTxBar")),
                                          column(2, textOutput("comparisonText1"))
                                 )
                               ) # End all scenarios tabsetPanel
                      ) # End all scenarios tabPanel
                    ) # End tabsetPanel
                        ) # End box
                )
            )
            )
                           ) # End dashboardBody
                           ) # End dashboardPage
                           ) # End ShinyUI




