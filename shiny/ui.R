library("shiny")
library("shinydashboard")
library("deSolve")
suppressMessages(library("EpiModel"))
library("plotly")

shinyUI(fluidPage(
                     tabPanel("PrEP Indications",
                              titlePanel(HTML("Estimate the number of individuals indicated for PrEP")),
                              h5("Start on the left and work your way to the right!"),
                              br(),
                              tabsetPanel(
                                tabPanel("Indications by Transmission Category",
                                         column(4,
                                                HTML("<h4 style=\"color:#2A268B\">1. Basic Inputs & Assumptions</h4>"),

                                                numericInput("msmpopsize",
                                                             "Jurisdiction MSM Population Size",
                                                             min = 0,
                                                             value = NA),

                                                selectInput("jurisdiction", label = "Jurisdiction Assumptions",
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
                                                            selected = 1)),

                                         column(4,
                                              #          HTML("<div style=\"width:3;height:3;border:3px solid #000;background:#46B7EB\"><h4 style=\"color:#2A268B\">2. Local Continuum Information</h4>"),
                                                HTML("<h4 style=\"color:#2A268B\">2. Jurisdiction Diagnosis Information</h4>"),

                                                HTML("<h5 style=\"color:green\">Of all those <u>newly diagnosed with HIV</u>:</h3>"),
                                                numericInput("msmdiagpct",
                                                             "% of new diagnoses attributed to MSM",
                                                             min = 0,
                                                             value = NA),

                                                HTML("<h5 style=\"color:green\">Of all those <u>newly diagnosed with HIV</u>:</h3>"),
                                                numericInput("hetdiagpct",
                                                             "% of new diagnoses attributed to Heterosexuals",
                                                             min = 0,
                                                             value = NA),

                                                HTML("<h5 style=\"color:green\">Of all those <u>newly diagnosed with HIV</u>:</h3>"),
                                                numericInput("pwiddiagpct",
                                                             "% of new diagnoses attributed to PWID",
                                                             min = 0,
                                                             value = NA)#,
                                         ),

                                         column(4,
                                                HTML("<h4 style=\"color:#2A268B\">3. Jurisdictional PrEP Indications by Transmission Category</h4>"),

                                                numericInput("totalprep",
                                                             "Total indicated for PrEP (#)",
                                                             min = 0,
                                                             value = NA),

                                                numericInput("msmprep",
                                                             "MSM indicated for PrEP (#)",
                                                             min = 0,
                                                             value = NA),

                                                numericInput("hetprep",
                                                             "HET indicated for PrEP (#)",
                                                             min = 0,
                                                             value = NA),

                                                numericInput("pwidprep",
                                                             "PWID indicated for PrEP (#)",
                                                             min = 0,
                                                             value = NA)
                                                #HTML("<font color=\"red\"><b>"), textOutput("warningText2"), HTML("</font></b>")
                                         )#,
                                         #conditionalPanel(condition = "output.warningText1 == \"\" & output.warningText2 == \"\"",

                                         #), # Conditional panel
                                ), #End tabPanel
                                tabPanel("Indications by Race and Transmission Category",
                                         column(4,
                                                HTML("<h4 style=\"color:#2A268B\">1. Basic Inputs and Assumptions </h4>"),
                                                numericInput("msmpopsize2",
                                                             "Jurisdiction MSM Population Size",
                                                             min = 0,
                                                             value = NA),
                                                selectInput("existingprop", label = "Jurisdiction Assumptions",
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
                                                            selected = 0)),
                                         column(4,
                                                HTML("<h4 style=\"color:#2A268B\">2. Jurisdiction Diagnosis Information </h4>"),
                                                numericInput("customblackmsmdiagpct", "Proportion of all diagnoses among African-Americans attributed to MSM", min = 0, value = NA),
                                                numericInput("customblackhetdiagpct", "Proportion of all diagnoses among African-Americans attributed to HET", min = 0, value = NA),
                                                numericInput("customblackpwiddiagpct", "Proportion of all diagnoses among African-Americans attributed to PWID", min = 0, value = NA),
                                                numericInput("customhispmsmdiagpct", "Proportion of all diagnoses among Hispanics attributed to MSM", min = 0, value = NA),
                                                numericInput("customhisphetdiagpct", "Proportion of all diagnoses among Hispanics attributed to HET", min = 0, value = NA),
                                                numericInput("customhisppwiddiagpct", "Proportion of all diagnoses among Hispanics attributed to PWID", min = 0, value = NA),
                                                numericInput("customwhitemsmdiagpct", "Proportion of all diagnoses among Non-Hispanic Whites attributed to MSM", min = 0, value = NA),
                                                numericInput("customwhitehetdiagpct", "Proportion of all diagnoses among Non-Hispanic Whites attributed to HET", min = 0, value = NA),
                                                numericInput("customwhitepwiddiagpct", "Proportion of all diagnoses among Non-Hispanic Whites attributed to PWID", min = 0, value = NA)),
                                         column(4,
                                                HTML("<h4 style=\"color:#2A268B\">3.  Jurisdictional PrEP Indications by Race and Transmission Category</h4>"),

                                                numericInput("blackmsmprep",
                                                             "Black MSM indicated for PrEP (#)",
                                                             min = 0,
                                                             value = NA),

                                                numericInput("hispmsmprep",
                                                             "Hispanic MSM indicated for PrEP (#)",
                                                             min = 0,
                                                             value = NA),

                                                numericInput("whitemsmprep",
                                                             "White MSM indicated for PrEP (#)",
                                                             min = 0,
                                                             value = NA),

                                                numericInput("blackhetprep",
                                                             "Black HET indicated for PrEP (#)",
                                                             min = 0,
                                                             value = NA),

                                                numericInput("hisphetprep",
                                                             "Hispanic HET indicated for PrEP (#)",
                                                             min = 0,
                                                             value = NA),

                                                numericInput("whitehetprep",
                                                             "White HET indicated for PrEP (#)",
                                                             min = 0,
                                                             value = NA),
                                                numericInput("blackpwidprep",
                                                             "Black PWID indicated for PrEP (#)",
                                                             min = 0,
                                                             value = NA),

                                                numericInput("hisppwidprep",
                                                             "Hispanic PWID indicated for PrEP (#)",
                                                             min = 0,
                                                             value = NA),

                                                numericInput("whitepwidprep",
                                                             "White PWID indicated for PrEP (#)",
                                                             min = 0,
                                                             value = NA)
                                         ),
                                         br(),
                                         br(),
                                         br(),
                                         HTML("<font color=\"red\"><b>"), textOutput("warningText1"), HTML("</font></b>"),
                                         HTML("</div>")
                                ) # End tabPanel
                              ), # End tabset panel
                              mainPanel(
                                tabsetPanel(
                                  tabPanel("Current Scenario",
                                           tabsetPanel(
                                             tabPanel("Report",
                                                      mainPanel(
                                                        textInput("reportTitle", label = "Report title"),
                                                        br(),
                                                        downloadButton("downloadReport",
                                                                       label = "Generate Report")
                                                      )))),

                                  tabPanel("All Scenarios",
                                           tabsetPanel(
                                             tabPanel("Save",
                                                      column(1.1, HTML("<h5 style=\"color:#006600\">Scenario name:</h5>")),
                                                      column(2.2,
                                                             textInput("scenarioName", "")
                                                      ),
                                                      column(2,
                                                             actionButton("saveButton", "Save Scenario")
                                                      ),
                                                      uiOutput("scenarioSummaryTable")
                                             ),
                                             tabPanel("Remove",
                                                      column(1.2, selectInput("removeScenarioNum", "Select scenario #", choices = 0),br(),br(),br(),br(),br(),br()),
                                                      #                                                  column(1, HTML("<h5 style=\"color:#006600\">OR</h5>"),br(),br(),br(),br(),br(),br()),
                                                      #                                                  column(1.2, selectInput("removeScenarioName", "Select scenario name", choices=""),br(),br(),br(),br(),br(),br()),
                                                      column(2, br(), actionButton("removeButton", "Remove Scenario"),br(),br(),br(),br(),br(),br())
                                             ),
                                             tabPanel("Compare",
                                                      column(1.5, selectInput("compareScenario1", "Select scenario", choices = 0)),
                                                      column(1.5, selectInput("compareScenario2", "Select scenario", choices = 0)),
                                                      column(1.5, br(), actionButton("compareScenarios", "Compare")),
                                                      br(),
                                                      column(5, plotOutput("compareTxBar")),
                                                      column(2, br(), br(), br(), textOutput("comparisonText1"))
                                             )
                                           )

                                  )
                                ),
                                width = 20
                              ) # End Main Panel
                     )
) # End Fluid Page
) # End ShinyUI
