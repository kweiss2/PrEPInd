library(shiny)
library(shinydashboard)
library(rsconnect)
library(shinythemes)
library(knitr)
library(rmarkdown)
library(ggplot2)
library(plyr)
library(dplyr)


shinyUI(dashboardPage(skin = "blue",
    dashboardHeader(title = "PrEP Indications"),
    dashboardSidebar(
        width = 200,
        sidebarMenu(id = "tabs",
            menuItem("Introduction", tabName = "Introduction", icon = icon("book")),
            menuItem("Estimation Tool", tabName = "Estimation", icon = icon("line-chart"))
        )


        ), # End dashboardSidebar
    dashboardBody(
      tags$head(tags$style(HTML('
                      /* logo */
                      .skin-blue .main-header .logo {
                      background-color: #333333;
                      color: #FFFFFF;
                      }

                      /* logo when hovered */
                      .skin-blue .main-header .logo:hover {
                      background-color: #f4b943;
                      }

                      /* navbar (rest of the header) */
                      .skin-blue .main-header .navbar {
                      background-color: #333333;
                      color: #FFFFFF;
                      }

                      /* main sidebar */
                      .skin-blue .main-sidebar {
                      background-color: #333333;
                      color: #FFFFFF;
                      }

                      /* sidebar collapse */
                      .skin-blue .main-sidebar .sidebar-collapse{
                      background-color: #333333;
                                color: #FFFFFF;
                      }

                      /* active selected tab in the sidebarmenu */
                      .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                      background-color: #ff0000;
                      }

                      /* other links in the sidebarmenu */
                      .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                      background-color: #333333;
                      color: #FFFFFF;
                      }

                      /* other links in the sidebarmenu when hovered */
                      .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                      background-color: #ff69b4;
                      }

                      /* toggle button when hovered  */
                      .skin-blue .main-header .navbar .sidebar-toggle:hover{
                      background-color: #ff69b4;
                      }
                      ')),

      # Default zoom
      tags$style("
              body {
                 -moz-transform: scale(1.0, 1.0); /* Moz-browsers */
                 zoom: 1.0; /* Other non-webkit browsers */
                 zoom: 100%; /* Webkit browsers */
                 }
                 ")),

        tabItems(

            ## Introduction tab ##
            tabItem(tabName = "Introduction",
                    column(10, offset = 1,
                           h1("Estimates of Persons with Indications for Preexposure Prophylaxis",
                              style = "color: black;"),
                           h2("A Web-Based Modeling Tool for Public Health Practice", style = "color: black;"),
                           hr(),
                           p("This software tool provides additional opportunities to explore the
                             estimates from the paper:",
                             tags$blockquote("Smith DK, Van Handel M, Grey J",
                                             "Estimates of Persons with Indications for Preexposure
                                              Prophylaxis by Jurisdiction Transmission Risk Group,
                                             and Race/Ethnicity, United States, 2015.", em("In Clearance."), "2017.",
                                             style = "color:black;"),
                            "This webtool provides users the ability to estimate the number of individuals indicated for PrEP.",
                            style = "color: black;"),

                           p("You can use the", em("Tab"), "key and the", em("Shift+Tab"), "keys on a keyboard to
                             navigate between fields. You can also use the ", em("Enter"), "key to expand or
                             collapse the sidebar.", style = "color: black;"),

                           p("To get started, enter an estimate of the MSM population size in your
                             jurisdiction of interest, and select a jurisdiction on which to base the
                             model assumptions. By changing the jurisdiction, you can see how the assumptions
                             about the model change.", style = "color: black;"),
                           tags$ul(
                               tags$li(strong("Estimates by transmission risk group:"),
                                       "this model will produce estimates of persons with
                                       indications for preexposure prophylaxis among men
                                       who have sex with men (MSM), heterosexuals (HET),
                                       and persons who inject drugs (PWID).",
                                       style = "color: black;"),
                               tags$li(strong("Estimates by race and transmission risk group:"),
                                       "this model will also produce race-stratified estimates of
                                       persons with indications for preexposure prophylaxis among
                                       men who have sex with men (MSM), heterosexuals (HET), and
                                       persons who inject drugs (PWID).",
                                       style = "color: black;")
                               ),
                           p("After selecting the parameters set in each model, the model will
                             automatically update the values and summary data tables.
                             These data should be interpreted with caution.", style = "color: black;"),
                           p(em("We acknowledge support from the CDC/NCHHSTP Epidemiological
                                and Economic Modeling Agreement (5U38PS004646). The findings
                                conclusions used to build this tool are solely the responsibility
                                of the authors and do not necessarily represent the official
                                views of the Centers for Disease Control and Prevention or the Department
                                of Health and Human Services."), style = "color: black;"),
                           p("You can change the page by navigating to the", em("Switch Tab"), "button below
                              or by using the links on the sidebar.",
                             style = "color: black;")
                           ),

                    fluidRow(
                      column(6, align = "center", offset = 3,
                             actionButton('switchtab', 'Switch Tab', icon = icon("line-chart"))
                           )
                      )
                    ),
        tabItem(
            ## Estimation Tab ##
            tabName = "Estimation",
            tags$head(
                tags$link(rel = "stylesheet", type = "text/css", href = "style.css", color = "color: black;"),
                tags$script(type = "text/javascript", src = "busy.js")
            ),

            div(class = "busy",
                img(src = "ajax-loader.gif")
            ),

            fluidRow(
                column(width = 3,
                box(width = NULL,
                    title = "1. Population Size and Jurisdiction", status = "primary", solidHeader = TRUE,
                    HTML("<h4 style=\"color:green\">Please enter a population size without using commas</u>:</h4>"),
                    numericInput("msmpopsize",
                                 "Jurisdiction MSM Population Size",
                                 min = 0,
                                 value = NA),
                    selectInput(inputId = "jurisdiction", label = "1. Jurisdiction Assumptions",
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
                    HTML("<font color=\"red\"><b>"), textOutput("warningText4"), HTML("</font></b>"),
                    HTML("<h4 style=\"color:green\">Of all those <u>newly diagnosed with HIV</u>:</h4>"),
                    numericInput("msmdiagpct",
                                 "% of new diagnoses attributed to MSM",
                                 min = 0, value = NA),

                    HTML("<h4 style=\"color:green\">Of all those <u>newly diagnosed with HIV</u>:</h4>"),
                    numericInput("hetdiagpct",
                                 "% of new diagnoses attributed to HET",
                                 min = 0, value = NA),

                    HTML("<h4 style=\"color:green\">Of all those <u>newly diagnosed with HIV</u>:</h4>"),
                    numericInput("pwiddiagpct",
                                 "% of new diagnoses attributed to PWID",
                                 min = 0, value = NA)#,
                )
                    ),

                column(width = 4,
                box(width = NULL,
                title = "3. Race-specific Assumptions about New Diagnoses" , status = "primary", solidHeader = TRUE,
                HTML("<h4 style=\"color:green\">Of all those <u>newly diagnosed with HIV</u>:</h4>"),
                  column(width = 4,
                    HTML("<font color=\"red\"><b>"), textOutput("warningText5"), HTML("</font></b>"),
                    HTML("<h4 style=\"color:green\">MSM</u>:</h4>"),
                    numericInput("blackmsmdiagpct", "% of new MSM diagnoses attributed to Black MSM",
                                 min = 0, value = NA),
                    numericInput("hispmsmdiagpct", "% of new MSM diagnoses attributed to Hispanic MSM",
                                 min = 0, value = NA),
                    numericInput("whitemsmdiagpct", "% of new MSM diagnoses attributed to White MSM",
                                 min = 0, value = NA)),
                  column(width = 4,
                    HTML("<font color=\"red\"><b>"), textOutput("warningText6"), HTML("</font></b>"),
                    HTML("<h4 style=\"color:green\">HET</u>:</h4>"),
                    numericInput("blackhetdiagpct", "% of new HET diagnoses attributed to Black HET",
                                 min = 0, value = NA),
                    numericInput("hisphetdiagpct", "% of new HET diagnoses attributed to Hispanic HET",
                                 min = 0, value = NA),
                    numericInput("whitehetdiagpct", "% of new HET diagnoses attributed to White HET",
                                 min = 0, value = NA )),
                  column(width = 4,
                    HTML("<font color=\"red\"><b>"), textOutput("warningText7"), HTML("</font></b>"),
                    HTML("<h4 style=\"color:green\">PWID</u>:</h4>"),
                    numericInput("blackpwiddiagpct", "% of new PWID diagnoses attributed to Black PWID",
                                  min = 0, value = NA),
                    numericInput("hisppwiddiagpct", "% of new PWID diagnoses attributed to Hispanic PWID",
                                  min = 0, value = NA),
                    numericInput("whitepwiddiagpct", "% of new PWID diagnoses attributed to White PWID",
                                  min = 0, value = NA))
                ), # End box
                fluidRow(
                  column(6, align = "center", offset = 3,
                         actionButton('switchtab2', 'Switch Tab', icon = icon("line-chart"))
                  ))
                ), # End column
                column(width = 5,
                   box(width = NULL,
                       title = "4. PrEP Indications by Transmission Risk Group", status = "success", solidHeader = TRUE,
                       #HTML("<font color=\"red\"><b>"), textOutput("warningText2"), HTML("</font></b>"),
                       numericInput("totalprep",
                                    "Estimated Total with indications for PrEP (#)",
                                    min = 0, value = NA),
                       numericInput("msmprep",
                                    "Estimated MSM with indications for PrEP (#)",
                                    min = 0, value = NA),
                       numericInput("hetprep",
                                    "Estimated HET with indications for PrEP (#)",
                                    min = 0, value = NA),
                       numericInput("pwidprep",
                                    "Estimated PWID with indications for PrEP (#)",
                                    min = 0, value = NA)
                   ) # End box
                ), # End column
                column(width = 5,
                       box(width = NULL,
                           title = "5. PrEP Indications by Race and Transmission Risk Group", status = "success", solidHeader = TRUE,
                           HTML("<font color=\"red\"><b>"), textOutput("warningText1"), HTML("</font></b>"),
                           #HTML("<font color=\"red\"><b>"), textOutput("warningText3"), HTML("</font></b>"),
                           column(width = 4,
                             HTML("<h4 style=\"color:green\">MSM with indications for PrEP</u>:</h4>"),
                             numericInput("blackmsmprep",
                                          "African-American (#)",
                                          min = 0, value = NA),
                             numericInput("hispmsmprep",
                                          "Hispanic (#)",
                                          min = 0, value = NA),
                             numericInput("whitemsmprep",
                                          "White (#)",
                                          min = 0, value = NA)),
                           column(width = 4,
                             HTML("<h4 style=\"color:green\">HET with indications for PrEP</u>:</h4>"),
                             numericInput("blackhetprep",
                                          "African-American (#)",
                                          min = 0, value = NA),
                             numericInput("hisphetprep",
                                          "Hispanic (#)",
                                          min = 0, value = NA),
                             numericInput("whitehetprep",
                                          "White (#)",
                                          min = 0, value = NA)),
                           column(width = 4,
                             HTML("<h4 style=\"color:green\">PWID with indications for PrEP</u>:</h4>"),
                             numericInput("blackpwidprep",
                                          "African-American (#)",
                                          min = 0, value = NA),
                             numericInput("hisppwidprep",
                                          "Hispanic (#)",
                                          min = 0, value = NA),
                             numericInput("whitepwidprep",
                                          "White (#)",
                                          min = 0, value = NA))
                       ) # End box
                ) # End column
                ), # End fluidRow

            fluidRow(
                box(width = 12, height = 180, status = "info", solidHeader = TRUE,
                       tabsetPanel(type = "pills",
                         tabPanel("Report",
                                  mainPanel(
                                    textInput("reportTitle", label = "Report Title"),
                                    downloadButton("report",
                                                   label = "Generate Report")
                                          ) #End mainPanel
                                          ) #End tabPanel
                                 ) # End tabsetPanel
                        ) # End box
                ) #End fluidRow
            ) # End tabItem
            ) # End tabItems
                           ) # End dashboardBody
                           ) # End dashboardPage
                           ) # End ShinyUI
