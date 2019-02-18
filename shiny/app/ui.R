library(shiny)
library(shinydashboard)
library(rsconnect)
library(shinythemes)
library(knitr)
library(rmarkdown)
library(ggplot2)
library(plyr)
library(dplyr)
library(kableExtra)
library(magrittr)
library(tinytex)
library(RColorBrewer)
library(grDevices)
library(scales)

shinyUI(

  ## Dashboard page ------------------------------------------------------------
    dashboardPage(skin = "blue",

  ## Dashboard header ----------------------------------------------------------
    dashboardHeader(title = "PrEP Populations"),

  ## Dashboard sidebar ---------------------------------------------------------
    dashboardSidebar(
        width = 200,
        sidebarMenu(id = "tabs",
            menuItem("Introduction", tabName = "Introduction",
                     icon = icon("book")),
            menuItem("Instructions", tabName = "Instructions",
                     icon = icon("book")),
            menuItem("Estimation Tool", tabName = "Estimation",
                     icon = icon("line-chart"))
        ) # End sidebarMenu
        ), # End dashboardSidebar

  ## Dashboard body ------------------------------------------------------------
    dashboardBody(

  #### HTML tags for display ---------------------------------------------------
        tags$head(tags$style(
                    HTML('
                      {
                      lang ="en";
                      xml:lang="en"
                      }

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

                      ') # End html
                    ), # End tags$style

      ## Default zoom --------------------------------------------------------
      tags$style("
              body {
                 -moz-transform: scale(1.0, 1.0); /* Moz-browsers */
                 zoom: 1.0; /* Other non-webkit browsers */
                 zoom: 100%; /* Webkit browsers */
                 }
                 ") # End tags$style
      ), # End tags$head

        tabItems(

        ## Introduction tab ----------------------------------------------------
            tabItem(tabName = "Introduction",
                    column(10, offset = 1,

                           #### Text -------------------------------------------
                           h1("Estimates of Persons with Indications for
                              Preexposure Prophylaxis",
                              style = "color: black;"),
                           h2("A Web-Based Modeling Tool for Public Health
                              Practice",
                              style = "color: black;"),
                           hr(),
                           p("This software tool provides additional
                              opportunities to explore the estimates from
                             the paper:",
                             tags$blockquote("Smith DK, Van Handel M, Grey J.",
                                             "Estimates of Persons with
                                              Indications for Preexposure
                                              Prophylaxis by Jurisdiction
                                              Transmission Risk Group, and
                                             Race/Ethnicity, United States,
                                             2015.",
                                             "Annals of Epidemiology, 2018.",
                             tags$a("Paper Link", target = "_blank",
                                    href = "https://doi.org/10.1016/j.annepidem.2018.05.003"),
                             style = "color:black;"),
                            "This webtool provides users the ability to estimate
                            the number of individuals indicated for PrEP.
                            You can use the", em("Tab"), "key and the",
                              em("Shift+Tab"), "keys on a keyboard to
                             navigate between fields. You can also use the ",
                              em("Enter"), "key to expand or collapse the
                             sidebar.", style = "color: black;"),

                           p("To get started, select a jurisdiction of interest
                              on which to base the model assumptions. By changing
                              the jurisdiction, you can see how assumptions about
                              the model change. You can also edit the assumptions
                              manually. Then, enter an estimate of the MSM
                              population size in that jurisdiction.",
                             style = "color: black;"),
                           tags$ul(
                               tags$li(strong("Estimates by transmission risk
                                              group:"), "this model will produce
                                        estimates of persons with indications
                                        for preexposure prophylaxis among men
                                         who have sex with men (MSM),
                                        heterosexuals (HET), and persons who
                                         inject drugs (PWID).",
                                       style = "color: black;"),
                               tags$li(strong("Estimates by race and
                                              transmission risk group:"),
                                       "this model will also produce
                                        race-stratified estimates of persons
                                        with indications for preexposure
                                        prophylaxis among men who have sex
                                        with men (MSM), heterosexuals (HET),
                                        and persons who inject drugs (PWID).",
                                       style = "color: black;")
                               ),
                           p("After selecting the parameters set in each model,
                              the model will automatically update the values
                              and summary data tables. These data should be
                              interpreted with caution. After generating the
                              values, you can navigate to the bottom of the
                              page, where you download a report with figures
                              and summary data tables. You can change the page
                              by navigating to the", em("Switch Tab"), "button
                              below or by using the links on the sidebar.",
                             style = "color: black;"),
                           hr(),
                           p(em("We acknowledge support from the CDC/NCHHSTP
                                  Epidemiological and Economic Modeling
                                  Agreement (5U38PS004646). The findings and
                                  conclusions used to build this tool are
                                  solely the responsibility of the authors and
                                  do not necessarily represent the official
                                  views of the Centers for Disease Control and
                                  Prevention or the Department of Health and
                                  Human Services."),
                             style = "color: black;"),
                           hr(),
                           p(em("*508 ACCESSIBILITY of this product:")),
                           p("Section 508 requires Federal agencies and grantees
                              receiving Federal funds to ensure that individuals
                              with disabilities who are members of the public or
                              Federal employees have access to and use of
                              electronic and information technology (EIT) that
                              is comparable to that provided to individuals
                              without disabilities, unless an undue burden
                              would be imposed on the agency.",
                             style = "color: black;"),
                           p("If you need assistance with this webtool, please
                             contact", a("Dawn Smith",
                                         href = "mailto:dks0@cdc.gov"),
                             ".",
                             style = "color: black;")
                           ), # End column

                    #### Action button------------------------------------------
                    fluidRow(
                      column(6, align = "center", offset = 3,
                             actionButton('switchtab', 'Switch to Instructions',
                                          icon = icon("book"),
                                          style = 'padding:12px; font-size:120%'),
                             actionButton('switchtab2', 'Switch to Estimation',
                                          icon = icon("book"),
                                          style = 'padding:12px; font-size:120%')
                           ) # End column
                      ) # End fluidRow
                    ), # End tabItem

        ## Instructions Tab ----------------------------------------------------
        tabItem(tabName = "Instructions",

                #### Action button------------------------------------------

                fluidRow(
                  column(10 , align = "left", offset = 4,
                         actionButton('switchtab3', 'Switch to Introduction',
                                      icon = icon("book"),
                                      style = 'padding:12px; font-size:120%'),
                         actionButton('switchtab4', 'Switch to Estimation',
                                      icon = icon("book"),
                                      style = 'padding:12px; font-size:120%'),
                         hr()
                  ) # End column
                ), # End fluidRow
                fluidRow(
                  column(12 , align = "left", offset = 1,
                         p("This webtool provides users the ability to estimate
                           the number of persons with indications for PrEP among
                           men who have sex with men (MSM), heterosexuals (HET),
                           and persons who use injection drugs (PWID), as well as
                           race/ethnicity-stratified estimates."),
                         p("To navigate: You can use your mouse or the",
                            strong("Tab"), "key and the", strong("Shift"), "and",
                            strong("Tab"), "keys together on a keyboard to
                            navigate forwards and backwards between fields.
                            You can also use the", strong("Enter"), "key to
                           expand or collapse the sidebar."),
                         p("This brief guide will walk you through each step to
                           produce your own tailored report:")
                  ) # End column
                ), # End fluidRow

                #### Images -------------------------------------------
                fluidRow(
                  column(10, offset = 3,

                       img(src = 'instructions.png', align = "left",
                           width = "600", height = "600",
                           alt = "Instructions for using the Webtool")
                       ) # End column
                  ) # End fluidRow
                ), # End tabItem

        tabItem(

            ## Estimation Tab --------------------------------------------------
            tabName = "Estimation",

            #### CSS styling for estimation tab --------------------------------
            tags$head(
                tags$link(rel = "stylesheet", type = "text/css",
                          href = "style.css",
                          color = "color: black;"),
                tags$script(type = "text/javascript", src = "busy.js")
            ),

            div(class = "busy",
                img(src = "ajax-loader.gif", alt = "A spinning busy symbol")
            ),

            fluidRow(
                column(width = 3,

                #### Box 1 - Pop Size, Jurisdiction ----------------------------
                box(width = NULL,
                    title = "1. Population Size and Jurisdiction",
                    status = "primary", solidHeader = TRUE,
                    p("Select a jurisdiction by using the up, down, and
                      Enter keys or the dropdown menu.",
                      style = "color: green;"),
                    p("You can also use the backspace key and begin to type the
                      name of the jurisdiction:",
                      style = "color: green;"),

                    selectInput(inputId = "jurisdiction",
                                label = "Jurisdiction Assumptions",
                                choices = c("Total" = 1, "Alabama" = 2,
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
                                            "North Dakota" = 35, "Ohio" = 36,
                                            "Oklahoma" = 37, "Oregon" = 38,
                                            "Pennsylvania" = 39, "Rhode Island" = 40,
                                            "South Carolina" = 41, "South Dakota" = 42,
                                            "Tennessee" = 43, "Texas" = 44,
                                            "Utah" = 45, "Vermont" = 46,
                                            "Virginia" = 47, "Washington" = 48,
                                            "West Virginia" = 49, "Wisconsin" = 50,
                                            "Wyoming" = 51, "Washington, D.C." = 52),
                                selected = 1),
                    p("Please enter a population size without using commas:",
                      style = "color: green;"),
                    numericInput(inputId = "msmpopsize",
                                 label = "Jurisdiction MSM Population Size",
                                 min = 0,
                                 value = NA)
                    # HTML('
                    #       Jurisdiction:
                    #       Total
                    #       Alabama
                    #       Alaska
                    #       Arizona
                    #       Arkansas
                    #       California
                    #       Colorado
                    #       Connecticut
                    #       Delaware
                    #       Florida
                    #       Georgia
                    #       Hawaii
                    #       Idaho
                    #       Illinois
                    #       Indiana
                    #       Iowa
                    #       Kansas
                    #       Kentucky
                    #       Louisiana
                    #       Maine
                    #       Maryland
                    #       Massachusetts
                    #       Michigan
                    #       Minnesota
                    #       Mississippi
                    #       Missouri
                    #       Montana
                    #       Nebraska
                    #       Nevada
                    #       New Hampshire
                    #       New Jersey
                    #       New Mexico
                    #       New York
                    #       North Carolina
                    #       North Dakota
                    #       Ohio
                    #       Oklahoma
                    #       Oregon
                    #       Pennsylvania
                    #       Rhode Island
                    #       South Carolina
                    #       South Dakota
                    #       Tennessee
                    #       Texas
                    #       Utah
                    #       Vermont
                    #       Virginia
                    #       Washington
                    #       West Virginia
                    #       Wisconsin
                    #       Wyoming
                    #       D.C.
                    #
                    #      ')
                ), # End box

                #### Box 2 - Assumptions about new diagnoses -------------------
                box(width = NULL,
                    title = "2. Assumptions about New Diagnoses",
                    status = "primary", solidHeader = TRUE,
                    HTML("<style=\"color=:red\"><strong>"),
                        textOutput("warningText4"),
                        HTML("</strong>"),
                    p("Of all those", em(strong("newly diagnosed")),
                      "with HIV:",
                      style = "color: green;"),
                    numericInput(inputId = "msmdiagpct",
                                 label = "% of new diagnoses attributed to MSM",
                                 min = 0, value = NA),

                    p("Of all those", em(strong("newly diagnosed")),
                      "with HIV:",
                      style = "color: green;"),
                    numericInput(inputId = "hetdiagpct",
                                 label = "% of new diagnoses attributed to HET",
                                 min = 0, value = NA),

                    p("Of all those", em(strong("newly diagnosed")),
                      "with HIV:",
                      style = "color: green;"),
                    numericInput(inputId = "pwiddiagpct",
                                 label = "% of new diagnoses attributed to PWID",
                                 min = 0, value = NA)#,
                ) # End box
                    ), # End column

                column(width = 4,

                #### Box 3 - Race-specific Assumptions about new diagnoses -----
                box(width = NULL,
                title = "3. Race-specific Assumptions about New Diagnoses" ,
                status = "primary", solidHeader = TRUE,
                p("Of all those", em(strong("newly diagnosed")), "with HIV:",
                  style = "color: green;"),
                  column(width = 4,
                    HTML("<font color=\"red\"><b>"),
                      textOutput("warningText5"),
                      HTML("</font></b>"),
                    p(em(strong("MSM:")),  style = "color: green;"),
                    numericInput(inputId = "blackmsmdiagpct",
                                 label = "% of new MSM diagnoses
                                 attributed to Black MSM",
                                 min = 0, value = NA),
                    numericInput(inputId = "hispmsmdiagpct",
                                 label = "% of new MSM diagnoses
                                 attributed to Hispanic MSM",
                                 min = 0, value = NA),
                    numericInput(inputId = "whitemsmdiagpct",
                                 label = "% of new MSM diagnoses
                                 attributed to White MSM",
                                 min = 0, value = NA)), # End column
                  column(width = 4,
                    HTML("<font color=\"red\"><b>"),
                      textOutput("warningText6"),
                      HTML("</font></b>"),
                    p(em(strong("HET:")),  style = "color: green;"),
                    numericInput(inputId = "blackhetdiagpct",
                                 label = "% of new HET diagnoses
                                 attributed to Black HET",
                                 min = 0, value = NA),
                    numericInput(inputId = "hisphetdiagpct",
                                 label = "% of new HET diagnoses
                                 attributed to Hispanic HET",
                                 min = 0, value = NA),
                    numericInput(inputId = "whitehetdiagpct",
                                 label = "% of new HET diagnoses
                                 attributed to White HET",
                                 min = 0, value = NA )), # End column
                  column(width = 4,
                    HTML("<font color=\"red\"><b>"),
                      textOutput("warningText7"),
                      HTML("</font></b>"),
                    p(em(strong("PWID:")),  style = "color: green;"),
                    numericInput(inputId = "blackpwiddiagpct",
                                 label = "% of new PWID diagnoses
                                 attributed to Black PWID",
                                  min = 0, value = NA),
                    numericInput(inputId = "hisppwiddiagpct",
                                 label = "% of new PWID diagnoses
                                 attributed to Hispanic PWID",
                                  min = 0, value = NA),
                    numericInput(inputId = "whitepwiddiagpct",
                                 label = "% of new PWID diagnoses
                                 attributed to White PWID",
                                  min = 0, value = NA)) # End column
                ), # End box

                #### Action button ---------------------------------------------
                fluidRow(
                  hr(),
                  hr(),
                  hr(),
                  hr(),
                  column(6, align = "center", offset = 3,
                         actionButton('switchtab5', 'Switch to Introduction',
                                      icon = icon("book"),
                                      style = 'padding:12px; font-size:120%'),
                         actionButton('switchtab6', 'Switch to Instructions',
                                      icon = icon("book"),
                                      style = 'padding:12px; font-size:120%')
                  )) # End fluidRow
                ), # End column

                column(width = 5,

                   #### Box 4 - PrEP Indications by Transmission Risk Group ----
                   box(width = NULL,
                       title = "4. Persons with PrEP Indications by
                       Transmission Risk Group",
                       status = "success", solidHeader = TRUE,
                       #HTML("<font color=\"red\"><b>"),
                         #textOutput("warningText2"),
                         #HTML("</font></b>"),
                       numericInput(inputId = "totalprep",
                                    label = "Estimated Total with
                                    indications for PrEP (#)",
                                    min = 0, value = NA),
                       numericInput(inputId = "msmprep",
                                    label = "Estimated MSM with
                                    indications for PrEP (#)",
                                    min = 0, value = NA),
                       numericInput(inputId = "hetprep",
                                    label = "Estimated HET with
                                    indications for PrEP (#)",
                                    min = 0, value = NA),
                       numericInput(inputId = "pwidprep",
                                    label = "Estimated PWID with
                                    indications for PrEP (#)",
                                    min = 0, value = NA)
                   ) # End box
                ), # End column

                column(width = 5,
                       #### Box 5 - PrEP Indications by Race and Risk Group ----

                       box(width = NULL,
                           title = "5. Persons with PrEP Indications by
                           Race and Transmission Risk Group",
                           status = "success", solidHeader = TRUE,
                           HTML("<font color=\"red\"><b>"),
                             textOutput("warningText1"),
                             HTML("</font></b>"),
                           # HTML("<font color=\"red\"><b>"),
                             # textOutput("warningText3"),
                             # HTML("</font></b>"),
                           column(width = 4,
                             p(em(strong("MSM with indications for PrEP:")),
                               style = "color: green;"),
                             numericInput(inputId = "blackmsmprep",
                                          label = "African-American MSM with
                                          indications for PrEP (#)",
                                          min = 0, value = NA),
                             numericInput(inputId = "hispmsmprep",
                                          label = "Hispanic MSM with
                                          indications for PrEP (#)",
                                          min = 0, value = NA),
                             numericInput(inputId = "whitemsmprep",
                                          label = "White MSM with
                                          indications for PrEP (#)",
                                          min = 0, value = NA)),
                           column(width = 4,
                             p(em(strong("HET with indications for PrEP:")),
                               style = "color: green;"),
                             numericInput(inputId = "blackhetprep",
                                          label = "African-American HET with
                                          indications for PrEP (#)",
                                          min = 0, value = NA),
                             numericInput(inputId = "hisphetprep",
                                          label = "Hispanic HET with indications
                                          for PrEP (#)",
                                          min = 0, value = NA),
                             numericInput(inputId = "whitehetprep",
                                          label = "White HET with indications
                                          for PrEP (#)",
                                          min = 0, value = NA)),
                           column(width = 4,
                             p(em(strong("PWID with indications for PrEP:")),
                               style = "color: green;"),
                             numericInput(inputId = "blackpwidprep",
                                          label = "African-American PWID with
                                          indications for PrEP (#)",
                                          min = 0, value = NA),
                             numericInput(inputId = "hisppwidprep",
                                          label = "Hispanic PWID with
                                          indications for PrEP (#)",
                                          min = 0, value = NA),
                             numericInput(inputId = "whitepwidprep",
                                          label = "White  PWID with
                                          indications for PrEP (#)",
                                          min = 0, value = NA))
                       ) # End box
                ) # End column
                ), # End fluidRow

            #### Report tab ----------------------------------------------------
            fluidRow(
                box(width = 12,
                    height = 220,
                    status = "info",
                    solidHeader = TRUE,
                       tabsetPanel(type = "pills",
                         tabPanel("Report",
                                  mainPanel(
                                    p("Type a title and press the enter key or
                                      click with your mouse to download a report
                                      in the format of your choosing, HTML,
                                      or RTF (compatible with Word).",
                                      style = "color: green;"),
                                    textInput(inputId = "reportTitle",
                                              label = "Report Title"),
                                    downloadButton(outputId = "report_html",
                                                   label = "Generate HTML
                                                   Report"),
                                    # downloadButton(outputId = "report_slidy",
                                    #                label = "Generate Presentation"),
                                    # downloadButton(outputId = "report_pdf",
                                    #                label = "Generate PDF
                                    #                Report"),
                                    downloadButton(outputId = "report_rtf",
                                                   label = "Generate RTF
                                                   Report"),
                                    downloadButton(outputId = "report",
                                                   label = "Generate Word
                                                   Report")
                                          ) #End mainPanel
                                          ) #End tabPanel
                                 ) # End tabsetPanel
                        ) # End box
                ), #End fluidRow
            fluidRow(column(12, align = "left", offset = 0,
                            p(em("*508 ACCESSIBILITY of this product:")),
                            p("Section 508 requires Federal agencies and grantees
                              receiving Federal funds to ensure that individuals
                              with disabilities who are members of the public or
                              Federal employees have access to and use of
                              electronic and information technology (EIT) that
                              is comparable to that provided to individuals
                              without disabilities, unless an undue burden
                              would be imposed on the agency.",
                              style = "color: black;"),
                             p("If you need assistance with this webtool, please
                                     contact", a("Dawn Smith",
                                                 href = "mailto:dks0@cdc.gov"),
                               ".",
                               style = "color: black;")
                            ) # End column
                            ) # End fluidRow
            ) # End tabItem
            ) # End tabItems
                           ) # End dashboardBody
                           ) # End dashboardPage
                           ) # End ShinyUI
