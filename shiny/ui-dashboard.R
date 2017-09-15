library("shiny")
library("shinydashboard")
library("deSolve")
suppressMessages(library("EpiModel"))
library("plotly")

# Define UI for application that draws a histogram
shinyUI(dashboardPage(

  dashboardHeader(),
  dashboardSidebar(
    width = 200,
    sidebarMenu(
      menuItem("Introduction", tabName = "Introduction", icon = icon("book")),
      menuItem("Data", tabName = "Data", icon = icon("line-chart")),
      menuItem("Export", tabName = "Export", icon = icon("book"))
    )
  ),

  dashboardBody(
    tabItems(


      # Intro Page --------------------------------------------------------------

      tabItem(tabName = "Introduction",

              fluidRow(
                column(width = 1),
                column(width = 10,
                       h3("Estimating the number of individuals indicated for PrEP",
                          style = "color: #193556;", align = "Center"),
                       h4("A Web-Based Modeling Tool for Public Health Practice",
                          style = "color: #2E619E;", align = "Center"),
                       hr(),
                       p("The aim of this app is to demonstrate the nature of inter-group disparities in infection rates over time,
                         for a very simple model of HIV transmission within and between two groups. Specifically, we aim to illustrate
                         how the ability for any phenomenon to sustain a pre-existing disparity is linked to its ability to generate
                         a disparity when none exists. Examples of the kinds of phenomena we mean are differences in care access
                         (and thus in infectiousness per contact and/or mortality rates), differences in contact rates, differences
                         in access to and use of prevention modalities like condoms (and thus in effective contact rates), and
                         assortative mixing between the two groups. If you are a returning user, you may wish to turn straight to the",
                         strong("Model."), "If you are new, we encourage you to first review the", strong("Context,"), strong("Model Structure,"),
                         "and", strong("Detailed Instructions"), "tabs. See also the", strong("Discussion"),"tab after exploring the app.
                         For those interested in the mathematical detail underlying the model, you may also visit the", strong("Equations"), "tab. The
                         code used to generate this app is located in a GitHub repository called",
                         tags$a("MSMMix", target = "_blank", href = "https://github.com/statnet/MSMmix"), ".")),
                column(width = 1)
              )
                ), #End tabItem

      tabItem(

        tabName = "Export",
        fluidRow(
          column(width = 1),
          column(width = 10,
                 h3("Context", style = "color: #2E619E;", align = "Center"),
                 hr(),
                 tags$ul(

                   tags$li( "This app was initially developed in concert with the article,",
                            em("Isolating the sources of racial disparities in HIV prevalence among men who have sex with men (MSM) in Atlanta, GA: A modeling study.")),


                   tags$li("However, it is", strong("not"), "a re-creation of the complex model in that article, but rather a simple model aimed at illustrating a general point
                           about the nature of infectious disease dynamics."),

                   tags$li("This general point pertains to the relationship between how disparities in infection burden between two groups are", strong("generated"),
                           "and how pre-existing disparities are", strong("sustained")),

                   tags$li("That is, if we know that a given disparity between groups in some causal factor (e.g. sexual behavior, care continuum)",
                           strong("generates"), "a given disparity in HIV incidence or prevalence, what does that tell us about the ability for that
                           same causal factor to", strong("sustain"), "a pre-existing disparity in incidence and prevalence over time?"),

                   tags$li("Epidemic modeling theory makes predictions about this relationship that, in our experience, many people find counter-intuitive."),

                   tags$li("In making this tool, we are motivated by the fact that existing differences in HIV prevalence by race are used in the literature
                           as explanatory factors for differences in HIV incidence. This argument goes as follows:"),

                   tags$ul(

                     tags$li("Given high levels of race assortative mixing, and higher standing prevalence among Black MSM than White MSM, an
                             individual HIV-negative Black MSM is more likely than an individual HIV-negative White MSM to have HIV-positive
                             partners, and thus to acquire HIV infection."),

                     tags$li("Indeed, partner race is a strong explanatory factor in incidence studies (e.g., Garofalo et al. 2016, Sullivan et al. 2016)."),

                     tags$li("If a pre-existing disparity plus assortative mixing together can explain short-term differences in HIV incidence, then it
                             would likely seem reasonable for many readers of this literature to imagine that the same phenomena might be sufficient to
                             sustain disparities indefinitely and thus provide the explanation for long-term disparities.")
                     ),

                   tags$li("Although this logic seems reasonable, modeling theory and practice contradict it:"),

                   tags$ul(

                     tags$li("modeling theory suggests that the short-term explanatory power of a pre-existing disparity combined with assortative mixing
                             does not generally translate into long-term explanatory power (e.g. Simon and Jacquez 1992)."),

                     tags$li("previous models have also shown this to be true in practice (e.g. Morris et al. 2008).")

                     ), #end tags$ul

                   tags$li("This line of research suggests that, in order for a pre-existing disparity to be maintained, something other than just assortative mixing
                           must be at work. One example would be higher average transmission probability per serodiscordant contact for members of one group than the
                           other. If something like this is in effect, then assortative mixing can help to maintain a disparity, and make that disparity larger than
                           it would be in the absense of assortative mixing."),

                   tags$li("We have developed this app to provide a broader audience with the tools to explore this theory in practice, and develop intuition around
                           how it works and what its implications are for understanding both short- and long-term disparities in HIV and other infectious diseases.")

                   ), #end tags$ul


                 h4("Literature Cited"),
                 tags$ul(
                   tags$li("Garofalo R., Hotton AL, Kuhns LM, Gratzer B, Mustanski B. (2016). Incidence of HIV infection and Sexually Transmitted Infections
                           and Related Risk Factors among Very Young Men Who Have Sex with Men.", em("J Acquir Immune Defic Syndr,"), "72(1):79-86. doi:10.1097/QAI.0000000000000933)",
                           tags$a("Paper Link", target = "_blank", href = "https://www.ncbi.nlm.nih.gov/pubmed/26745827")),
                   tags$li("Morris M, Kurth AE, Hamilton DT, Moody J, Wakefield S (2009). Concurrent Partnerships and HIV Prevalence Disparities by Race:
                           Linking Science and Public Health Practice.", em("American Journal of Public Health,"), "99(6) pp. 1023-1031.",
                           tags$a("Paper Link", target = "_blank", href = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2679771/")),
                   tags$li("Simon CP, Jacquez JA (1992). Reproduction Numbers and the Stability of Equilibria of SI Models for Heterogeneous Populations.",
                           em("SIAM Journal on Applied Mathematics,"), "52(2), 541-576.",
                           tags$a("Paper Link", target = "_blank", href = "https://www.jstor.org/stable/2102425")),
                   tags$li("Sullivan PS, Rosenberg ES, Sanchez TH, Kelley CF, Luisi N, Cooper HL, Diclemente RJ, Wingood GM, Frew PM, Salazar LF,
                           Del Rio C, Mulligan MJ, Peterson JL. (2015). Explaining racial disparities in HIV incidence in black and white men
                           who have sex with men in Atlanta, GA: a prospective observational cohort study.", em("Ann Epidemiol,"), "25(6), 445-454. doi:10.1016/j.annepidem.2015.03.006.",
                           tags$a("Paper Link", target = "_blank", href = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4433604/"))
                     ) #end tags$ul

                   ), # End column
          column(width = 1)
                 ) # End fluidRow
                   ), #End tabItem

      # Data Page --------------------------------------------------------------
      tabItem(
        tabName = "Data",

        ## Plot windows

        fluidRow(
          column(width = 12),
          box(width = 12,
              title = "Quick Start", status = "primary", solidHeader = TRUE,
              p("Select values for all of the model input parameters, either by accepting
                the defaults or changing a subset of interest. Press Run Model, and note the incidence
                and prevalence levels that each group heads towards over time. Now, try different
                values for the starting prevalence within each group, and run the model again.
                Notice the impact on the values of final HIV incidence and prevalence for each group.
                Try again with other parameter values."))
              ),
        fluidRow(
          column(width = 6,
                 box(width = NULL,
                     title = "Prevalence", status = "primary", solidHeader = TRUE,
                     plotlyOutput("plot1"))),
          column(width = 6,
                 box(width = NULL,
                     title = "Incidence", status = "primary", solidHeader = TRUE,
                     plotlyOutput("plot2")))
        ),
        fluidRow(
          column(width = 12, align = "Center",
                 actionButton(inputId = "runMod", "Run Model")
          )
        ),
        hr(),

        # Inputs
        fluidRow(
          column(width = 4,
                 box(width = NULL,
                     title = "Initial Conditions", status = "warning", solidHeader = TRUE,
                     numericInput(inputId = "Num.g1", label = "Population Size, Group 1",
                                  min = 0, value = 1000),
                     numericInput(inputId = "prevalence.g1", label = "Proportion Infected, Group 1 ",
                                  min = 0, max = 1, value = 0.01, step = 0.01),
                     numericInput(inputId = "Num.g2", label = "Population Size, Group 2",
                                  min = 0, value = 1000),
                     numericInput(inputId = "prevalence.g2", label = "Proportion Infected, Group 2",
                                  min = 0, max = 1, value = 0.01, step = 0.01)
                 ),

                 box(width = NULL,
                     title = "Control Settings", status = "warning", solidHeader = TRUE,
                     numericInput(inputId = "nsteps", label = "Time Steps",
                                  min = 0, value = 500),
                     numericInput(inputId = "dt", label = "Length of Time Step",
                                  min = 0, value = 0.1)
                 )
          ),

          column(width = 4,
                 box(width = NULL,
                     title = "Contact and Transmission", status = "success", solidHeader = TRUE,
                     sliderInput(inputId = "Q",
                                 label = "Q Mixing Statistic (0 = proportional, 1 = fully assortative)",
                                 min = 0, max = 1, value = 0, step = 0.01),
                     sliderInput(inputId = "c.g1",
                                 label = "Contact Rate, Group 1",
                                 min = 0, max = 10, value = 2, step = 0.1),
                     sliderInput(inputId = "c.g2", label = "Contact Rate, Group 2",
                                 min = 0, max = 10, value = 2, step = 0.1),
                     sliderInput(inputId = "rho.g1", label = "Transmission Rate, Group 1",
                                 min = 0, max = 1, value = 0.05, step = 0.005),
                     sliderInput(inputId = "rho.g2", label = "Transmission Rate, Group 2",
                                 min = 0, max = 1, value = 0.05, step = 0.005)
                 )
          ),

          column(width = 4,
                 box(width = NULL,
                     title = "Demographics", status = "success", solidHeader = TRUE,

                     sliderInput(inputId = "b.rate", label = "Birth Rate",
                                 min = 0, max = 1, value = 0.03, step = 0.01),
                     sliderInput(inputId = "muS.g1",
                                 label = "Death Rate, Susceptible, Group 1 ",
                                 min = 0, max = 1, value = 0.03, step = 0.01),
                     sliderInput(inputId = "muI.g1",
                                 label = "Death Rate, Infected, Group 1",
                                 min = 0, max = 1, value = 0.03, step = 0.01),
                     sliderInput(inputId = "muS.g2",
                                 label = "Death Rate, Susceptible, Group 2",
                                 min = 0, max = 1, value = 0.03, step = 0.01),
                     sliderInput(inputId = "muI.g2",
                                 label = "Death Rate, Infected, Group 2",
                                 min = 0, max = 1, value = 0.03, step = 0.01)
                 )
          )

        ) # end fluidRow
              ) # end tabItem
              ) # end tabItems
      ) # end dashboardBody
          )
      )


library(shiny)

shinyUI(fluidPage(

  conditionalPanel(condition="output.incorrect == true",
                   textInput("password", "Password", value="")
  ),

  conditionalPanel(condition="output.correct == true",
                   tabsetPanel(
                     tabPanel("HIV Care Continuum",
                              titlePanel(HTML("Estimate the HIV Care Continuum")),
                              h5("Start on the left and work your way to the right!"),
                              br(),
                              tabsetPanel(
                                tabPanel("Basic Settings",
                                         column(3,
                                                #          HTML("<div style=\"width:3;height:3;border:3px solid #000;background:#46B7EB\"><h4 style=\"color:#2A268B\">1. Basic Inputs</h4>"),
                                                HTML("<h4 style=\"color:#2A268B\">1. Basic Inputs & Assumptions</h4>"),

                                                numericInput("diagnosedcnt1",
                                                             "Living with HIV diagnosis (#)",
                                                             min=0,
                                                             value=NA),

                                                selectInput("pctassumption", label="Population (percentages)",
                                                            c("Total" = 1,
                                                              "Male" = 2, "Female" = 3,
                                                              "MSM" = 4, "IDU: Male" = 5, "IDU: Female" = 6, "IDU: MSM" = 7, "Heterosexual male" = 8, "Heterosexual female" = 9,
                                                              "Age group: 25-34" = 10, "Age group: 35-44" = 11, "Age group: 45-54" = 12, "Age group: 55-64" = 13, "Age group: 65+" = 14
                                                            ),
                                                            selected = 1),

                                                selectInput("rateassumption", label="Population (rates)",
                                                            c("Total" = 1,
                                                              "Male" = 2, "Female" = 3,
                                                              "MSM" = 4, "IDU: Male" = 5, "IDU: Female" = 6, "IDU: MSM" = 7, "Heterosexual male" = 8, "Heterosexual female" = 9,
                                                              "Age group: 25-34" = 10, "Age group: 35-44" = 11, "Age group: 45-54" = 12, "Age group: 55-64" = 13, "Age group: 65+" = 14
                                                            ),
                                                            selected = 1)
                                         ),

                                         column(3,
                                                #          HTML("<div style=\"width:3;height:3;border:3px solid #000;background:#46B7EB\"><h4 style=\"color:#2A268B\">2. Local Continuum Information</h4>"),
                                                HTML("<h4 style=\"color:#2A268B\">2. Continuum Information</h4>"),

                                                HTML("<h5 style=\"color:green\">Of <u>all HIV-positive persons</u>:</h3>"),
                                                numericInput("undiagnosedpct",
                                                             "% Undiagnosed",
                                                             min = 0,
                                                             value = NA),

                                                HTML("<h5 style=\"color:green\">Of HIV-positive persons <u>who have been diagnosed</u>:</h3>"),
                                                numericInput("diagnosedretpct",
                                                             "% Retained in care",
                                                             min = 0,
                                                             value = NA),

                                                numericInput("diagnosedarvpct",
                                                             "% Prescribed ARVs",
                                                             min = 0,
                                                             value = NA),

                                                numericInput("diagnosedsuppct",
                                                             "% Suppressed",
                                                             min = 0,
                                                             value = NA),

                                                HTML("<font color=\"red\"><b>"), textOutput("warningText1"), HTML("</font></b>")
                                                #          ,HTML("</div>")
                                         ),

                                         column(3,
                                                HTML("<h4 style=\"color:#2A268B\">3. HIV Continuum (Cascade)</h4>"),

                                                numericInput("totalcnt",
                                                             "Total HIV-positive (#)",
                                                             min = 0,
                                                             value = NA),

                                                numericInput("diagnosedcnt2",
                                                             "Diagnosed (#)",
                                                             min = 0,
                                                             value = NA),

                                                numericInput("diagnosedretcnt",
                                                             "Diagnosed, retained in care (#)",
                                                             min = 0,
                                                             value = NA),

                                                numericInput("diagnosedarvcnt",
                                                             "Diagnosed, prescribed ARVs (#)",
                                                             min = 0,
                                                             value = NA),

                                                numericInput("diagnosedsupcnt",
                                                             "Diagnosed, suppressed (#)",
                                                             min = 0,
                                                             value = NA),
                                                HTML("<font color=\"red\"><b>"), textOutput("warningText2"), HTML("</font></b>")
                                         ),

                                         column(3,
                                                HTML("<h4 style=\"color:#2A268B\">4. HIV Continuum (Mutually Exclusive)</h4>"),
                                                numericInput("undiagnosedcnt.mutex",
                                                             "Undiagnosed (#)",
                                                             min = 0,
                                                             value = NA),

                                                numericInput("diagnosednrcnt.mutex",
                                                             "Diagnosed, not retained in care (#)",
                                                             min = 0,
                                                             value = NA),

                                                numericInput("diagnosedretcnt.mutex",
                                                             "Diagnosed, retained in care, not prescribed ARVs (#)",
                                                             min = 0,
                                                             value = NA),

                                                numericInput("diagnosedarvcnt.mutex",
                                                             "Diagnosed, prescribed ARVs, not suppressed (#)",
                                                             min = 0,
                                                             value = NA),

                                                numericInput("diagnosedsupcnt.mutex",
                                                             "Diagnosed, suppressed (#)",
                                                             min = 0,
                                                             value = NA)

                                         ),
                                         conditionalPanel(condition="output.warningText1 == \"\" & output.warningText2 == \"\"",
                                                          mainPanel(
                                                            tabsetPanel(
                                                              tabPanel("Current Scenario",
                                                                       tabsetPanel(
                                                                         tabPanel("Continuum Bar Chart (Cascade)",
                                                                                  mainPanel(
                                                                                    plotOutput("continuumPlotCntCasc")
                                                                                  )
                                                                         ),
                                                                         tabPanel("Continuum Bar Chart (Mutually Exclusive)",
                                                                                  mainPanel(
                                                                                    plotOutput("continuumPlotCntSep")
                                                                                  )
                                                                         ),
                                                                         tabPanel("Report",
                                                                                  mainPanel(
                                                                                    textInput("reportTitle", label="Report title"),
                                                                                    br(),
                                                                                    downloadButton("downloadReport", label="Generate Report")
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
                                                                                  column(1.2, selectInput("removeScenarioNum", "Select scenario #", choices=0),br(),br(),br(),br(),br(),br()),
                                                                                  #                                                  column(1, HTML("<h5 style=\"color:#006600\">OR</h5>"),br(),br(),br(),br(),br(),br()),
                                                                                  #                                                  column(1.2, selectInput("removeScenarioName", "Select scenario name", choices=""),br(),br(),br(),br(),br(),br()),
                                                                                  column(2, br(), actionButton("removeButton", "Remove Scenario"),br(),br(),br(),br(),br(),br())
                                                                         ),
                                                                         tabPanel("Compare",
                                                                                  column(1.5, selectInput("compareScenario1", "Select scenario", choices=0)),
                                                                                  column(1.5, selectInput("compareScenario2", "Select scenario", choices=0)),
                                                                                  column(1.5, br(), actionButton("compareScenarios", "Compare")),
                                                                                  br(),
                                                                                  column(5, plotOutput("compareTxBar")),
                                                                                  column(2, br(), br(), br(), textOutput("comparisonText1"))
                                                                         )
                                                                       )

                                                              )
                                                            ),
                                                            width=20
                                                          ))),
                                tabPanel("Advanced Settings",
                                         column(3,
                                                HTML("<h4 style=\"color:#2A268B\">Custom Transmission Rates</h4>"),
                                                selectInput("existingrate", label="Start with existing rates (optional)",
                                                            c(" " = NA, "Total" = 1,
                                                              "Male" = 2, "Female" = 3,
                                                              "MSM" = 4, "IDU: Male" = 5, "IDU: Female" = 6, "IDU: MSM" = 7, "Heterosexual male" = 8, "Heterosexual female" = 9,
                                                              "Age group: 25-34" = 10, "Age group: 35-44" = 11, "Age group: 45-54" = 12, "Age group: 55-64" = 13, "Age group: 65+" = 14
                                                            ),
                                                            selected = 0),
                                                numericInput("customUndiagnosedTxRate", "Undiagnosed Tx rate", min=0, value=NA),
                                                numericInput("customDiagnosedNRTxRate", "Diagnosed (not retained in care) Tx rate", min=0, value=NA),
                                                numericInput("customDiagnosedRetTxRate", "Diagnosed, retained in care (not prescribed ARVs) Tx rate", min=0, value=NA),
                                                numericInput("customDiagnosedARVTxRate", "Diagnosed, prescribed ARVs (not suppressed) Tx rate", min=0, value=NA),
                                                numericInput("customDiagnosedSupTxRate", "Diagnosed, suppressed Tx rate", min=0, value=NA)
                                         )
                                )
                              )
                     ),
                     tabPanel("HIV Transmissions",
                              tabsetPanel(
                                tabPanel("Tx Bar Chart",
                                         mainPanel(
                                           plotOutput("txBarCnt")
                                         )
                                ),

                                tabPanel("Tx Pie Chart",
                                         mainPanel(
                                           plotOutput("txPieCnt")
                                         )
                                ),
                                tabPanel("Tx Table",
                                         dataTableOutput("txTableCnt")
                                )
                              )
                     )
                   )
  )
)
)
