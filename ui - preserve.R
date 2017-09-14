library(shiny)

shinyUI(fluidPage(
  
  conditionalPanel(condition="output.incorrect == true",
                   textInput("password", "Password", value="")
  ),
  
  conditionalPanel(condition="output.correct == true",
                   
                   titlePanel(HTML("Estimate the Number of HIV Transmissions<br>along the HIV Care Continuum")),
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
                                                  value = NA)
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
                                                  value = NA)
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
                              mainPanel(
                                tabsetPanel(
                                  tabPanel("Continuum Bar (Cascade)",
                                           mainPanel(
                                             plotOutput("continuumPlotCntCasc")
                                           )
                                  ),
                                  tabPanel("Continuum Bar (Mutually Exclusive)",
                                           mainPanel(
                                             plotOutput("continuumPlotCntSep")
                                           )
                                  ),
                                  tabPanel("Tx Bar",
                                           mainPanel(
                                             plotOutput("txBarCnt")
                                           )
                                  ),
                                  
                                  tabPanel("Tx Pie",
                                           mainPanel(
                                             plotOutput("txPieCnt")
                                           )
                                  ),
                                  tabPanel("Tx Table",
                                           dataTableOutput("txTableCnt")
                                  ),
                                  tabPanel("Report",
                                           mainPanel(
                                             textInput("reportTitle", label="Report title"),
                                             br(),
                                             downloadButton("downloadReport", label="Generate Report")
                                           )
                                  ))
                              )),
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
  )
)
)
