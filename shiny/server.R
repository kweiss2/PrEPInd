## Packages and options --------------------------------------------------------
library(shiny)
library(knitr)
library(rmarkdown)

options("scipen" = 100)

## Group names -----------------------------------------------------------------
stagenames.transcat <- c("Total", "MSM", "HET", "PWID")
stagenames.racetranscat <- c("Black MSM", "Black HET",
                      "Black PWID", "Hispanic MSM", "Hispanic HET",
                      "Hispanic PWID", "White MSM", "White HET", "White PWID")
colors <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0")

values <- reactiveValues()
values$df <- data.frame(t(rep(0, 8))) # May need to be edited


## Diagnosed percentages by jurisdiction ---------------------------------------
#Diagnoses split by percent of total by jurisdiction:
# Sum to 100 within race
# Black MSM, Black HET, Black PWID,
# Hispanic MSM, Hispanic HET, Hispanic PWID
# White MSM, White HET, White PWID
race.diagnosis.percents <- list(
  c(61.8, 32.9, 5.3, 78.2, 16.5, 5.3, 78.7, 12.1, 9.2), # Natl. Avg
  c(62.7, 34.1, 3.2, 78.0, 17.1, 4.9, 69.7, 21.3, 9.0), # Alabama
  c(50.0, 50.0, 0.0, 100, 0.0, 0.0, 74.7, 25.3, 0.0), # Alaska
  c(56.2, 37.7, 6.1, 78.1, 12.5, 9.4, 82.1, 9.1, 8.8), # Arizona
  c(67.5, 26.7, 5.8, 62.9, 31.4, 5.7, 81.0, 11.5, 7.5), # Arkansas
  c(71.2, 23.0, 5.8, 85.9, 10.7, 3.4, 85.5, 7.2, 7.3), # California
  c(58.3, 36.6, 5.1, 82.6, 13.0, 4.4, 90.3, 7.0, 2.7), # Colorado
  c(43.5, 50.6, 6.0, 63.7, 31.3, 4.9, 81.2, 9.4, 9.4), # Connecticut
  c(50.0, 48.1, 1.9, 41.9, 41.9, 16.3, 78.1, 19.0, 2.9), # Delaware
  c(48.7, 48.0, 3.3, 77.6, 19.7, 2.6, 72.6, 19.1, 8.3), # Florida
  c(68.3, 27.7, 3.9, 82.0, 16.8, 1.2, 76.9, 17.3, 5.8), # Georgia
  c(72.3, 8.5, 19.1, 79.1, 20.9, 0.0, 75.4, 14.9, 9.7), # Hawaii
  c(11.0, 78.0, 11.0, 100.0, 0.0, 0.0, 89.6, 10.4, 0.0), # Idaho
  c(69.3, 26.6, 4.1, 86.3, 12.0, 1.7, 85.1, 10.1, 4.7), # Illinois
  c(68.4, 29.1, 2.5, 70.3, 25.9, 3.8, 40.9, 10.9, 48.2), # Indiana
  c(53.8, 46.3, 0.0, 68.5, 18.5, 13.0, 74.8, 13.2, 12.0), # Iowa
  c(56.8, 38.5, 4.5, 85.8, 11.3, 2.8, 86.0, 12.2, 1.7), # Kansas
  c(66.9, 29.9, 3.2, 91.7, 8.3, 0.0, 80.2, 12.1, 7.7), # Kentucky
  c(58.5, 34.8, 6.7, 65.3, 25.3, 9.3, 75.7, 13.6, 10.7), # Louisiana
  c(25.0, 67.0, 8.0, 81.1, 0.0, 18.9, 88.5, 0.0, 11.5), # Maine
  c(53.6, 41.8, 4.6, 67.8, 28.2, 4.0, 70.1, 17.2, 12.7), # Maryland
  c(32.0, 55.8, 12.2, 56.7, 28.9, 14.4, 76.0, 10.5, 13.4), # Massachusetts
  c(71.6, 23.4, 5.0, 83.2, 14.3, 2.5, 79.3, 12.2, 8.5), # Michigan
  c(49.2, 46.0, 4.9, 83.2, 12.5, 4.3, 87.4, 9.2, 3.4), # Minnesota
  c(68.8, 27.3, 4.0, 71.4, 28.6, 0.0, 76.0, 20.0, 4.0), # Mississippi
  c(65.0, 31.4, 3.7, 72.9, 17.6, 69.4, 83.4, 9.6, 7.1), # Missouri
  c(100.0, 0.0, 0.0, 100.0, 0.0, 0.0, 73.0, 20.1, 6.9), # Montana
  c(52.9, 41.2, 5.9, 81.8, 18.2, 0.0, 87.1, 12.9, 0.0), # Nebraska
  c(66.4, 28.7, 4.9, 89.2, 8.8, 2.0, 80.2, 9.6, 10.2), # Nevada
  c(NA, NA, NA, NA, NA, NA, NA, NA, NA), # New Hampshire
  c(45.9, 42.3, 11.8, 66.6, 25.3, 8.0, 68.3, 17.3, 14.3), # New Jersey
  c(100.0, 0.0, 0.0, 75.6, 9.0, 15.4, 94.0, 5.2, 0.0), # New Mexico
  c(51.6, 39.0, 9.4, 75.2, 17.0, 7.8, 81.8, 11.7, 6.5), # New York
  c(66.8, 29.0, 4.2, 77.8, 20.7, 1.5, 79.5, 15.2, 5.3), # North Carolina
  c(17.5, 73.0, 9.5, 50.0, 0.0, 50.0, 85.0, 0.0, 15.0), # North Dakota
  c(69.3, 26.9, 3.8, 73.6, 20.4, 6.0, 78.0, 12.6, 9.3), # Ohio
  c(74.5, 24.1, 1.4, 85.8, 11.7, 2.5, 79.9, 12.2, 8.0), # Oklahoma
  c(50.0, 50.0, 0.0, 84.8, 9.1, 6.1, 83.0, 6.1, 10.9), # Oregon
  c(55.0, 40.4, 4.6, 47.7, 39.5, 12.8, 71.9, 19.5, 8.6), # Pennsylvania
  c(29.2, 64.6, 6.2, 66.9, 23.8, 9.4, 78.3, 17.1, 4.6), # Rhode Island
  c(63.6, 30.6, 5.8, 68.3, 20.6, 11.1, 75.9, 19.2, 4.9), # South Carolina
  c(33.3, 66.7, 0.0, 33.3, 33.3, 33.3, 72.3, 27.7, 0.0), # South Dakota
  c(66.1, 30.6, 3.3, 76.3, 23.7, 0.0, 69.6, 19.5, 10.9), # Tennessee
  c(63.5, 31.0, 5.5, 79.1, 17.0, 4.0, 80.3, 12.2, 7.4), # Texas
  c(38.3, 61.7, 0.0, 83.6, 13.1, 3.3, 88.3, 7.3, 4.5), # Utah
  c(NA, NA, NA, 50.0, 0.0, 50.0, 44.6, 33.2, 22.3), # Vermont
  c(63.5, 33.6, 2.9, 80.2, 17.0, 2.8, 79.4, 16.1, 4.5), # Virginia
  c(56.6, 37.8, 5.5, 86.8, 7.2, 6.0, 82.2, 6.8, 10.9), # Washington
  c(25.5, 66.7, 7.8, 100.0, 0.0, 0.0, 83.9, 12.6, 3.5), # West Virginia
  c(74.4, 22.4, 3.2, 7.7, 14.4, 5.9, 85.8, 10.6, 3.5), # Wisconsin
  c(50.0, 0.0, 50.0, 67.4, 0.0, 32.6, 50.4, 37.4, 12.2), # Wyoming
  c(52.5, 41.0, 6.5, 95.8, 2.1, 2.1, 94.8, 2.6, 2.6)  # Washington D.C
)

trans.diagnosis.percents <- list(
  c(71.1, 22.5, 6.3), # Natl. Avg
  c(64.9, 30.7, 4.4), # Alabama
  c(62.3, 37.3, 0.0), # Alaska
  c(74.6, 16.7, 8.6), # Arizona
  c(72.7, 21.0, 6.3), # Arkansas
  c(83.1, 11.9, 5.0), # California
  c(82.7, 13.7, 3.5), # Colorado
  c(60.5, 32.1, 7.4), # Connecticut
  c(59.6, 36.7, 3.7), # Delaware
  c(63.9, 31.8, 4.3), # Florida
  c(71.0, 25.0, 4.0), # Georgia
  c(75.7, 14.7, 9.6), # Hawaii
  c(71.0, 25.0, 4.0), # Idaho
  c(77.3, 19.1, 3.6), # Illinois
  c(52.5, 18.9, 28.7), # Indiana
  c(67.9, 22.5, 9.5), # Iowa
  c(77.5, 19.1, 3.4), # Kansas
  c(74.7, 20.0, 5.4), # Kentucky
  c(62.6, 29.5, 7.8), # Louisiana
  c(68.3, 20.3, 11.4), # Maine
  c(57.3, 37.0, 5.7), # Maryland
  c(57.9, 29.4, 12.7), # Massachusetts
  c(75.2, 19.0, 5.8), # Michigan
  c(69.6, 26.2, 4.2), # Minnesota
  c(69.5, 26.5, 4.0), # Mississippi
  c(73.7, 20.9, 5.4), # Missouri
  c(75.2, 15.2, 10.0), # Montana
  c(78.1, 19.4, 2.4), # Nebraska
  c(80.0, 14.4, 5.6), # Nevada
  c(71.3, 14.3, 14.3), # New Hampshire
  c(57.8, 31.4, 10.8), # New Jersey
  c(81.4, 8.9, 9.6), # New Mexico
  c(67.1, 25.0, 7.9), # New York
  c(71.0, 24.9, 4.1), # North Carolina
  c(47.7, 38.6, 12.9), # North Dakota
  c(72.9, 20.9, 6.2), # Ohio
  c(78.4, 16.3, 5.4), # Oklahoma
  c(79.1, 11.4, 9.5), # Oregon
  c(59.3, 33.7, 7.0), # Pennsylvania
  c(60.9, 32.8, 6.1), # Rhode Island
  c(66.8, 27.1, 6.1), # South Carolina
  c(55.0, 41.1, 4.7), # South Dakota
  c(67.9, 26.2, 5.9), # Tennessee
  c(73.4, 21.3, 5.3), # Texas
  c(82.7, 12.9, 4.2), # Utah
  c(41.6, 33.5, 24.9), # Vermont
  c(69.5, 27.2, 3.3), # Virginia
  c(77.5, 14.0, 8.6), # Washington
  c(72.5, 20.6, 6.9), # West Virginia
  c(80.8, 15.7, 3.5), # Wisconsin
  c(49.8, 28.6, 21.2), # Wyoming
  c(64.0, 30.5, 5.5)  # Washington D.C

)

shinyServer(function(input, output, session) {

  ## Get diagnosis percents ----------------------------------------------------
  observe({
    # Transmission category within race
    x <- race.diagnosis.percents[[as.numeric(input$jurisdiction)]]
    updateNumericInput(session, "blackmsmdiagpct", value = x[1])
    updateNumericInput(session, "blackhetdiagpct", value = x[2])
    updateNumericInput(session, "blackpwiddiagpct", value = x[3])
    updateNumericInput(session, "hispmsmdiagpct", value = x[4])
    updateNumericInput(session, "hisphetdiagpct", value = x[5])
    updateNumericInput(session, "hisppwiddiagpct", value = x[6])
    updateNumericInput(session, "whitemsmdiagpct", value = x[7])
    updateNumericInput(session, "whitehetdiagpct", value = x[8])
    updateNumericInput(session, "whitepwiddiagpct", value = x[9])

    # Transmission category only
    y <- trans.diagnosis.percents[[as.numeric(input$jurisdiction)]]
    updateNumericInput(session, "msmdiagpct", value = y[1])
    updateNumericInput(session, "hetdiagpct", value = y[2])
    updateNumericInput(session, "pwiddiagpct", value = y[3])
  })

  observe({

    ## Calculate number indicated for PrEP -------------------------------------
    totmsm <- input$msmpopsize
    totmsm2 <- input$msmpopsize2
    x <- race.diagnosis.percents[[as.numeric(input$existingprop)]]
    y <- trans.diagnosis.percents[[as.numeric(input$jurisdiction)]]
    z <- trans.diagnosis.percents[[as.numeric(input$existingprop)]]

    updateNumericInput(session, "totalmsm",
                       value = round(totmsm, 0))
    updateNumericInput(session, "msmprep",
                       value = round((totmsm * 0.247), 0))
    updateNumericInput(session, "hetprep",
                       value = round(((totmsm * 0.247) * (y[2] / y[1])), 0))
    updateNumericInput(session, "pwidprep",
                       value = round(((totmsm * 0.247) * (y[3] / y[1])), 0))
    updateNumericInput(session, "totalprep",
                       value = round((totmsm * 0.247), 0) +
                         round(((totmsm * 0.247) * (y[2] / y[1])), 0) +
      round(((totmsm * 0.247) * (y[3] / y[1])), 0))
    updateNumericInput(session, "blackmsmprep",
                       value = round((totmsm2 * 0.247) * (x[1] / 100), 0))
    updateNumericInput(session, "blackhetprep",
                       value = round((totmsm2 * 0.247) * (z[2] / z[1]) * (x[2] / 100), 0))
    updateNumericInput(session, "blackpwidprep",
                       value = round((totmsm2 * 0.247) * (z[3] / z[1]) * (x[3] / 100), 0))
    updateNumericInput(session, "hispmsmprep",
                       value = round((totmsm2 * 0.247) * (x[4] / 100), 0))
    updateNumericInput(session, "hisphetprep",
                       value = round((totmsm2 * 0.247) * (z[2] / z[1]) * (x[5] / 100), 0))
    updateNumericInput(session, "hisppwidprep",
                       value = round((totmsm2 * 0.247) * (z[3] / z[1]) * (x[6] / 100), 0))
    updateNumericInput(session, "whitemsmprep",
                       value = round((totmsm2 * 0.247) * (x[7] / 100), 0))
    updateNumericInput(session, "whitehetprep",
                       value = round((totmsm2 * 0.247) * (z[2] / z[1]) * (x[8] / 100), 0))
    updateNumericInput(session, "whitepwidprep",
                       value = round((totmsm2 * 0.247) * (z[3] / z[1]) * (x[9] / 100), 0))

    if (input$existingprop == 46) {
      updateNumericInput(session, "blackmsmprep",
                         value = NA)
      updateNumericInput(session, "blackhetprep",
                         value = NA)
      updateNumericInput(session, "blackpwidprep",
                         value = NA)
    }

    if (input$existingprop == 30) {
      updateNumericInput(session, "blackmsmprep",
                         value = NA)
      updateNumericInput(session, "blackhetprep",
                         value = NA)
      updateNumericInput(session, "blackpwidprep",
                         value = NA)
      updateNumericInput(session, "hispmsmprep",
                         value = NA)
      updateNumericInput(session, "hisphetprep",
                         value = NA)
      updateNumericInput(session, "hisppwidprep",
                         value = NA)
      updateNumericInput(session, "whitemsmprep",
                         value = NA)
      updateNumericInput(session, "whitehetprep",
                         value = NA)
      updateNumericInput(session, "whitepwidprep",
                         value = NA)
    }

  })


  observe({
    if (is.na(input$existingprop) == FALSE) {

      updateNumericInput(session, "customblackmsmdiagpct", value = race.diagnosis.percents[[as.numeric(input$existingprop)]][1])
      updateNumericInput(session, "customblackhetdiagpct", value = race.diagnosis.percents[[as.numeric(input$existingprop)]][2])
      updateNumericInput(session, "customblackpwiddiagpct", value = race.diagnosis.percents[[as.numeric(input$existingprop)]][3])
      updateNumericInput(session, "customhispmsmdiagpct", value = race.diagnosis.percents[[as.numeric(input$existingprop)]][4])
      updateNumericInput(session, "customhisphetdiagpct", value = race.diagnosis.percents[[as.numeric(input$existingprop)]][5])
      updateNumericInput(session, "customhisppwiddiagpct", value = race.diagnosis.percents[[as.numeric(input$existingprop)]][6])
      updateNumericInput(session, "customwhitemsmdiagpct", value = race.diagnosis.percents[[as.numeric(input$existingprop)]][7])
      updateNumericInput(session, "customwhitehetdiagpct", value = race.diagnosis.percents[[as.numeric(input$existingprop)]][8])
      updateNumericInput(session, "customwhitepwiddiagpct", value = race.diagnosis.percents[[as.numeric(input$existingprop)]][9])
    }
  })
  ## Warning text --------------------------------------------------------------

  output$warningText1 <- renderText({
  warning.text <- ""
    if (input$existingprop == 46) {
       warning.text <- "African-American estimates by transmission category are not available for Vermont"
     }
    if (input$existingprop == 30) {
      warning.text <- "Race-specific estimates by transmission category are not available for New Hampshire"
    }
    #   if (input$blackpwiddiagpct != x[3]) {
  #     warning.text <- "You are changing proportions."
  #   }

  # output$warningText1 <- renderText({
  #   warning.text <- ""
  #   if (input$blackmsmdiagpct != x[1]) {
  #     warning.text <- "You are changing proportions."
  #   }
  #   if (input$blackhetdiagpct != x[2]) {
  #     warning.text <- "You are changing proportions."
  #   }
  #   if (input$blackpwiddiagpct != x[3]) {
  #     warning.text <- "You are changing proportions."
  #   }
  #   if (input$hispmsmdiagpct != x[4]) {
  #     warning.text <- "You are changing proportions."
  #   }
  #   if (input$hisphetdiagpct != x[5]) {
  #     warning.text <- "You are changing proportions."
  #   }
  #   if (input$hisppwiddiagpct != x[6]) {
  #     warning.text <- "You are changing proportions."
  #   }
  #   if (input$whitemsmdiagpct != x[7]) {
  #     warning.text <- "You are changing proportions."
  #   }
  #   if (input$whitehetdiagpct != x[8]) {
  #     warning.text <- "You are changing proportions."
  #   }
  #   if (input$whitepwiddiagpct != x[9]) {
  #     warning.text <- "You are changing proportions."
  #   }
  #   return(warning.text)
  # })
  #
  # output$warningText2 <- renderText({
  #   warning.text <- ""
  #
  #   if (is.na(input$totalcnt) == FALSE &
  #       is.na(input$diagnosedcnt2) == FALSE &
  #       is.na(input$diagnosedretcnt) == FALSE &
  #       is.na(input$diagnosedarvcnt) == FALSE &
  #       is.na(input$diagnosedsupcnt) == FALSE) {
  #
  #     if (input$diagnosedcnt2 > input$totalcnt) {
  #       warning.text <- "The number diagnosed cannot be greater than the total number living with HIV."
  #     }
  #     if (input$diagnosedretcnt > input$diagnosedcnt2) {
  #       warning.text <- "The number retained in care cannot be greater than the number diagnosed."
  #     }
  #     if (input$diagnosedarvcnt > input$diagnosedretcnt) {
  #       warning.text <- "The number prescribed antiretroviral medication cannot be greater than the number retained in care."
  #     }
  #     if (input$diagnosedsupcnt > input$diagnosedarvcnt) {
  #       warning.text <- "The number virally suppressed cannot be greater than the number on antiretroviral medication."
  #     }
  #   }
    return(warning.text)
  })

  ## Calculate data for output table -------------------------------------------
  currentData <- reactive({

    # Input custom data
    if (is.na(input$customblackmsmdiagpct) == FALSE &
        is.na(input$customblackhetdiagpct) == FALSE &
        is.na(input$customblackpwiddiagpct) == FALSE &
        is.na(input$customhispmsmdiagpct) == FALSE &
        is.na(input$customhisphetdiagpct) == FALSE &
        is.na(input$customhisppwiddiagpct) == FALSE &
        is.na(input$customwhitemsmdiagpct) == FALSE &
        is.na(input$customwhitehetdiagpct) == FALSE &
        is.na(input$customwhitepwiddiagpct) == FALSE) {
      proportions <- c(input$customblackmsmdiagpct, input$customblackhetdiagpct,
                 input$customblackpwiddiagpct, input$customhispmsmdiagpct,
                 input$customhisphetdiagpct, input$customhisppwiddiagpct,
                 input$customwhitemsmdiagpct, input$customwhitehetdiagpct,
                 input$customwhitepwiddiagpct)
    }
    else {
      proportions <- race.diagnosis.percents[[as.numeric(input$jurisdiction)]]
    }
      totalind <- 1000000
      nind <- round((proportions / 100) * totalind)
      txTableCnt <- cbind(stagenames.racetranscat,
                          format(nind, digits = 12, decimal.mark = ",",
                                 big.mark = ",", small.mark = "."),
                          paste(as.character(proportions),
                                "%",
                                sep = "")
                          )
      colnames(txTableCnt) <- c("Group", "#", "Proportion of diagnosed (sums to 100% within race)")

      return(txTableCnt)
    # }
  })

  #http://stackoverflow.com/questions/23236944/add-values-to-a-reactive-table-in-shiny/23243820#23243820

  ## Save and remove and compare scenarios -------------------------------------
  observe({
    if (input$saveButton > 0) {
      newData <- isolate(cbind(input$saveButton, input$scenarioName, currentData()))
      isolate(colnames(newData) <- c("Scenario", "Name", "Stage", "#", "
                                     % of HIV+", "Rate/100PY", "# Tx", "% of Tx"))
      isolate(colnames(values$df) <- c("Scenario", "Name", "Stage", "#",
                                       "% of HIV+", "Rate/100PY", "# Tx", "% of Tx"))
      isolate(values$df <- rbind(values$df, newData))
      isolate(values$df <- values$df[which(values$df$Scenario != 0), ])

      isolate(updateSelectInput(session, "removeScenarioNum",
                                choices = unique(values$df$"Scenario")))
      isolate(updateSelectInput(session, "compareScenario1",
                                choices = unique(values$df$"Scenario")))
      isolate(updateSelectInput(session, "compareScenario2",
                                choices = unique(values$df$"Scenario")))
      isolate(updateTextInput(session, "scenarioName", value = ""))
    }
  })

  observe({
    if (input$removeButton > 0) {
      isolate(values$df <- values$df[which(values$df$"Scenario" != input$removeScenarioNum), ])
    }
  })

  ## Render output -------------------------------------------------------------
  output$txTableCnt <- renderDataTable({currentData()})

  #  output$fullDataTable <- renderDataTable(values$df, options = list(pagelength = 5))

  # output$scenarioSummaryTable <- renderTable({
  #   if (input$saveButton > 0) {
  #     scenarios <- unique(as.numeric(as.character(values$df$"Scenario")))
  #     scenario.names <- unique(as.character(values$df$"Name"))
  #     summary.data <- data.frame(matrix(nrow = length(scenarios), ncol = 14))
  #
  #     # Cases
  #     total.hiv.cases <- aggregate(as.numeric(as.character(gsub(",", "", values$df$"#"))),
  #                                  by = list(as.character(values$df$Scenario)),
  #                                  FUN = sum)[2]
  #
  #     undiagnosed.cases <- round(as.numeric(as.character(gsub(",", "",
  #                                                             values$df$"#"[which(values$df$"Stage" == stagenames.mutex[1])]))))
  #     diagnosed.nr.cases <- round(as.numeric(as.character(gsub(",", "",
  #                                                              values$df$"#"[which(values$df$"Stage" == stagenames.mutex[2])]))))
  #     retained.cases <- round(as.numeric(as.character(gsub(",", "",
  #                                                          values$df$"#"[which(values$df$"Stage" == stagenames.mutex[3])]))))
  #     arv.cases <- round(as.numeric(as.character(gsub(",", "",
  #                                                     values$df$"#"[which(values$df$"Stage" == stagenames.mutex[4])]))))
  #     suppressed.cases <- round(as.numeric(as.character(gsub(",", "",
  #                                                            values$df$"#"[which(values$df$"Stage" == stagenames.mutex[5])]))))
  #
  #     undiagnosed.percent <- round((undiagnosed.cases / total.hiv.cases) * 100, 1)
  #     diagnosed.nr.percent <- round((diagnosed.nr.cases / total.hiv.cases) * 100, 1)
  #     retained.percent <- round((retained.cases / total.hiv.cases) * 100, 1)
  #     arv.percent <- round((arv.cases / total.hiv.cases) * 100, 1)
  #     suppressed.percent <- round((suppressed.cases / total.hiv.cases) * 100, 1)
  #
  #     total.hiv.cases.display <- lapply(1:nrow(total.hiv.cases),
  #                                       function(y)
  #                                         paste(format(total.hiv.cases[y, 1],
  #                                                      digits = 12, decimal.mark = ",",
  #                                                      big.mark = ",", small.mark = ".")))
  #     undiagnosed.cases.display <- lapply(1:length(undiagnosed.cases),
  #                                         function(y)
  #                                           paste(format(undiagnosed.cases[y],
  #                                                        digits = 12, decimal.mark = ",",
  #                                                        big.mark = ",", small.mark = "."),
  #                                                 " (", undiagnosed.percent[y, 1],
  #                                                 "%)", sep = ""))
  #     diagnosed.nr.cases.display <- lapply(1:length(diagnosed.nr.cases),
  #                                          function(y)
  #                                            paste(format(diagnosed.nr.cases[y],
  #                                                         digits = 12, decimal.mark = ",",
  #                                                         big.mark = ",", small.mark = "."),
  #                                                  " (", diagnosed.nr.percent[y, 1],
  #                                                  "%)", sep = ""))
  #     retained.cases.display <- lapply(1:length(retained.cases),
  #                                      function(y)
  #                                        paste(format(retained.cases[y],
  #                                                     digits = 12, decimal.mark = ",",
  #                                                     big.mark = ",", small.mark = "."),
  #                                              " (", retained.percent[y, 1],
  #                                              "%)", sep = ""))
  #     arv.cases.display <- lapply(1:length(arv.cases),
  #                                 function(y)
  #                                   paste(format(arv.cases[y],
  #                                                digits = 12, decimal.mark = ",",
  #                                                big.mark = ",", small.mark = "."),
  #                                         " (", arv.percent[y, 1], "%)", sep = ""))
  #     suppressed.cases.display <- lapply(1:length(suppressed.cases),
  #                                        function(y)
  #                                          paste(format(suppressed.cases[y],
  #                                                       digits = 12, decimal.mark = ",",
  #                                                       big.mark = ",", small.mark = "."),
  #                                                " (", suppressed.percent[y, 1],
  #                                                "%)", sep = ""))
  #
  #     # Scenario number
  #     scenario <- lapply(scenarios, function(x) paste(round(x, 0)))
  #
  #     # Transmissions
  #     total.hiv.tx <- aggregate(as.numeric(as.character(gsub(",", "", values$df$"# Tx"))),
  #                               by = list(as.character(values$df$Scenario)),
  #                               FUN = sum)[2]
  #
  #     undiagnosed.tx <- round(as.numeric(as.character(gsub(",", "",
  #                                                          values$df$"# Tx"[which(values$df$"Stage" == stagenames.mutex[1])]))))
  #     diagnosed.nr.tx <- round(as.numeric(as.character(gsub(",", "",
  #                                                           values$df$"# Tx"[which(values$df$"Stage" == stagenames.mutex[2])]))))
  #     retained.tx <- round(as.numeric(as.character(gsub(",", "",
  #                                                       values$df$"# Tx"[which(values$df$"Stage" == stagenames.mutex[3])]))))
  #     arv.tx <- round(as.numeric(as.character(gsub(",", "",
  #                                                  values$df$"# Tx"[which(values$df$"Stage" == stagenames.mutex[4])]))))
  #     suppressed.tx <- round(as.numeric(as.character(gsub(",", "",
  #                                                         values$df$"# Tx"[which(values$df$"Stage" == stagenames.mutex[5])]))))
  #
  #     undiagnosed.tx.rate <- as.numeric(as.character(gsub(",", "",
  #                                                         values$df$"Rate/100PY"[which(values$df$"Stage" == stagenames.mutex[1])])))
  #     diagnosed.nr.tx.rate <- as.numeric(as.character(gsub(",", "",
  #                                                          values$df$"Rate/100PY"[which(values$df$"Stage" == stagenames.mutex[2])])))
  #     retained.tx.rate <- as.numeric(as.character(gsub(",", "",
  #                                                      values$df$"Rate/100PY"[which(values$df$"Stage" == stagenames.mutex[3])])))
  #     arv.tx.rate <- as.numeric(as.character(gsub(",", "",
  #                                                 values$df$"Rate/100PY"[which(values$df$"Stage" == stagenames.mutex[4])])))
  #     suppressed.tx.rate <- as.numeric(as.character(gsub(",", "",
  #                                                        values$df$"Rate/100PY"[which(values$df$"Stage" == stagenames.mutex[5])])))
  #
  #     undiagnosed.tx.percent <- round((undiagnosed.tx / total.hiv.tx) * 100, 1)
  #     diagnosed.nr.tx.percent <- round((diagnosed.nr.tx / total.hiv.tx) * 100, 1)
  #     retained.tx.percent <- round((retained.tx / total.hiv.tx) * 100, 1)
  #     arv.tx.percent <- round((arv.tx / total.hiv.tx) * 100, 1)
  #     suppressed.tx.percent <- round((suppressed.tx / total.hiv.tx) * 100, 1)
  #
  #     total.hiv.tx.display <- lapply(1:nrow(total.hiv.tx),
  #                                    function(y)
  #                                      paste(format(total.hiv.tx[y,1], digits = 12,
  #                                                   decimal.mark = ",", big.mark = ",",
  #                                                   small.mark = ".")))
  #     undiagnosed.tx.display <- lapply(1:length(undiagnosed.tx),
  #                                      function(y)
  #                                        paste(format(undiagnosed.tx[y], digits = 12,
  #                                                     decimal.mark = ",",
  #                                                     big.mark = ",", small.mark = "."),
  #                                              " (", undiagnosed.tx.percent[y, 1],
  #                                              "%); ",
  #                                              format(undiagnosed.tx.rate[y],
  #                                                     digits = 3, small.mark = ","),
  #                                              sep = ""))
  #     diagnosed.nr.tx.display <- lapply(1:length(diagnosed.nr.tx),
  #                                       function(y)
  #                                         paste(format(diagnosed.nr.tx[y],
  #                                                      digits = 12, decimal.mark = ",",
  #                                                      big.mark = ",", small.mark = "."),
  #                                               " (", diagnosed.nr.tx.percent[y, 1],
  #                                               "%); ",
  #                                               format(diagnosed.nr.tx.rate[y],
  #                                                      digits = 3, small.mark = ","),
  #                                               sep = ""))
  #     retained.tx.display <- lapply(1:length(retained.tx),
  #                                   function(y)
  #                                     paste(format(retained.tx[y], digits = 12,
  #                                                  decimal.mark = ",", big.mark = ",",
  #                                                  small.mark = "."),
  #                                           " (",
  #                                           retained.tx.percent[y, 1], "%); ",
  #                                           format(retained.tx.rate[y], digits = 3,
  #                                                  small.mark = ","), sep = ""))
  #     arv.tx.display <- lapply(1:length(arv.tx),
  #                              function(y)
  #                                paste(format(arv.tx[y], digits = 12,
  #                                             decimal.mark = ",", big.mark = ",",
  #                                             small.mark = "."),
  #                                      " (",
  #                                      arv.tx.percent[y, 1], "%); ",
  #                                      format(arv.tx.rate[y], digits = 3,
  #                                             small.mark = ","), sep = ""))
  #     suppressed.tx.display <- lapply(1:length(suppressed.tx),
  #                                     function(y)
  #                                       paste(format(suppressed.tx[y], digits = 12,
  #                                                    decimal.mark = ",",
  #                                                    big.mark = ",", small.mark = "."),
  #                                             " (",
  #                                             suppressed.tx.percent[y, 1], "%); ",
  #                                             format(suppressed.tx.rate[y],
  #                                                    digits = 3, small.mark = ","),
  #                                             sep = ""))
  #
  #
  #     # Create table
  #     colnames(summary.data) <- c("Scenario #", "Scenario Name", "Total HIV+",
  #                                 "Undiagnosed\n# (%)",
  #                                 "Diagnosed, not retained in care\n# (%)",
  #                                 "Retained in care, no ARVs\n# (%)",
  #                                 "ARVs, not suppressed\n# (%)",
  #                                 "Suppressed\n# (%)", "Total Tx",
  #                                 "Tx from undiagnosed\n# (%); rate/100 PY",
  #                                 "Tx from diagnosed, not retained in care\n# (%); rate/100 PY",
  #                                 "Tx from retained in care, no ARVs\n# (%); rate/100 PY",
  #                                 "Tx from ARVs, not suppressed\n# (%); rate/100 PY",
  #                                 "Tx from suppressed\n# (%); rate/100 PY")
  #     summary.data[,1] <- unlist(scenario)
  #     summary.data[,2] <- scenario.names
  #     summary.data[,3] <- unlist(total.hiv.cases.display)
  #     summary.data[,4] <- unlist(undiagnosed.cases.display)
  #     summary.data[,5] <- unlist(diagnosed.nr.cases.display)
  #     summary.data[,6] <- unlist(retained.cases.display)
  #     summary.data[,7] <- unlist(arv.cases.display)
  #     summary.data[,8] <- unlist(suppressed.cases.display)
  #     summary.data[,9] <- unlist(total.hiv.tx.display)
  #     summary.data[,10] <- unlist(undiagnosed.tx.display)
  #     summary.data[,11] <- unlist(diagnosed.nr.tx.display)
  #     summary.data[,12] <- unlist(retained.tx.display)
  #     summary.data[,13] <- unlist(arv.tx.display)
  #     summary.data[,14] <- unlist(suppressed.tx.display)
  #
  #     summary.data
  #   }}, align = rep("r", 15), include.rownames = FALSE
  # )

  output$comparisonText1 <- renderText({
    if (input$compareScenarios > 0) {
      if (isolate(input$compareScenario1 > 0) &
          isolate(input$compareScenario2 > 0) &
          isolate(input$compareScenario1 != input$compareScenario2)) {

        name1 <- values$df$"Name"[which(values$df$"Scenario" == isolate(input$compareScenario1))][1]
        name2 <- values$df$"Name"[which(values$df$"Scenario" == isolate(input$compareScenario2))][1]

        total1 <- sum(as.numeric(as.character(gsub(",", "",
                                                   values$df$"# indicated for PrEP"[which(values$df$"Scenario" == isolate(input$compareScenario1))]))))
        total2 <- sum(as.numeric(as.character(gsub(",", "",
                                                   values$df$"# Tx"[which(values$df$"Scenario" == isolate(input$compareScenario2))]))))

        difference <- total2 - total1
        abs.difference <- abs(difference)
        if (difference < 0) comparison.phrase <- paste(", or ",
                                                       format(abs.difference,
                                                              digits = 12,
                                                              decimal.mark = ",",
                                                              big.mark = ",",
                                                              small.mark = "."),
                                                       " fewer HIV transmissions than \"",
                                                       name1,
                                                       ".\"",
                                                       sep = "")
        if (difference > 0) comparison.phrase <- paste(", or ",
                                                       format(abs.difference,
                                                              digits = 12,
                                                              decimal.mark = ",",
                                                              big.mark = ",",
                                                              small.mark = "."),
                                                       " more HIV transmissions than \"",
                                                       name1,
                                                       ".\"",
                                                       sep = "")
        if (difference == 0) comparison.phrase <- paste(", the same number of HIV transmissions as \"",
                                                        name1,
                                                        ".\"",
                                                        sep = "")

        comparison.text <- paste("The first comparison scenario, \"",
                                 name1, ",\" resulted in ",
                                 format(total1, digits = 12,
                                        decimal.mark = ",", big.mark = ",",
                                        small.mark = "."),
                                 " total HIV transmissions. The second comparison scenario,
                                 \"",
                                 name2,
                                 ",\" resulted in ",
                                 format(total2, digits = 12,
                                        decimal.mark = ",", big.mark = ",",
                                        small.mark = "."),
                                 " total HIV transmissions", comparison.phrase,
                                 sep = "")

        return(comparison.text)
      }
    }
  })

  ## Generate report -----------------------------------------------------------
  output$downloadReport <- downloadHandler(
    filename = function(){
      paste("Custom HIV Continuum Report ", Sys.Date(), ".docx", sep = "")
    },
    content = function(file){
      src <- normalizePath("report.Rmd")

      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, "report.Rmd")

      out <- render("report.Rmd",word_document())
      file.rename(out, file)
    })
})
