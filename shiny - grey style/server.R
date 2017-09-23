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
# Black among MSM, Hisp among MSM, white among MSM
# Black among HET, Hisp among HET, white among HET
# Black among PWID,Hisp among PWID, white among PWID
race.diagnosis.percents <-
  100 * c(309190/813970, 220760/813970, 238670/813970,
    164660/258080, 46580/258080, 36540/258080,
    26490/72510, 14920/72510, 28020/72510)

trans.diagnosis.percents <- list(
  100 * c(813970/1144550, 258080/1144550, 72510/1144550), # Natl. Avg
  100 * c(7680/11840, 3640/11840, 520/11840), # Alabama
  100 * c(1470/2360, 880/2360, 0, 2360), # Alaska
  100 * c(18920/25350, 4240/25350, 2190/25350), # Arizona
  100 * c(3350/4610, 970/4610, 290/4610), # Arkansas
  100 * c(129820/156210, 18640/156210, 7750/156210), # California
  100 * c(20110/24310, 3340/24310, 850/24310), # Colorado
  100 * c(5830/9640, 3090/9640, 710/9640), # Connecticut
  100 * c(2390/4010, 1470/4010, 150/4010), # Delaware
  100 * c(73570/115200, 36670/115200, 4970/115200), # Florida
  100 * c(25330/35700, 8930/35700, 1440/35700), # Georgia
  100 * c(3700/4890, 720/4890, 470/4890), # Hawaii
  100 * c(2740/3860, 910/3860, 200/3860), # Idaho
  100 * c(39600/51240, 9770/51240, 1870/51240), # Illinois
  100 * c(12320/23480, 4430/23480, 6740/23480), # Indiana
  100 * c(2840/4180, 940/4180, 400/4180), # Iowa
  100 * c(3410/4400, 840/4400, 150/4400), # Kansas
  100 * c(9100/12190, 2440/12190, 660/12190), # Kentucky
  100 * c(8380/13390, 3960/13390, 1050/13390), # Louisiana
  100 * c(2220/3250, 660/3250, 370/3250), # Maine
  100 * c(15700/27390, 10130/27390, 1550/27390), # Maryland
  100 * c(12670/21890, 6430/21890, 2790/21890), # Massachusetts
  100 * c(20700/27540, 5230/27540, 1600/27540), # Michigan
  100 * c(15180/21820, 5720/21820, 920/21820), # Minnesota
  100 * c(3480/5010, 1330/5010, 200/5010), # Mississippi
  100 * c(13220/17930, 3740/17930, 960/17930), # Missouri
  100 * c(1880/2500, 380/2500, 250/2500), # Montana
  100 * c(1930/2470, 480/2470, 60/2470), # Nebraska
  100 * c(7770/9710, 1400/9710, 540/9710), # Nevada
  100 * c(1890/2650, 380/2650, 380/2650), # New Hampshire
  100 * c(15380/26610, 8350/26610, 2870/26610), # New Jersey
  100 * c(4560/5600, 500/5600, 540/5600), # New Mexico
  100 * c(48740/72610, 18120/72610, 5750/72610), # New York
  100 * c(21160/29820, 7430/29820, 1230/29820), # North Carolina
  100 * c(630/1320, 510/1320, 170/1320), # North Dakota
  100 * c(27780/38110, 7980/38110, 2350/38110), # Ohio
  100 * c(7170/9140, 1490/9140, 490/9140), # Oklahoma
  100 * c(13360/16880, 1920/16880, 1600/16880), # Oregon
  100 * c(21380/36050, 12140/36050, 2530/36050), # Pennsylvania
  100 * c(2970/4880, 1600/4880, 300/4880), # Rhode Island
  100 * c(6040/9040, 2450/9040, 550/9050), # South Carolina
  100 * c(710/1290, 530/1290, 60/1290), # South Dakota
  100 * c(15530/22880, 6000/22880, 1350/22880), # Tennessee
  100 * c(86020/117180, 25000/117180, 6160/117180), # Texas
  100 * c(5650/6830, 880/6830, 290/6830), # Utah
  100 * c(1120/2690, 900/2690, 670/2690), # Vermont
  100 * c(22490/32380, 8810/32380, 1080/32380), # Virginia
  100 * c(24130/31150, 4350/31150, 2670/31150), # Washington
  100 * c(2220/3060, 630/3060, 210/3060), # West Virginia
  100 * c(9840/12180, 1910/12180, 430/12180), # Wisconsin
  100 * c(1010/2030, 580/2030, 430/2030), # Wyoming
  100 * c(8850/13820, 4210/13820, 760/13820)  # Washington D.C

)

shinyServer(function(input, output, session) {

  ## Get diagnosis percents ----------------------------------------------------
  observe({
    # Transmission category within race
    x <- race.diagnosis.percents
    updateNumericInput(session, "blackmsmdiagpct", value = round(x[1], 1))
    updateNumericInput(session, "blackhetdiagpct", value = round(x[2], 1))
    updateNumericInput(session, "blackpwiddiagpct", value = round(x[3], 1))
    updateNumericInput(session, "hispmsmdiagpct", value = round(x[4], 1))
    updateNumericInput(session, "hisphetdiagpct", value = round(x[5], 1))
    updateNumericInput(session, "hisppwiddiagpct", value = round(x[6], 1))
    updateNumericInput(session, "whitemsmdiagpct", value = round(x[7], 1))
    updateNumericInput(session, "whitehetdiagpct", value = round(x[8], 1))
    updateNumericInput(session, "whitepwiddiagpct", value = round(x[9], 1))

    # Transmission category only
    y <- trans.diagnosis.percents[[as.numeric(input$jurisdiction)]]
    updateNumericInput(session, "msmdiagpct", value = round(y[1], 1))
    updateNumericInput(session, "hetdiagpct", value = round(y[2], 1))
    updateNumericInput(session, "pwiddiagpct", value = round(y[3], 1))
  })

  observe({

    ## Calculate number indicated for PrEP -------------------------------------
    totmsm <- input$msmpopsize
    x <- race.diagnosis.percents
    y <- trans.diagnosis.percents[[as.numeric(input$jurisdiction)]]

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
                       value = round((totmsm * 0.247) * (x[1] / 100), 0))
    updateNumericInput(session, "blackhetprep",
                       value = round((totmsm * 0.247) * (y[2] / y[1]) * (x[4] / 100), 0))
    updateNumericInput(session, "blackpwidprep",
                       value = round((totmsm * 0.247) * (y[3] / y[1]) * (x[7] / 100), 0))
    updateNumericInput(session, "hispmsmprep",
                       value = round((totmsm * 0.247) * (x[2] / 100), 0))
    updateNumericInput(session, "hisphetprep",
                       value = round((totmsm * 0.247) * (y[2] / y[1]) * (x[5] / 100), 0))
    updateNumericInput(session, "hisppwidprep",
                       value = round((totmsm * 0.247) * (y[3] / y[1]) * (x[8] / 100), 0))
    updateNumericInput(session, "whitemsmprep",
                       value = round((totmsm * 0.247) * (x[3] / 100), 0))
    updateNumericInput(session, "whitehetprep",
                       value = round((totmsm * 0.247) * (y[2] / y[1]) * (x[6] / 100), 0))
    updateNumericInput(session, "whitepwidprep",
                       value = round((totmsm * 0.247) * (y[3] / y[1]) * (x[9] / 100), 0))



    if (input$jurisdiction == 46) {
      updateNumericInput(session, "blackmsmprep",
                         value = NA)
      updateNumericInput(session, "blackhetprep",
                         value = NA)
      updateNumericInput(session, "blackpwidprep",
                         value = NA)
    }

    if (input$jurisdiction == 30) {
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
    # if (is.na(input$existingprop) == FALSE) {

      updateNumericInput(session, "customblackmsmdiagpct", value = round(race.diagnosis.percents[1], 1))
      updateNumericInput(session, "customblackhetdiagpct", value = round(race.diagnosis.percents[4], 1))
      updateNumericInput(session, "customblackpwiddiagpct", value = round(race.diagnosis.percents[7], 1))
      updateNumericInput(session, "customhispmsmdiagpct", value = round(race.diagnosis.percents[2], 1))
      updateNumericInput(session, "customhisphetdiagpct", value = round(race.diagnosis.percents[5], 1))
      updateNumericInput(session, "customhisppwiddiagpct", value = round(race.diagnosis.percents[8], 1))
      updateNumericInput(session, "customwhitemsmdiagpct", value = round(race.diagnosis.percents[3], 1))
      updateNumericInput(session, "customwhitehetdiagpct", value = round(race.diagnosis.percents[6], 1))
      updateNumericInput(session, "customwhitepwiddiagpct", value = round(race.diagnosis.percents[9], 1))
    # }
    #
  })
  ## Warning text --------------------------------------------------------------

  output$warningText1 <- renderText({
  warning.text <- ""
    if (input$jurisdiction == 46) {
       warning.text <- "African-American estimates by transmission category are not available for Vermont"
     }
    if (input$jurisdiction == 30) {
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
      proportions <- race.diagnosis.percents
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
