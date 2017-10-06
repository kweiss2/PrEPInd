library(shiny)
library(shinydashboard)
library(wesanderson)
library(shiny)
library(knitr)
library(rmarkdown)


## Group names -----------------------------------------------------------------
stagenames.transcat <- c("Total", "MSM", "HET", "PWID")
stagenames.racetranscat <- c("Black MSM", "Black HET",
                             "Black PWID", "Hispanic MSM", "Hispanic HET",
                             "Hispanic PWID", "White MSM", "White HET", "White PWID")
colors <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0")

## Diagnosed percentages by jurisdiction ---------------------------------------
#Diagnoses split by percent of total by jurisdiction:
# Sum to 100 within race
# Black among MSM, Hisp among MSM, white among MSM
# Black among HET, Hisp among HET, white among HET
# Black among PWID,Hisp among PWID, white among PWID
race.diagnosis.percents <- list(
          c(37.9851, 27.121, 29.3218, 36.5408, 20.5847, 38.6521, 63.8007, 18.0471, 14.1581),    #Natl. Avg
          c(68.9103, 4.1667, 22.1154, 52.381, 4.7619, 42.8571, 79.0541, 2.027, 14.1892),    #Alabama
          c(13.3333, 13.3333, 40, 0, 0, 0, 22.2222, 0, 22.2222),    #Alaska
          c(13.6194, 37.3134, 38.4328, 12.9032, 38.7097, 35.4839, 40.8333, 26.6667, 19.1667),    #Arizona
          c(48.9247, 6.4516, 41.9355, 50, 6.25, 43.75, 66.6667, 11.1111, 20.3704),    #Arkansas
          c(16.1224, 45.3316, 29.5408, 22.2222, 29.9145, 42.3077, 36.2345, 39.4316, 17.2291),    #California
          c(11.4007, 30.9446, 54.3974, 23.0769, 38.4615, 38.4615, 43.1373, 29.4118, 25.4902),    #Colorado
          c(26.2195, 31.0976, 41.4634, 30, 20, 40, 57.4713, 28.7356, 9.1954),    #Connecticut
          c(44.6154, 7.6923, 44.6154, 25, 50, 25, 70, 12.5, 17.5),    #Delaware
          c(32.3529, 37.3626, 27.9573, 32.5359, 18.6603, 47.3684, 63.9429, 19.0661, 14.786),    #Florida
          c(72.1992, 7.8838, 16.8346, 72.9167, 2.0833, 21.875, 83.0252, 4.5378, 10.7563),    #Georgia
          c(9.1954, 9.1954, 35.6322, 18.1818, 0, 36.3636, 5.8824, 11.7647, 35.2941),    #Hawaii
          c(3.7037, 29.6296, 62.963, 50, 0, 0, 77.7778, 0, 22.2222),    #Idaho
          c(45.325, 23.1523, 25.4675, 56.6038, 9.434, 30.1887, 70.3971, 12.9964, 12.2744),    #Illinois
          c(42.5982, 10.574, 44.1088, 2.7624, 1.105, 95.0276, 50.4202, 10.9244, 32.7731),    #Indiana
          c(15.2941, 12.9412, 65.8824, 0, 16.6667, 75, 39.2857, 10.7143, 35.7143),    #Iowa
          c(22.1239, 26.5487, 43.3628, 40, 20, 20, 60.7143, 14.2857, 25),    #Kansas
          c(34, 4.8, 58.4, 22.2222, 0, 77.7778, 56.7164, 1.4925, 32.8358),    #Kentucky
          c(67.234, 5.8156, 25.2482, 62.5, 6.8182, 28.4091, 84.6847, 4.8048, 9.6096),    #Louisiana
          c(10, 13.3333, 76.6667, 20, 20, 60, 88.8889, 0, 0),    #Maine
          c(70.7792, 8.7013, 15.8442, 61.8421, 5.2632, 28.9474, 85.5131, 5.6338, 6.0362),    #Maryland
          c(16.8116, 27.2464, 47.8261, 28.9474, 31.5789, 38.1579, 57.7143, 27.4286, 13.1429),    #Massachusetts
          c(58.6716, 6.4576, 30.9963, 52.381, 2.381, 42.8571, 75.9124, 4.3796, 18.9781),    #Michigan
          c(30.6533, 10.0503, 52.7638, 50, 8.3333, 33.3333, 76, 4, 14.6667),    #Minnesota
          c(79.0368, 2.8329, 16.4306, 80, 0, 15, 81.4815, 2.963, 11.1111),    #Mississippi
          c(41.691, 4.6647, 48.105, 32, 8, 56, 71.134, 4.1237, 19.5876),    #Missouri
          c(6.6667, 20, 73.3333, 0, 0, 50, 0, 0, 100),    #Montana
          c(14.0625, 14.0625, 62.5, 50, 0, 0, 43.75, 12.5, 37.5),    #Nebraska
          c(20.8226, 33.9332, 34.4473, 22.2222, 11.1111, 62.963, 50, 18.5714, 22.8571),    #Nevada
          c(NA, NA, NA, NA, NA, NA, NA, NA, NA),# New Hampshire
          c(35.6204, 38.8321, 20.7299, 49.2188, 25, 23.4375, 60.4839, 27.1505, 9.6774),    #New Jersey
          c(4.5455, 53.6364, 31.8182, 0, 92.3077, 0, 0, 58.3333, 16.6667),    #New Mexico
          c(30.8648, 36.3115, 23.3636, 47.7733, 31.9838, 15.7895, 62.7249, 22.108, 8.9974),    #New York
          c(60.2116, 9.9471, 25.5026, 65.4545, 3.6364, 29.0909, 74.3976, 7.5301, 13.8554),    #North Carolina
          c(18.1818, 9.0909, 54.5455, 33.3333, 33.3333, 33.3333, 88.8889, 0, 0),    #North Dakota
          c(48.5926, 5.3333, 42.0741, 31.5789, 5.2632, 59.6491, 65.4639, 5.1546, 23.7113),    #Ohio
          c(22.4, 14.4, 47.6, 5.8824, 5.8824, 70.5882, 34.6154, 9.6154, 34.6154),    #Oklahoma
          c(5.3892, 16.7665, 73.0539, 0, 10, 80, 37.5, 12.5, 37.5),    #Oregon
          c(49.5677, 10.8069, 36.0231, 35.3659, 24.3902, 36.5854, 64.2132, 15.736, 17.2589),    #Pennsylvania
          c(12.8205, 35.8974, 46.1538, 25, 50, 25, 52.381, 23.8095, 19.0476),    #Rhode Island
          c(65.3017, 7.1121, 25.431, 66.6667, 11.9048, 19.0476, 77.6596, 5.3191, 15.9574),    #South Carolina
          c(16.6667, 8.3333, 66.6667, 0, 100, 0, 44.4444, 11.1111, 33.3333),    #South Dakota
          c(57.2314, 6.6116, 34.0909, 33.3333, 0, 61.9048, 68.4492, 5.3476, 24.5989),    #Tennessee
          c(31.9525, 40.9382, 22.9668, 38.2979, 28.5106, 29.7872, 53.6688, 30.2935, 12.0545),    #Texas
          c(3.125, 27.0833, 62.5, 0, 20, 60, 33.3333, 26.6667, 33.3333),    #Utah
          c(0, 20, 80, 0, 33.3333, 66.6667, 0, 0, 75),    #Vermont
          c(56.4759, 12.8012, 26.8072, 53.125, 9.375, 31.25, 76.1538, 6.9231, 13.8462),    #Virginia
          c(14.8256, 20.9302, 52.3256, 13.1579, 13.1579, 63.1579, 54.8387, 9.6774, 24.1935),    #Washington
          c(5.6604, 3.7736, 86.7925, 20, 0, 40, 53.3333, 0, 46.6667),    #West Virginia
          c(37.8378, 15.1351, 39.4595, 37.5, 25, 37.5, 58.3333, 13.8889, 25),    #Wisconsin
          c(14.2857, 28.5714, 57.1429, 33.3333, 33.3333, 33.3333, 0, 0, 75),    #Wyoming
          c(59.4262, 20.4918, 16.3934, 85.7143, 4.7619, 4.7619, 97.4138, 0.8621, 0.8621))    #Washington, D.C.

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
    x <- race.diagnosis.percents[[as.numeric(input$jurisdiction)]]
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
    x <- race.diagnosis.percents[[as.numeric(input$jurisdiction)]]
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
                       value = round((totmsm * 0.247) * (y[1] / 100) * (x[1] / 100), 0))
    updateNumericInput(session, "blackhetprep",
                       value = round((totmsm * 0.247) * (y[2] / y[1]) * (x[4] / 100), 0))
    updateNumericInput(session, "blackpwidprep",
                       value = round((totmsm * 0.247) * (y[3] / y[1]) * (x[7] / 100), 0))
    updateNumericInput(session, "hispmsmprep",
                       value = round((totmsm * 0.247) * (y[1] / 100) * (x[2] / 100), 0))
    updateNumericInput(session, "hisphetprep",
                       value = round((totmsm * 0.247) * (y[2] / y[1]) * (x[5] / 100), 0))
    updateNumericInput(session, "hisppwidprep",
                       value = round((totmsm * 0.247) * (y[3] / y[1]) * (x[8] / 100), 0))
    updateNumericInput(session, "whitemsmprep",
                       value = round((totmsm * 0.247) * (y[1] / 100) * (x[3] / 100), 0))
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
    updateNumericInput(session, "customblackmsmdiagpct", value = round(race.diagnosis.percents[[as.numeric(input$jurisdiction)]][1], 1))
    updateNumericInput(session, "customblackhetdiagpct", value = round(race.diagnosis.percents[[as.numeric(input$jurisdiction)]][4], 1))
    updateNumericInput(session, "customblackpwiddiagpct", value = round(race.diagnosis.percents[[as.numeric(input$jurisdiction)]][7], 1))
    updateNumericInput(session, "customhispmsmdiagpct", value = round(race.diagnosis.percents[[as.numeric(input$jurisdiction)]][2], 1))
    updateNumericInput(session, "customhisphetdiagpct", value = round(race.diagnosis.percents[[as.numeric(input$jurisdiction)]][5], 1))
    updateNumericInput(session, "customhisppwiddiagpct", value = round(race.diagnosis.percents[[as.numeric(input$jurisdiction)]][8], 1))
    updateNumericInput(session, "customwhitemsmdiagpct", value = round(race.diagnosis.percents[[as.numeric(input$jurisdiction)]][3], 1))
    updateNumericInput(session, "customwhitehetdiagpct", value = round(race.diagnosis.percents[[as.numeric(input$jurisdiction)]][6], 1))
    updateNumericInput(session, "customwhitepwiddiagpct", value = round(race.diagnosis.percents[[as.numeric(input$jurisdiction)]][9], 1))
    # }
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
  # observe({
  #   if (input$saveButton > 0) {
  #     newData <- isolate(cbind(input$saveButton, input$scenarioName, currentData()))
  #     isolate(colnames(newData) <- c("Scenario", "Name", "Stage", "#", "
  #                                    % of HIV+", "Rate/100PY", "# Tx", "% of Tx"))
  #     isolate(colnames(values$df) <- c("Scenario", "Name", "Stage", "#",
  #                                      "% of HIV+", "Rate/100PY", "# Tx", "% of Tx"))
  #     isolate(values$df <- rbind(values$df, newData))
  #     isolate(values$df <- values$df[which(values$df$Scenario != 0), ])
  #
  #     isolate(updateSelectInput(session, "removeScenarioNum",
  #                               choices = unique(values$df$"Scenario")))
  #     isolate(updateSelectInput(session, "compareScenario1",
  #                               choices = unique(values$df$"Scenario")))
  #     isolate(updateSelectInput(session, "compareScenario2",
  #                               choices = unique(values$df$"Scenario")))
  #     isolate(updateTextInput(session, "scenarioName", value = ""))
  #   }
  # })
  #
  # observe({
  #   if (input$removeButton > 0) {
  #     isolate(values$df <- values$df[which(values$df$"Scenario" != input$removeScenarioNum), ])
  #   }
  # })
  #
  # output$comparisonText1 <- renderText({
  #   if (input$compareScenarios > 0) {
  #     if (isolate(input$compareScenario1 > 0) &
  #         isolate(input$compareScenario2 > 0) &
  #         isolate(input$compareScenario1 != input$compareScenario2)) {
  #
  #       name1 <- values$df$"Name"[which(values$df$"Scenario" == isolate(input$compareScenario1))][1]
  #       name2 <- values$df$"Name"[which(values$df$"Scenario" == isolate(input$compareScenario2))][1]
  #
  #       total1 <- sum(as.numeric(as.character(gsub(",", "",
  #                                                  values$df$"# indicated for PrEP"[which(values$df$"Scenario" == isolate(input$compareScenario1))]))))
  #       total2 <- sum(as.numeric(as.character(gsub(",", "",
  #                                                  values$df$"# Tx"[which(values$df$"Scenario" == isolate(input$compareScenario2))]))))
  #
  #       difference <- total2 - total1
  #       abs.difference <- abs(difference)
  #       if (difference < 0) comparison.phrase <- paste(", or ",
  #                                                      format(abs.difference,
  #                                                             digits = 12,
  #                                                             decimal.mark = ",",
  #                                                             big.mark = ",",
  #                                                             small.mark = "."),
  #                                                      " fewer HIV transmissions than \"",
  #                                                      name1,
  #                                                      ".\"",
  #                                                      sep = "")
  #       if (difference > 0) comparison.phrase <- paste(", or ",
  #                                                      format(abs.difference,
  #                                                             digits = 12,
  #                                                             decimal.mark = ",",
  #                                                             big.mark = ",",
  #                                                             small.mark = "."),
  #                                                      " more HIV transmissions than \"",
  #                                                      name1,
  #                                                      ".\"",
  #                                                      sep = "")
  #       if (difference == 0) comparison.phrase <- paste(", the same number of HIV transmissions as \"",
  #                                                       name1,
  #                                                       ".\"",
  #                                                       sep = "")
  #
  #       comparison.text <- paste("The first comparison scenario, \"",
  #                                name1, ",\" resulted in ",
  #                                format(total1, digits = 12,
  #                                       decimal.mark = ",", big.mark = ",",
  #                                       small.mark = "."),
  #                                " total HIV transmissions. The second comparison scenario,
  #                                \"",
  #                                name2,
  #                                ",\" resulted in ",
  #                                format(total2, digits = 12,
  #                                       decimal.mark = ",", big.mark = ",",
  #                                       small.mark = "."),
  #                                " total HIV transmissions", comparison.phrase,
  #                                sep = "")
  #
  #       return(comparison.text)
  #     }
  #   }
  # })

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
