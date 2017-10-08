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
  100 * c(0.3799, 0.2712, 0.2932, 0.638, 0.1805, 0.1416, 0.3654, 0.2058, 0.3865),   #Natl. Avg
  100 * c(0.6891, 0.0417, 0.2212, 0.7905, 0.0203, 0.1419, 0.5238, 0.0476, 0.4286),   #Alabama
  100 * c(0.1333, 0.1333, 0.4, 0.2222, 0, 0.2222, 0, 0, 0),   #Alaska
  100 * c(0.1362, 0.3731, 0.3843, 0.4083, 0.2667, 0.1917, 0.129, 0.3871, 0.3548),   #Arizona
  100 * c(0.4892, 0.0645, 0.4194, 0.6667, 0.1111, 0.2037, 0.5, 0.0625, 0.4375),   #Arkansas
  100 * c(0.1612, 0.4533, 0.2954, 0.3623, 0.3943, 0.1723, 0.2222, 0.2991, 0.4231),   #California
  100 * c(0.114, 0.3094, 0.544, 0.4314, 0.2941, 0.2549, 0.2308, 0.3846, 0.3846),  #Colorado
  100 * c(0.2622, 0.311, 0.4146, 0.5747, 0.2874, 0.092, 0.3, 0.2, 0.4),   #Connecticut
  100 * c(0.4462, 0.0769, 0.4462, 0.7, 0.125, 0.175, 0.25, 0.5, 0.25),   #Delaware
  100 * c(0.3235, 0.3736, 0.2796, 0.6394, 0.1907, 0.1479, 0.3254, 0.1866, 0.4737),   #Florida
  100 * c(0.722, 0.0788, 0.1683, 0.8303, 0.0454, 0.1076, 0.7292, 0.0208, 0.2188),   #Georgia
  100 * c(0.092, 0.092, 0.3563, 0.0588, 0.1176, 0.3529, 0.1818, 0, 0.3636),   #Hawaii
  100 * c(0.037, 0.2963, 0.6296, 0.7778, 0, 0.2222, 0.5, 0, 0),   #Idaho
  100 * c(0.4533, 0.2315, 0.2547, 0.704, 0.13, 0.1227, 0.566, 0.0943, 0.3019),   #Illinois
  100 * c(0.426, 0.1057, 0.4411, 0.5042, 0.1092, 0.3277, 0.0276, 0.011, 0.9503),   #Indiana
  100 * c(0.1529, 0.1294, 0.6588, 0.3929, 0.1071, 0.3571, 0, 0.1667, 0.75),   #Iowa
  100 * c(0.2212, 0.2655, 0.4336, 0.6071, 0.1429, 0.25, 0.4, 0.2, 0.2),   #Kansas
  100 * c(0.34, 0.048, 0.584, 0.5672, 0.0149, 0.3284, 0.2222, 0, 0.7778),   #Kentucky
  100 * c(0.6723, 0.0582, 0.2525, 0.8468, 0.048, 0.0961, 0.625, 0.0682, 0.2841),   #Louisiana
  100 * c(0.1, 0.1333, 0.7667, 0.8889, 0, 0, 0.2, 0.2, 0.6),   #Maine
  100 * c(0.7078, 0.087, 0.1584, 0.8551, 0.0563, 0.0604, 0.6184, 0.0526, 0.2895),   #Maryland
  100 * c(0.1681, 0.2725, 0.4783, 0.5771, 0.2743, 0.1314, 0.2895, 0.3158, 0.3816),   #Massachusetts
  100 * c(0.5867, 0.0646, 0.31, 0.7591, 0.0438, 0.1898, 0.5238, 0.0238, 0.4286),   #Michigan
  100 * c(0.3065, 0.1005, 0.5276, 0.76, 0.04, 0.1467, 0.5, 0.0833, 0.3333),   #Minnesota
  100 * c(0.7904, 0.0283, 0.1643, 0.8148, 0.0296, 0.1111, 0.8, 0, 0.15),   #Mississippi
  100 * c(0.4169, 0.0466, 0.481, 0.7113, 0.0412, 0.1959, 0.32, 0.08, 0.56),   #Missouri
  100 * c(0.0667, 0.2, 0.7333, 0, 0, 1, 0, 0, 0.5),   #Montana
  100 * c(0.1406, 0.1406, 0.625, 0.4375, 0.125, 0.375, 0.5, 0, 0),   #Nebraska
  100 * c(0.2082, 0.3393, 0.3445, 0.5, 0.1857, 0.2286, 0.2222, 0.1111, 0.6296),   #Nevada
  c(NA, NA, NA, NA, NA, NA, NA, NA, NA), # New Hampshire
  100 * c(0.3562, 0.3883, 0.2073, 0.6048, 0.2715, 0.0968, 0.4922, 0.25, 0.2344),   #New Jersey
  100 * c(0.0455, 0.5364, 0.3182, 0, 0.5833, 0.1667, 0, 0.9231, 0),   #New Mexico
  100 * c(0.3086, 0.3631, 0.2336, 0.6272, 0.2211, 0.09, 0.4777, 0.3198, 0.1579),   #New York
  100 * c(0.6021, 0.0995, 0.255, 0.744, 0.0753, 0.1386, 0.6545, 0.0364, 0.2909),   #North Carolina
  100 * c(0.1818, 0.0909, 0.5455, 0.8889, 0, 0, 0.3333, 0.3333, 0.3333),   #North Dakota
  100 * c(0.4859, 0.0533, 0.4207, 0.6546, 0.0515, 0.2371, 0.3158, 0.0526, 0.5965),  #Ohio
  100 * c(0.224, 0.144, 0.476, 0.3462, 0.0962, 0.3462, 0.0588, 0.0588, 0.7059),   #Oklahoma
  100 * c(0.0539, 0.1677, 0.7305, 0.375, 0.125, 0.375, 0, 0.1, 0.8),   #Oregon
  100 * c(0.4957, 0.1081, 0.3602, 0.6421, 0.1574, 0.1726, 0.3537, 0.2439, 0.3659),   #Pennsylvania
  100 * c(0.1282, 0.359, 0.4615, 0.5238, 0.2381, 0.1905, 0.25, 0.5, 0.25),   #Rhode Island
  100 * c(0.653, 0.0711, 0.2543, 0.7766, 0.0532, 0.1596, 0.6667, 0.119, 0.1905),   #South Carolina
  100 * c(0.1667, 0.0833, 0.6667, 0.4444, 0.1111, 0.3333, 0, 1, 0),   #South Dakota
  100 * c(0.5723, 0.0661, 0.3409, 0.6845, 0.0535, 0.246, 0.3333, 0, 0.619),   #Tennessee
  100 * c(0.3195, 0.4094, 0.2297, 0.5367, 0.3029, 0.1205, 0.383, 0.2851, 0.2979),   #Texas
  100 * c(0.0313, 0.2708, 0.625, 0.3333, 0.2667, 0.3333, 0, 0.2, 0.6),   #Utah
  100 * c(0, 0.2, 0.8, 0, 0, 0.75, 0, 0.3333, 0.6667),   #Vermont
  100 * c(0.5648, 0.128, 0.2681, 0.7615, 0.0692, 0.1385, 0.5313, 0.0938, 0.3125),  #Virginia
  100 * c(0.1483, 0.2093, 0.5233, 0.5484, 0.0968, 0.2419, 0.1316, 0.1316, 0.6316),   #Washington
  100 * c(0.0566, 0.0377, 0.8679, 0.5333, 0, 0.4667, 0.2, 0, 0.4),   #West Virginia
  100 * c(0.3784, 0.1514, 0.3946, 0.5833, 0.1389, 0.25, 0.375, 0.25, 0.375),   #Wisconsin
  100 * c(0.1429, 0.2857, 0.5714, 0, 0, 0.75, 0.3333, 0.3333, 0.3333),   #Wyoming
  100 * c(0.5943, 0.2049, 0.1639, 0.9741, 0.0086, 0.0086, 0.8571, 0.0476, 0.0476))   # Washington, District of Columbia

# MSM, HET, PWID
trans.diagnosis.percents <- list(
  100 * c(813970/1144550, 258080/1144550, 72510/1144550), # Natl. Avg
  100 * c(7680/11840, 3640/11840, 520/11840), # Alabama
  100 * c(1470/2360, 880/2360, 0 / 2360), # Alaska
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
    # Black among MSM, Hisp among MSM, white among MSM
    # Black among HET, Hisp among HET, white among HET
    # Black among PWID,Hisp among PWID, white among PWID
    x <- race.diagnosis.percents[[as.numeric(input$jurisdiction)]]
    updateNumericInput(session, "blackmsmdiagpct", value = round(x[1], 1))
    updateNumericInput(session, "blackhetdiagpct", value = round(x[4], 1))
    updateNumericInput(session, "blackpwiddiagpct", value = round(x[7], 1))
    updateNumericInput(session, "hispmsmdiagpct", value = round(x[2], 1))
    updateNumericInput(session, "hisphetdiagpct", value = round(x[5], 1))
    updateNumericInput(session, "hisppwiddiagpct", value = round(x[8], 1))
    updateNumericInput(session, "whitemsmdiagpct", value = round(x[3], 1))
    updateNumericInput(session, "whitehetdiagpct", value = round(x[6], 1))
    updateNumericInput(session, "whitepwiddiagpct", value = round(x[9], 1))

    # MSM, HET, PWID
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
