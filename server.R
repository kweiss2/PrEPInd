library(shiny)
library(knitr)
library(rmarkdown)

options("scipen"=100)

stagenames.casc <- c("Total", "Diagnosed", "Retained in care", "Prescribed ARVs", "Suppressed")
stagenames.mutex <- c("Undiagnosed", "Diagnosed, not retained", "Retained, no ARVs", "ARVs, not suppressed", "Suppressed")
colors <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0")

values <- reactiveValues()
values$df <- data.frame(t(rep(0,8)))

continuum.percents <- list(
  c(18, 82, 37, 33, 25),
  c(19, 81, 35, 32, 25),
  c(15, 85, 41, 36, 26),
  c(19, 81, 34, 29, 21),
  c(20, 80, 37, 33, 26),
  c(15, 85, 38, 35, 30),
  c(21, 79, 36, 33, 27),
  c(10, 90, 32, 29, 22),
  c(9, 91, 44, 39, 28),
  c(9, 91, 39, 35, 26),
  c(24, 76, 29, 26, 19),
  c(18, 82, 40, 34, 25),
  c(28, 72, 28, 22, 15)
) 

tx.rates <- list(
  c(6.6, 5.3, 2.6, 1.8, 0.4),
  c(7.2, 6.0, 3.5, 2.0, 0.5),
  c(3.9, 3.2, 0.7, 1.2, 0.1),
  c(7.3, 5.6, 4.4, 2.9, 0.7),
  c(6.9, 6.3, 0.2, 0.6, 0.3),
  c(8.2, 6.3, 0.7, 2.4, 0.1),
  c(12.3, 10.5, 5.9, 2.9, 0.2),
  c(5.7, 4.4, 0.2, 0.5, 0.2),
  c(3.2, 2.0, 0.5, 1.0, 0.1),
  c(7.4, 8.1, 3.9, 2.8, 1.1),
  c(6.1, 5.4, 2.2, 1.6, 0.5),
  c(6.3, 4.6, 2.4, 1.7, 0.2), 
  c(6.3, 3.2, 0.4, 0.5, 0.2)
)

shinyServer(function(input, output, session) {
  
  output$incorrect <- reactive({
    isTRUE(input$password != "Demo!!!")
  })
  
  output$correct <- reactive({
    isTRUE(input$password == "Demo!!!")
  })
  
  outputOptions(output, 'incorrect', suspendWhenHidden=FALSE)
  outputOptions(output, 'correct', suspendWhenHidden=FALSE)
  
  observe({
    updateNumericInput(session, "rateassumption", value=input$pctassumption)
  })
  
  observe({
    x <- continuum.percents[[as.numeric(input$pctassumption)]]
    updateNumericInput(session, "undiagnosedpct", value=x[1])
    updateNumericInput(session, "diagnosedretpct", value=round(x[3]/(1-(x[1]/100))))
    updateNumericInput(session, "diagnosedarvpct", value=round(x[4]/(1-(x[1]/100))))
    updateNumericInput(session, "diagnosedsuppct", value=round(x[5]/(1-(x[1]/100))))
  })
  
  output$warningText1 <- renderText({
    warning.text <- ""
    if (input$diagnosedarvpct > input$diagnosedretpct){
      warning.text <- "The percent on antiretroviral medication cannot be greater than the percent retained in care."
    } 
    if (input$diagnosedsuppct > input$diagnosedarvpct){
      warning.text <- "The percent virally suppressed cannot be greater than the percent on antiretroviral medication."
    }
    if (input$diagnosedarvpct > input$diagnosedretpct & input$diagnosedsuppct > input$diagnosedarvpct){
      warning.text <- "The percent prescribed antiretroviral medication cannot be greater than the percent retained in care, and the percent virally suppressed cannot be greater than the percent on antiretroviral medication."
    }
    return(warning.text)
  })
  
  observe({    
    tothiv <- input$diagnosedcnt1
    updateNumericInput(session, "totalcnt", value=round(tothiv/(1-(input$undiagnosedpct/100))))
    updateNumericInput(session, "diagnosedcnt2", value=tothiv)
    updateNumericInput(session, "diagnosedretcnt", value=round((input$diagnosedretpct/100)*tothiv))
    updateNumericInput(session, "diagnosedarvcnt", value=round((input$diagnosedarvpct/100)*tothiv))
    updateNumericInput(session, "diagnosedsupcnt", value=round((input$diagnosedsuppct/100)*tothiv))
  })

  output$warningText2 <- renderText({
    warning.text <- ""
    
    if (is.na(input$totalcnt)==FALSE & is.na(input$diagnosedcnt2)==FALSE & is.na(input$diagnosedretcnt)==FALSE & is.na(input$diagnosedarvcnt)==FALSE & is.na(input$diagnosedsupcnt)==FALSE){
        
      if (input$diagnosedcnt2 > input$totalcnt){
        warning.text <- "The number diagnosed cannot be greater than the total number living with HIV."
      } 
      if (input$diagnosedretcnt > input$diagnosedcnt2){
        warning.text <- "The number retained in care cannot be greater than the number diagnosed."
      } 
      if (input$diagnosedarvcnt > input$diagnosedretcnt){
        warning.text <- "The number prescribed antiretroviral medication cannot be greater than the number retained in care."
      } 
      if (input$diagnosedsupcnt > input$diagnosedarvcnt){
        warning.text <- "The number virally suppressed cannot be greater than the number on antiretroviral medication."
      }
    }
    return(warning.text)
  })  
  
  observe({
    if (is.na(input$existingrate)==FALSE){
      updateNumericInput(session, "customUndiagnosedTxRate", value=tx.rates[[as.numeric(input$existingrate)]][1])
      updateNumericInput(session, "customDiagnosedNRTxRate", value=tx.rates[[as.numeric(input$existingrate)]][2])
      updateNumericInput(session, "customDiagnosedRetTxRate", value=tx.rates[[as.numeric(input$existingrate)]][3])
      updateNumericInput(session, "customDiagnosedARVTxRate", value=tx.rates[[as.numeric(input$existingrate)]][4])
      updateNumericInput(session, "customDiagnosedSupTxRate", value=tx.rates[[as.numeric(input$existingrate)]][5])
    }
  })
  
  observe({
    tothiv <- input$diagnosedcnt1
    updateNumericInput(session, "undiagnosedcnt.mutex", value=round((input$undiagnosedpct/100)*(tothiv/(1-(input$undiagnosedpct/100)))))
    updateNumericInput(session, "diagnosednrcnt.mutex", value=round(((100 - input$diagnosedretpct)/100)*tothiv))
    updateNumericInput(session, "diagnosedretcnt.mutex", value=round(((input$diagnosedretpct - input$diagnosedarvpct)/100)*tothiv))
    updateNumericInput(session, "diagnosedarvcnt.mutex", value=round(((input$diagnosedarvpct - input$diagnosedsuppct)/100)*tothiv))
    updateNumericInput(session, "diagnosedsupcnt.mutex", value=round((input$diagnosedsuppct/100)*tothiv))       
  })
  
  observe({
    updateNumericInput(session, "undiagnosedcnt.mutex", value=round(input$totalcnt - input$diagnosedcnt2))
    updateNumericInput(session, "diagnosednrcnt.mutex", value=round(input$diagnosedcnt2 - input$diagnosedretcnt))
    updateNumericInput(session, "diagnosedretcnt.mutex", value=round(input$diagnosedretcnt - input$diagnosedarvcnt))
    updateNumericInput(session, "diagnosedarvcnt.mutex", value=round(input$diagnosedarvcnt - input$diagnosedsupcnt))
    updateNumericInput(session, "diagnosedsupcnt.mutex", value=round(input$diagnosedsupcnt))       
  })
  
  output$continuumPlotCntCasc <- renderPlot({
    if (is.na(input$undiagnosedcnt.mutex)==FALSE & is.na(input$diagnosednrcnt.mutex)==FALSE & is.na(input$diagnosedretcnt.mutex)==FALSE & is.na(input$diagnosedarvcnt.mutex)==FALSE & is.na(input$diagnosedsupcnt.mutex)==FALSE){
      stagescnt <- c(sum(input$undiagnosedcnt.mutex, input$diagnosednrcnt.mutex, input$diagnosedretcnt.mutex, input$diagnosedarvcnt.mutex, input$diagnosedsupcnt.mutex), sum(input$diagnosednrcnt.mutex, input$diagnosedretcnt.mutex, input$diagnosedarvcnt.mutex, input$diagnosedsupcnt.mutex), sum(input$diagnosedretcnt.mutex, input$diagnosedarvcnt.mutex, input$diagnosedsupcnt.mutex), sum(input$diagnosedarvcnt.mutex, input$diagnosedsupcnt.mutex), input$diagnosedsupcnt.mutex)
      barplot(stagescnt, col=colors, names.arg=stagenames.casc, cex.names=.8, main="HIV Care Continuum", sub="Cascade", ylim=c(0,max(stagescnt)))
    }
  })  
  
  output$continuumPlotCntSep <- renderPlot({
    if (is.na(input$undiagnosedcnt.mutex)==FALSE & is.na(input$diagnosednrcnt.mutex)==FALSE & is.na(input$diagnosedretcnt.mutex)==FALSE & is.na(input$diagnosedarvcnt.mutex)==FALSE & is.na(input$diagnosedsupcnt.mutex)==FALSE){
      stagescnt <- c(input$undiagnosedcnt.mutex, input$diagnosednrcnt.mutex, input$diagnosedretcnt.mutex, input$diagnosedarvcnt.mutex, input$diagnosedsupcnt.mutex)
      barplot(stagescnt, col=colors, names.arg=stagenames.mutex, cex.names=.8, main="HIV Care Continuum", sub="Mutually Exclusive", ylim=c(0,max(stagescnt)))
      #text(cex=.8, x=c(1:5), y=-2, stagenames.mutex, xpd=TRUE, srt=45)
    }
  })
  
  output$txBarCnt <- renderPlot({
    if (is.na(input$customUndiagnosedTxRate)==FALSE & is.na(input$customDiagnosedNRTxRate)==FALSE & is.na(input$customDiagnosedRetTxRate)==FALSE & is.na(input$customDiagnosedARVTxRate)==FALSE & is.na(input$customDiagnosedSupTxRate)==FALSE){
      rates <- c(input$customUndiagnosedTxRate, input$customDiagnosedNRTxRate, input$customDiagnosedRetTxRate, input$customDiagnosedARVTxRate, input$customDiagnosedSupTxRate)
    }
    else {
      rates <- tx.rates[[as.numeric(input$rateassumption)]]
    }
    if (is.na(input$undiagnosedcnt.mutex)==FALSE & is.na(input$diagnosednrcnt.mutex)==FALSE & is.na(input$diagnosedretcnt.mutex)==FALSE & is.na(input$diagnosedarvcnt.mutex)==FALSE & is.na(input$diagnosedsupcnt.mutex)==FALSE){
      stagescnt <- c(input$undiagnosedcnt.mutex, input$diagnosednrcnt.mutex, input$diagnosedretcnt.mutex, input$diagnosedarvcnt.mutex, input$diagnosedsupcnt.mutex)
      txcnt <- round((rates/100) * stagescnt)
      barplot(txcnt, col=colors, names.arg=stagenames.mutex, cex.names=.8, main="Transmissions", ylim=c(0, max(txcnt)))
    }
  })
  
  output$txPieCnt <- renderPlot({
    if (is.na(input$customUndiagnosedTxRate)==FALSE & is.na(input$customDiagnosedNRTxRate)==FALSE & is.na(input$customDiagnosedRetTxRate)==FALSE & is.na(input$customDiagnosedARVTxRate)==FALSE & is.na(input$customDiagnosedSupTxRate)==FALSE){
      rates <- c(input$customUndiagnosedTxRate, input$customDiagnosedNRTxRate, input$customDiagnosedRetTxRate, input$customDiagnosedARVTxRate, input$customDiagnosedSupTxRate)
    }
    else {
      rates <- tx.rates[[as.numeric(input$rateassumption)]]
    }
    if (is.na(input$undiagnosedcnt.mutex)==FALSE & is.na(input$diagnosednrcnt.mutex)==FALSE & is.na(input$diagnosedretcnt.mutex)==FALSE & is.na(input$diagnosedarvcnt.mutex)==FALSE & is.na(input$diagnosedsupcnt.mutex)==FALSE){
      stagescnt <- c(input$undiagnosedcnt.mutex, input$diagnosednrcnt.mutex, input$diagnosedretcnt.mutex, input$diagnosedarvcnt.mutex, input$diagnosedsupcnt.mutex)
      txcnt <- round((rates/100) * stagescnt)
      stagenamespct <- paste(stagenames.mutex, " (", round((txcnt/sum(txcnt))*100), "%)", sep="")
      pie(txcnt, labels=stagenamespct, col=colors, main="Transmissions")
    }
  })
  
  currentData <- reactive({
    if (is.na(input$customUndiagnosedTxRate)==FALSE & is.na(input$customDiagnosedNRTxRate)==FALSE & is.na(input$customDiagnosedRetTxRate)==FALSE & is.na(input$customDiagnosedARVTxRate)==FALSE & is.na(input$customDiagnosedSupTxRate)==FALSE){
      rates <- c(input$customUndiagnosedTxRate, input$customDiagnosedNRTxRate, input$customDiagnosedRetTxRate, input$customDiagnosedARVTxRate, input$customDiagnosedSupTxRate)
    }
    else {
      rates <- tx.rates[[as.numeric(input$rateassumption)]]
    }
    if (is.na(input$undiagnosedcnt.mutex)==FALSE & is.na(input$diagnosednrcnt.mutex)==FALSE & is.na(input$diagnosedretcnt.mutex)==FALSE & is.na(input$diagnosedarvcnt.mutex)==FALSE & is.na(input$diagnosedsupcnt.mutex)==FALSE){
      stagescnt <- c(input$undiagnosedcnt.mutex, input$diagnosednrcnt.mutex, input$diagnosedretcnt.mutex, input$diagnosedarvcnt.mutex, input$diagnosedsupcnt.mutex)
      stagescnt2pctdisc <- round((stagescnt / sum(stagescnt)) * 100)
      txcnt <- round((rates/100) * stagescnt)
      pcttxcnt <- round((txcnt/sum(txcnt))*100)
      txTableCnt <- cbind(stagenames.mutex, format(stagescnt, digits=12, decimal.mark=",", big.mark=",", small.mark="."), paste(as.character(stagescnt2pctdisc), "%", sep=""), rates, format(txcnt, digits=12, decimal.mark=",", big.mark=",", small.mark="."), paste(as.character(pcttxcnt), "%", sep=""))
      colnames(txTableCnt) <- c("Stage", "#", "% of HIV+", "Rate/100PY", "# Tx", "% of Tx")
      
      return(txTableCnt)
    }
  })
  
  #http://stackoverflow.com/questions/23236944/add-values-to-a-reactive-table-in-shiny/23243820#23243820  
  
  observe({
    if (input$saveButton > 0){
      newData <- isolate(cbind(input$saveButton, input$scenarioName, currentData()))
      isolate(colnames(newData) <- c("Scenario", "Name", "Stage", "#", "% of HIV+", "Rate/100PY", "# Tx", "% of Tx"))
      isolate(colnames(values$df) <- c("Scenario", "Name", "Stage", "#", "% of HIV+", "Rate/100PY", "# Tx", "% of Tx"))
      isolate(values$df <- rbind(values$df, newData))
      isolate(values$df <- values$df[which(values$df$Scenario!=0),])
      
      isolate(updateSelectInput(session, "removeScenarioNum", choices=unique(values$df$"Scenario")))
      isolate(updateSelectInput(session, "compareScenario1", choices=unique(values$df$"Scenario")))
      isolate(updateSelectInput(session, "compareScenario2", choices=unique(values$df$"Scenario")))
#       isolate(updateSelectInput(session, "removeScenarioName", choices=unique(values$df$"Name")))
      isolate(updateTextInput(session, "scenarioName", value=""))
    }
  })

#   observe({
#     scenario <- unique(values$df$"Scenario")[as.numeric(grep(input$removeScenarioName, unique(values$df$"Name")))]
#     updateSelectInput(session, "removeScenarioNum", select=scenario)
#   })

#   observe({
#     name <- unique(values$df$"Name")[grep(as.numeric(input$removeScenarioNum), as.numeric(unique(values$df$"Scenario")))]
#     isolate(updateSelectInput(session, "removeScenarioName", select=name))
#   })
    
  observe({
    if (input$removeButton > 0){
      isolate(values$df <- values$df[which(values$df$"Scenario"!=input$removeScenarioNum),])
#       
#       updateSelectInput(session, "removeScenarioName", select=NULL)
#       updateSelectInput(session, "removeScenarioNum", select=NULL)
#       updateSelectInput(session, "removeScenarioNum", choices=unique(values$df$"Scenario"))
#       updateSelectInput(session, "removeScenarioName", choices=unique(values$df$"Name"))
    }
  })  
  
  output$txTableCnt <- renderDataTable({currentData()})
  
#  output$fullDataTable <- renderDataTable(values$df, options=list(pagelength=5))
  
  output$scenarioSummaryTable <- renderTable({
    if (input$saveButton > 0){
    scenarios <- unique(as.numeric(as.character(values$df$"Scenario")))
    scenario.names <- unique(as.character(values$df$"Name"))
    summary.data <- data.frame(matrix(nrow = length(scenarios), ncol = 14))
    
    # Cases
    total.hiv.cases <- aggregate(as.numeric(as.character(gsub(",", "", values$df$"#"))), by=list(as.character(values$df$Scenario)), FUN=sum)[2]
    
    undiagnosed.cases <- round(as.numeric(as.character(gsub(",", "", values$df$"#"[which(values$df$"Stage"==stagenames.mutex[1])]))))
    diagnosed.nr.cases <- round(as.numeric(as.character(gsub(",", "", values$df$"#"[which(values$df$"Stage"==stagenames.mutex[2])]))))
    retained.cases <- round(as.numeric(as.character(gsub(",", "", values$df$"#"[which(values$df$"Stage"==stagenames.mutex[3])]))))
    arv.cases <- round(as.numeric(as.character(gsub(",", "", values$df$"#"[which(values$df$"Stage"==stagenames.mutex[4])]))))
    suppressed.cases <- round(as.numeric(as.character(gsub(",", "", values$df$"#"[which(values$df$"Stage"==stagenames.mutex[5])]))))
    
    undiagnosed.percent <- round((undiagnosed.cases / total.hiv.cases) * 100, 1)
    diagnosed.nr.percent <- round((diagnosed.nr.cases / total.hiv.cases) * 100, 1)
    retained.percent <- round((retained.cases / total.hiv.cases) * 100, 1)
    arv.percent <- round((arv.cases / total.hiv.cases) * 100, 1)
    suppressed.percent <- round((suppressed.cases / total.hiv.cases) * 100, 1)

    total.hiv.cases.display <- lapply(1:nrow(total.hiv.cases), function(y) paste(format(total.hiv.cases[y,1], digits=12, decimal.mark=",", big.mark=",", small.mark=".")))
    undiagnosed.cases.display <- lapply(1:length(undiagnosed.cases), function(y) paste(format(undiagnosed.cases[y], digits=12, decimal.mark=",", big.mark=",", small.mark="."), " (", undiagnosed.percent[y,1], "%)", sep=""))
    diagnosed.nr.cases.display <- lapply(1:length(diagnosed.nr.cases), function(y) paste(format(diagnosed.nr.cases[y], digits=12, decimal.mark=",", big.mark=",", small.mark="."), " (", diagnosed.nr.percent[y,1], "%)", sep=""))
    retained.cases.display <- lapply(1:length(retained.cases), function(y) paste(format(retained.cases[y], digits=12, decimal.mark=",", big.mark=",", small.mark="."), " (", retained.percent[y,1], "%)", sep=""))
    arv.cases.display <- lapply(1:length(arv.cases), function(y) paste(format(arv.cases[y], digits=12, decimal.mark=",", big.mark=",", small.mark="."), " (", arv.percent[y,1], "%)", sep=""))
    suppressed.cases.display <- lapply(1:length(suppressed.cases), function(y) paste(format(suppressed.cases[y], digits=12, decimal.mark=",", big.mark=",", small.mark="."), " (", suppressed.percent[y,1], "%)", sep=""))

    # Scenario number
    scenario <- lapply(scenarios, function(x) paste(round(x, 0)))
    
    # Transmissions
    total.hiv.tx <- aggregate(as.numeric(as.character(gsub(",", "", values$df$"# Tx"))), by=list(as.character(values$df$Scenario)), FUN=sum)[2]
  
    undiagnosed.tx <- round(as.numeric(as.character(gsub(",", "", values$df$"# Tx"[which(values$df$"Stage"==stagenames.mutex[1])]))))
    diagnosed.nr.tx <- round(as.numeric(as.character(gsub(",", "", values$df$"# Tx"[which(values$df$"Stage"==stagenames.mutex[2])]))))
    retained.tx <- round(as.numeric(as.character(gsub(",", "", values$df$"# Tx"[which(values$df$"Stage"==stagenames.mutex[3])]))))
    arv.tx <- round(as.numeric(as.character(gsub(",", "", values$df$"# Tx"[which(values$df$"Stage"==stagenames.mutex[4])]))))
    suppressed.tx <- round(as.numeric(as.character(gsub(",", "", values$df$"# Tx"[which(values$df$"Stage"==stagenames.mutex[5])]))))

    undiagnosed.tx.rate <- as.numeric(as.character(gsub(",", "", values$df$"Rate/100PY"[which(values$df$"Stage"==stagenames.mutex[1])])))
    diagnosed.nr.tx.rate <- as.numeric(as.character(gsub(",", "", values$df$"Rate/100PY"[which(values$df$"Stage"==stagenames.mutex[2])])))
    retained.tx.rate <- as.numeric(as.character(gsub(",", "", values$df$"Rate/100PY"[which(values$df$"Stage"==stagenames.mutex[3])])))
    arv.tx.rate <- as.numeric(as.character(gsub(",", "", values$df$"Rate/100PY"[which(values$df$"Stage"==stagenames.mutex[4])])))
    suppressed.tx.rate <- as.numeric(as.character(gsub(",", "", values$df$"Rate/100PY"[which(values$df$"Stage"==stagenames.mutex[5])])))
    
    undiagnosed.tx.percent <- round((undiagnosed.tx / total.hiv.tx) * 100, 1)
    diagnosed.nr.tx.percent <- round((diagnosed.nr.tx / total.hiv.tx) * 100, 1)
    retained.tx.percent <- round((retained.tx / total.hiv.tx) * 100, 1)
    arv.tx.percent <- round((arv.tx / total.hiv.tx) * 100, 1)
    suppressed.tx.percent <- round((suppressed.tx / total.hiv.tx) * 100, 1)
    
    total.hiv.tx.display <- lapply(1:nrow(total.hiv.tx), function(y) paste(format(total.hiv.tx[y,1], digits=12, decimal.mark=",", big.mark=",", small.mark=".")))
    undiagnosed.tx.display <- lapply(1:length(undiagnosed.tx), function(y) paste(format(undiagnosed.tx[y], digits=12, decimal.mark=",", big.mark=",", small.mark="."), " (", undiagnosed.tx.percent[y,1], "%); ", format(undiagnosed.tx.rate[y], digits=3, small.mark=","), sep=""))
    diagnosed.nr.tx.display <- lapply(1:length(diagnosed.nr.tx), function(y) paste(format(diagnosed.nr.tx[y], digits=12, decimal.mark=",", big.mark=",", small.mark="."), " (", diagnosed.nr.tx.percent[y,1], "%); ", format(diagnosed.nr.tx.rate[y], digits=3, small.mark=","), sep=""))
    retained.tx.display <- lapply(1:length(retained.tx), function(y) paste(format(retained.tx[y], digits=12, decimal.mark=",", big.mark=",", small.mark="."), " (", retained.tx.percent[y,1], "%); ", format(retained.tx.rate[y], digits=3, small.mark=","), sep=""))
    arv.tx.display <- lapply(1:length(arv.tx), function(y) paste(format(arv.tx[y], digits=12, decimal.mark=",", big.mark=",", small.mark="."), " (", arv.tx.percent[y,1], "%); ", format(arv.tx.rate[y], digits=3, small.mark=","), sep=""))
    suppressed.tx.display <- lapply(1:length(suppressed.tx), function(y) paste(format(suppressed.tx[y], digits=12, decimal.mark=",", big.mark=",", small.mark="."), " (", suppressed.tx.percent[y,1], "%); ", format(suppressed.tx.rate[y], digits=3, small.mark=","), sep=""))
    
    
    # Create table
    colnames(summary.data) <- c("Scenario #", "Scenario Name", "Total HIV+", "Undiagnosed\n# (%)", "Diagnosed, not retained in care\n# (%)", "Retained in care, no ARVs\n# (%)", "ARVs, not suppressed\n# (%)", "Suppressed\n# (%)", "Total Tx", "Tx from undiagnosed\n# (%); rate/100 PY", "Tx from diagnosed, not retained in care\n# (%); rate/100 PY", "Tx from retained in care, no ARVs\n# (%); rate/100 PY", "Tx from ARVs, not suppressed\n# (%); rate/100 PY", "Tx from suppressed\n# (%); rate/100 PY")
    summary.data[,1] <- unlist(scenario)
    summary.data[,2] <- scenario.names
    summary.data[,3] <- unlist(total.hiv.cases.display)
    summary.data[,4] <- unlist(undiagnosed.cases.display)
    summary.data[,5] <- unlist(diagnosed.nr.cases.display)
    summary.data[,6] <- unlist(retained.cases.display)
    summary.data[,7] <- unlist(arv.cases.display)
    summary.data[,8] <- unlist(suppressed.cases.display)
    summary.data[,9] <- unlist(total.hiv.tx.display)
    summary.data[,10] <- unlist(undiagnosed.tx.display)
    summary.data[,11] <- unlist(diagnosed.nr.tx.display)
    summary.data[,12] <- unlist(retained.tx.display)
    summary.data[,13] <- unlist(arv.tx.display)
    summary.data[,14] <- unlist(suppressed.tx.display)
    
    summary.data
  }}, align=rep("r", 15), include.rownames=FALSE
  )

  output$compareTxBar <- renderPlot({
    if (input$compareScenarios>0){
      if (isolate(input$compareScenario1>0) & isolate(input$compareScenario2>0) & isolate(input$compareScenario1!=input$compareScenario2)){
        bars <- matrix(nrow=5, ncol=2)
        bars[,1] <- unlist(lapply(c(1:5), function(x) round(as.numeric(as.character(gsub(",", "", values$df$"# Tx"[which(values$df$"Stage"==stagenames.mutex[x] & values$df$"Scenario"==isolate(input$compareScenario1))]))))))
        bars[,2] <- unlist(lapply(c(1:5), function(x) round(as.numeric(as.character(gsub(",", "", values$df$"# Tx"[which(values$df$"Stage"==stagenames.mutex[x] & values$df$"Scenario"==isolate(input$compareScenario2))]))))))
        barplot(bars, names.arg=c(values$df$"Name"[which(values$df$"Scenario"==isolate(input$compareScenario1))][1], values$df$"Name"[which(values$df$"Scenario"==isolate(input$compareScenario2))][1]), col=colors)
      }
    }
  })

  output$comparisonText1 <- renderText({
    if (input$compareScenarios>0){
      if (isolate(input$compareScenario1>0) & isolate(input$compareScenario2>0) & isolate(input$compareScenario1!=input$compareScenario2)){
        
        name1 <- values$df$"Name"[which(values$df$"Scenario"==isolate(input$compareScenario1))][1]
        name2 <- values$df$"Name"[which(values$df$"Scenario"==isolate(input$compareScenario2))][1]
        
        total1 <- sum(as.numeric(as.character(gsub(",", "", values$df$"# Tx"[which(values$df$"Scenario"==isolate(input$compareScenario1))]))))
        total2 <- sum(as.numeric(as.character(gsub(",", "", values$df$"# Tx"[which(values$df$"Scenario"==isolate(input$compareScenario2))]))))
        
        difference <- total2 - total1
        abs.difference <- abs(difference)
        if (difference < 0) comparison.phrase <- paste(", or ", format(abs.difference, digits=12, decimal.mark=",", big.mark=",", small.mark="."), " fewer HIV transmissions than \"", name1, ".\"", sep="")
        if (difference > 0) comparison.phrase <- paste(", or ", format(abs.difference, digits=12, decimal.mark=",", big.mark=",", small.mark="."), " more HIV transmissions than \"", name1, ".\"", sep="")
        if (difference == 0) comparison.phrase <- paste(", the same number of HIV transmissions as \"", name1, ".\"", sep="")
        
        comparison.text <- paste("The first comparison scenario, \"", name1, ",\" resulted in ", format(total1, digits=12, decimal.mark=",", big.mark=",", small.mark="."), " total HIV transmissions. The second comparison scenario, \"", name2, ",\" resulted in ", format(total2, digits=12, decimal.mark=",", big.mark=",", small.mark="."), " total HIV transmissions", comparison.phrase, sep="")
        
        return(comparison.text)
      }
    }
  })

  output$downloadReport <- downloadHandler(
    filename = function(){
      paste("Custom HIV Continuum Report ", Sys.Date(), ".docx", sep="")
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
