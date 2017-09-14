library(shiny)
library(knitr)
library(rmarkdown)

options("scipen"=100)

stagenames.casc <- c("Undiagnosed", "Diagnosed", "Retained in care", "Prescribed ARVs", "Suppressed")
stagenames.mutex <- c("Undiagnosed", "Diagnosed, not retained", "Retained, no ARVs", "ARVs, not suppressed", "Suppressed")
colors <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0")

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
    isTRUE(input$password != "CAMPnCDC4eva!")
  })
  
  output$correct <- reactive({
    isTRUE(input$password == "CAMPnCDC4eva!")
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
  
  observe({    
    tothiv <- input$diagnosedcnt1
    updateNumericInput(session, "totalcnt", value=round(tothiv/(1-(input$undiagnosedpct/100))))
    updateNumericInput(session, "diagnosedcnt2", value=tothiv)
    updateNumericInput(session, "diagnosedretcnt", value=round((input$diagnosedretpct/100)*tothiv))
    updateNumericInput(session, "diagnosedarvcnt", value=round((input$diagnosedarvpct/100)*tothiv))
    updateNumericInput(session, "diagnosedsupcnt", value=round((input$diagnosedsuppct/100)*tothiv))
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
  
  output$continuumPlotCntCasc <- renderPlot({
    if (is.na(input$undiagnosedcnt.mutex)==FALSE & is.na(input$diagnosednrcnt.mutex)==FALSE & is.na(input$diagnosedretcnt.mutex)==FALSE & is.na(input$diagnosedarvcnt.mutex)==FALSE & is.na(input$diagnosedsupcnt.mutex)==FALSE){
      stagescnt <- c(sum(input$undiagnosedcnt.mutex, input$diagnosednrcnt.mutex, input$diagnosedretcnt.mutex, input$diagnosedarvcnt.mutex, input$diagnosedsupcnt.mutex), sum(input$diagnosednrcnt.mutex, input$diagnosedretcnt.mutex, input$diagnosedarvcnt.mutex, input$diagnosedsupcnt.mutex), sum(input$diagnosedretcnt.mutex, input$diagnosedarvcnt.mutex, input$diagnosedsupcnt.mutex), sum(input$diagnosedarvcnt.mutex, input$diagnosedsupcnt.mutex), input$diagnosedsupcnt.mutex)
      barplot(stagescnt, col=colors, names.arg=stagenames.casc, cex.names=.8, main="HIV Care Continuum", sub="Cascade", ylim=c(0,max(stagescnt)))
    }
  })  
  
  output$continuumPlotCntSep <- renderPlot({
    if (is.na(input$undiagnosedcnt.mutex)==FALSE & is.na(input$diagnosednrcnt.mutex)==FALSE & is.na(input$diagnosedretcnt.mutex)==FALSE & is.na(input$diagnosedarvcnt.mutex)==FALSE & is.na(input$diagnosedsupcnt.mutex)==FALSE){
      stagescnt <- c(input$undiagnosedcnt.mutex, input$diagnosednrcnt.mutex, input$diagnosedretcnt.mutex, input$diagnosedarvcnt.mutex, input$diagnosedsupcnt.mutex)
      barplot(stagescnt, col=colors, main="HIV Care Continuum", sub="Mutually Exclusive", ylim=c(0,max(stagescnt)))
      text(cex=.8, x=c(1:5), y=-2, stagenames.mutex, xpd=TRUE, srt=45)
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
  
  output$txTableCnt <- renderDataTable({
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
      
      txTableCnt
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
