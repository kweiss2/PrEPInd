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
  100 * c(0.379851362510322, 0.271209564499336, 0.293217965748752, 0.638007147361783, 0.180470885011562, 0.141580828253101, 0.365408038976857, 0.20584652862363, 0.386520503451076),   #National Average
  100 * c(0.689102564102564, 0.0416666666666667, 0.221153846153846, 0.790540540540541, 0.0202702702702703, 0.141891891891892, 0.523809523809524, 0.0476190476190476, 0.428571428571429),   #Alabama
  100 * c(0.133333333333333, 0.133333333333333, 0.4, 0.222222222222222, 0, 0.222222222222222, 0, 0, 0),   #Alaska
  100 * c(0.136194029850746, 0.373134328358209, 0.384328358208955, 0.408333333333333, 0.266666666666667, 0.191666666666667, 0.129032258064516, 0.387096774193548, 0.354838709677419),   #Arizona
  100 * c(0.489247311827957, 0.0645161290322581, 0.419354838709677, 0.666666666666667, 0.111111111111111, 0.203703703703704, 0.5, 0.0625, 0.4375),   #Arkansas
  100 * c(0.161224489795918, 0.453316326530612, 0.295408163265306, 0.36234458259325, 0.394316163410302, 0.172291296625222, 0.222222222222222, 0.299145299145299, 0.423076923076923),   #California
  100 * c(0.11400651465798, 0.309446254071661, 0.543973941368078, 0.431372549019608, 0.294117647058824, 0.254901960784314, 0.230769230769231, 0.384615384615385, 0.384615384615385),   #Colorado
  100 * c(0.26219512195122, 0.310975609756098, 0.414634146341463, 0.574712643678161, 0.28735632183908, 0.0919540229885057, 0.3, 0.2, 0.4),   #Connecticut
  100 * c(0.446153846153846, 0.0769230769230769, 0.446153846153846, 0.7, 0.125, 0.175, 0.25, 0.5, 0.25),   #Delaware
  100 * c(0.323529411764706, 0.373626373626374, 0.279573367808662, 0.639429312581064, 0.190661478599222, 0.147859922178988, 0.325358851674641, 0.186602870813397, 0.473684210526316),   #Florida
  100 * c(0.721991701244813, 0.0788381742738589, 0.168346176644932, 0.830252100840336, 0.0453781512605042, 0.107563025210084, 0.729166666666667, 0.0208333333333333, 0.21875),   #Georgia
  100 * c(0.0919540229885057, 0.0919540229885057, 0.35632183908046, 0.0588235294117647, 0.117647058823529, 0.352941176470588, 0.181818181818182, 0, 0.363636363636364),   #Hawaii
  100 * c(0.037037037037037, 0.296296296296296, 0.62962962962963, 0.777777777777778, 0, 0.222222222222222, 0.5, 0, 0),   #Idaho
  100 * c(0.453250222617988, 0.231522707034728, 0.254674977738201, 0.703971119133574, 0.129963898916967, 0.122743682310469, 0.566037735849057, 0.0943396226415094, 0.30188679245283),   #Illinois
  100 * c(0.425981873111782, 0.105740181268882, 0.441087613293051, 0.504201680672269, 0.109243697478992, 0.327731092436975, 0.0276243093922652, 0.0110497237569061, 0.950276243093923),   #Indiana
  100 * c(0.152941176470588, 0.129411764705882, 0.658823529411765, 0.392857142857143, 0.107142857142857, 0.357142857142857, 0, 0.166666666666667, 0.75),   #Iowa
  100 * c(0.221238938053097, 0.265486725663717, 0.433628318584071, 0.607142857142857, 0.142857142857143, 0.25, 0.4, 0.2, 0.2),   #Kansas
  100 * c(0.34, 0.048, 0.584, 0.567164179104478, 0.0149253731343284, 0.328358208955224, 0.222222222222222, 0, 0.777777777777778),   #Kentucky
  100 * c(0.672340425531915, 0.0581560283687943, 0.252482269503546, 0.846846846846847, 0.048048048048048, 0.0960960960960961, 0.625, 0.0681818181818182, 0.284090909090909),   #Louisiana
  100 * c(0.1, 0.133333333333333, 0.766666666666667, 0.888888888888889, 0, 0, 0.2, 0.2, 0.6),   #Maine
  100 * c(0.707792207792208, 0.087012987012987, 0.158441558441558, 0.85513078470825, 0.0563380281690141, 0.0603621730382294, 0.618421052631579, 0.0526315789473684, 0.289473684210526),   #Maryland
  100 * c(0.168115942028985, 0.272463768115942, 0.478260869565217, 0.577142857142857, 0.274285714285714, 0.131428571428571, 0.289473684210526, 0.315789473684211, 0.381578947368421),   #Massachusetts
  100 * c(0.586715867158672, 0.0645756457564576, 0.309963099630996, 0.759124087591241, 0.0437956204379562, 0.18978102189781, 0.523809523809524, 0.0238095238095238, 0.428571428571429),   #Michigan
  100 * c(0.306532663316583, 0.100502512562814, 0.527638190954774, 0.76, 0.04, 0.146666666666667, 0.5, 0.0833333333333333, 0.333333333333333),   #Minnesota
  100 * c(0.790368271954674, 0.028328611898017, 0.164305949008499, 0.814814814814815, 0.0296296296296296, 0.111111111111111, 0.8, 0, 0.15),   #Mississippi
  100 * c(0.416909620991254, 0.0466472303206997, 0.481049562682216, 0.711340206185567, 0.0412371134020619, 0.195876288659794, 0.32, 0.08, 0.56),   #Missouri
  100 * c(0.0666666666666667, 0.2, 0.733333333333333, 0, 0, 1, 0, 0, 0.5),   #Montana
  100 * c(0.140625, 0.140625, 0.625, 0.4375, 0.125, 0.375, 0.5, 0, 0),   #Nebraska
  100 * c(0.208226221079692, 0.339331619537275, 0.344473007712082, 0.5, 0.185714285714286, 0.228571428571429, 0.222222222222222, 0.111111111111111, 0.62962962962963),   #Nevada
  100 * c(NA, NA, NA, NA, NA, NA, NA, NA, NA),   #New Hampshire
  100 * c(0.356204379562044, 0.388321167883212, 0.207299270072993, 0.604838709677419, 0.271505376344086, 0.0967741935483871, 0.4921875, 0.25, 0.234375),   #New Jersey
  100 * c(0.0454545454545455, 0.536363636363636, 0.318181818181818, 0, 0.583333333333333, 0.166666666666667, 0, 0.923076923076923, 0),   #New Mexico
  100 * c(0.308647873865265, 0.363115145723841, 0.233635929288103, 0.627249357326478, 0.22107969151671, 0.089974293059126, 0.477732793522267, 0.319838056680162, 0.157894736842105),   #New York
  100 * c(0.602116402116402, 0.0994708994708995, 0.255026455026455, 0.743975903614458, 0.0753012048192771, 0.13855421686747, 0.654545454545455, 0.0363636363636364, 0.290909090909091),   #North Carolina
  100 * c(0.181818181818182, 0.0909090909090909, 0.545454545454545, 0.888888888888889, 0, 0, 0.333333333333333, 0.333333333333333, 0.333333333333333),   #North Dakota
  100 * c(0.485925925925926, 0.0533333333333333, 0.420740740740741, 0.654639175257732, 0.0515463917525773, 0.237113402061856, 0.315789473684211, 0.0526315789473684, 0.596491228070175),   #Ohio
  100 * c(0.224, 0.144, 0.476, 0.346153846153846, 0.0961538461538462, 0.346153846153846, 0.0588235294117647, 0.0588235294117647, 0.705882352941177),   #Oklahoma
  100 * c(0.0538922155688623, 0.167664670658683, 0.730538922155689, 0.375, 0.125, 0.375, 0, 0.1, 0.8),   #Oregon
  100 * c(0.495677233429395, 0.10806916426513, 0.360230547550432, 0.642131979695431, 0.157360406091371, 0.17258883248731, 0.353658536585366, 0.24390243902439, 0.365853658536585),   #Pennsylvania
  100 * c(0.128205128205128, 0.358974358974359, 0.461538461538462, 0.523809523809524, 0.238095238095238, 0.19047619047619, 0.25, 0.5, 0.25),   #Rhode Island
  100 * c(0.65301724137931, 0.0711206896551724, 0.254310344827586, 0.776595744680851, 0.0531914893617021, 0.159574468085106, 0.666666666666667, 0.119047619047619, 0.19047619047619),   #South Carolina
  100 * c(0.166666666666667, 0.0833333333333333, 0.666666666666667, 0.444444444444444, 0.111111111111111, 0.333333333333333, 0, 1, 0),   #South Dakota
  100 * c(0.572314049586777, 0.0661157024793388, 0.340909090909091, 0.684491978609626, 0.053475935828877, 0.245989304812834, 0.333333333333333, 0, 0.619047619047619),   #Tennessee
  100 * c(0.319524824855315, 0.409381663113006, 0.229667986597624, 0.536687631027254, 0.30293501048218, 0.120545073375262, 0.382978723404255, 0.285106382978723, 0.297872340425532),   #Texas
  100 * c(0.03125, 0.270833333333333, 0.625, 0.333333333333333, 0.266666666666667, 0.333333333333333, 0, 0.2, 0.6),   #Utah
  100 * c(NA, 0.2, 0.8, NA, 0, 0.75, NA, 0.333333333333333, 0.666666666666667),   #Vermont
  100 * c(0.564759036144578, 0.128012048192771, 0.268072289156627, 0.761538461538461, 0.0692307692307692, 0.138461538461538, 0.53125, 0.09375, 0.3125),   #Virginia
  100 * c(0.148255813953488, 0.209302325581395, 0.523255813953488, 0.548387096774194, 0.0967741935483871, 0.241935483870968, 0.131578947368421, 0.131578947368421, 0.631578947368421),   #Washington
  100 * c(0.0566037735849057, 0.0377358490566038, 0.867924528301887, 0.533333333333333, 0, 0.466666666666667, 0.2, 0, 0.4),   #West Virginia
  100 * c(0.378378378378378, 0.151351351351351, 0.394594594594595, 0.583333333333333, 0.138888888888889, 0.25, 0.375, 0.25, 0.375),   #Wisconsin
  100 * c(0.142857142857143, 0.285714285714286, 0.571428571428571, 0, 0, 0.75, 0.333333333333333, 0.333333333333333, 0.333333333333333),   #Wyoming
  100 * c(0.594262295081967, 0.204918032786885, 0.163934426229508, 0.974137931034483, 0.00862068965517241, 0.00862068965517241, 0.857142857142857, 0.0476190476190476, 0.0476190476190476))   # Washington, District of Columbia

# MSM, HET, PWID
trans.diagnosis.percents <- list(
  100 * c(813970/1144550, 258080/1144550, 72510/1144550),  # National Average
  100 * c(7680/11840, 3640/11840, 520/11840),  # Alabama
  100 * c(1470/2360, 880/2360, 0/2360),  # Alaska
  100 * c(18920/25350, 4240/25350, 2190/25350),  # Arizona
  100 * c(3350/4610, 970/4610, 290/4610),  # Arkansas
  100 * c(129820/156210, 18640/156210, 7750/156210),  # California
  100 * c(20110/24310, 3340/24310, 850/24310),  # Colorado
  100 * c(5830/9640, 3090/9640, 710/9640),  # Connecticut
  100 * c(2390/4010, 1470/4010, 150/4010),  # Delaware
  100 * c(73570/115200, 36670/115200, 4970/115200),  # Florida
  100 * c(25330/35700, 8930/35700, 1440/35700),  # Georgia
  100 * c(3700/4890, 720/4890, 470/4890),  # Hawaii
  100 * c(2740/3860, 910/3860, 200/3860),  # Idaho
  100 * c(39600/51240, 9770/51240, 1870/51240),  # Illinois
  100 * c(12320/23480, 4430/23480, 6740/23480),  # Indiana
  100 * c(2840/4180, 940/4180, 400/4180),  # Iowa
  100 * c(3410/4400, 840/4400, 150/4400),  # Kansas
  100 * c(9100/12190, 2440/12190, 660/12190),  # Kentucky
  100 * c(8380/13390, 3960/13390, 1050/13390),  # Louisiana
  100 * c(2220/3250, 660/3250, 370/3250),  # Maine
  100 * c(15700/27390, 10130/27390, 1550/27390),  # Maryland
  100 * c(12670/21890, 6430/21890, 2790/21890),  # Massachusetts
  100 * c(20700/27540, 5230/27540, 1600/27540),  # Michigan
  100 * c(15180/21820, 5720/21820, 920/21820),  # Minnesota
  100 * c(3480/5010, 1330/5010, 200/5010),  # Mississippi
  100 * c(13220/17930, 3740/17930, 960/17930),  # Missouri
  100 * c(1880/2500, 380/2500, 250/2500),  # Montana
  100 * c(1930/2470, 480/2470, 60/2470),  # Nebraska
  100 * c(7770/9710, 1400/9710, 540/9710),  # Nevada
  100 * c(1890/2650, 380/2650, 380/2650),  # New Hampshire
  100 * c(15380/26610, 8350/26610, 2870/26610),  # New Jersey
  100 * c(4560/5600, 500/5600, 540/5600),  # New Mexico
  100 * c(48740/72610, 18120/72610, 5750/72610),  # New York
  100 * c(21160/29820, 7430/29820, 1230/29820),  # North Carolina
  100 * c(630/1320, 510/1320, 170/1320),  # North Dakota
  100 * c(27780/38110, 7980/38110, 2350/38110),  # Ohio
  100 * c(7170/9140, 1490/9140, 490/9140),  # Oklahoma
  100 * c(13360/16880, 1920/16880, 1600/16880),  # Oregon
  100 * c(21380/36050, 12140/36050, 2530/36050),  # Pennsylvania
  100 * c(2970/4880, 1600/4880, 300/4880),  # Rhode Island
  100 * c(6040/9040, 2450/9040, 550/9040),  # South Carolina
  100 * c(710/1290, 530/1290, 60/1290),  # South Dakota
  100 * c(15530/22880, 6000/22880, 1350/22880),  # Tennessee
  100 * c(86020/117180, 25000/117180, 6160/117180),  # Texas
  100 * c(5650/6830, 880/6830, 290/6830),  # Utah
  100 * c(1120/2690, 900/2690, 670/2690),  # Vermont
  100 * c(22490/32380, 8810/32380, 1080/32380),  # Virginia
  100 * c(24130/31150, 4350/31150, 2670/31150),  # Washington
  100 * c(2220/3060, 630/3060, 210/3060),  # West Virginia
  100 * c(9840/12180, 1910/12180, 430/12180),  # Wisconsin
  100 * c(1010/2030, 580/2030, 430/2030),  # Wyoming
  100 * c(8850/13820, 4210/13820, 760/13820)# DC

)

### Jurisdiction ---------------------------------------------------------------
# input$jurisdiction <- c("Total" = 1, "Alabama" = 2,
#                   "Alaska" = 3, "Arizona" = 4,
#                   "Arkansas" = 5, "California" = 6,
#                   "Colorado" = 7, "Connecticut" = 8,
#                   "Delaware" = 9, "Florida" = 10,
#                   "Georgia" = 11, "Hawaii" = 12,
#                   "Idaho" = 13, "Illinois" = 14,
#                   "Indiana" = 15, "Iowa" = 16,
#                   "Kansas" = 17, "Kentucky" = 18,
#                   "Louisiana" = 19, "Maine" = 20,
#                   "Maryland" = 21, "Massachusetts" = 22,
#                   "Michigan" = 23, "Minnesota" = 24,
#                   "Mississippi" = 25, "Missouri" = 26,
#                   "Montana" = 27, "Nebraska" = 28,
#                   "Nevada" = 29, "New Hampshire" = 30,
#                   "New Jersey" = 31, "New Mexico" = 32,
#                   "New York" = 33, "North Carolina" = 34,
#                   "North Dakota" = 35, "Ohio" = 36,
#                   "Oklahoma" = 37, "Oregon" = 38,
#                   "Pennsylvania" = 39, "Rhode Island" = 40,
#                   "South Carolina" = 41, "South Dakota" = 42,
#                   "Tennessee" = 43, "Texas" = 44,
#                   "Utah" = 45, "Vermont" = 46,
#                   "Virginia" = 47, "Washington" = 48,
#                   "West Virginia" = 49, "Wisconsin" = 50,
#                   "Wyoming" = 51, "D.C." = 52)

shinyServer(function(input, output, session) {

  ## Get diagnosis percents ----------------------------------------------------
  observe({
    # Transmission Risk Group within race
    # Black among MSM, Hisp among MSM, white among MSM
    # Black among HET, Hisp among HET, white among HET
    # Black among PWID,Hisp among PWID, white among PWID
    x <- race.diagnosis.percents[[as.numeric(input$jurisdiction)]]
    updateNumericInput(session, "blackmsmdiagpct", value = round(x[1], 2))
    updateNumericInput(session, "blackhetdiagpct", value = round(x[4], 2))
    updateNumericInput(session, "blackpwiddiagpct", value = round(x[7], 2))
    updateNumericInput(session, "hispmsmdiagpct", value = round(x[2], 2))
    updateNumericInput(session, "hisphetdiagpct", value = round(x[5], 2))
    updateNumericInput(session, "hisppwiddiagpct", value = round(x[8], 2))
    updateNumericInput(session, "whitemsmdiagpct", value = round(x[3], 2))
    updateNumericInput(session, "whitehetdiagpct", value = round(x[6], 2))
    updateNumericInput(session, "whitepwiddiagpct", value = round(x[9], 2))

    # MSM, HET, PWID
    # Transmission Risk Group only
    y <- trans.diagnosis.percents[[as.numeric(input$jurisdiction)]]
    updateNumericInput(session, "msmdiagpct", value = round(y[1], 2))
    updateNumericInput(session, "hetdiagpct", value = round(y[2], 2))
    updateNumericInput(session, "pwiddiagpct", value = round(y[3], 2))
  })

  ## Calculate number indicated for PrEP -------------------------------------
  observe({

    totmsm <- as.numeric(input$msmpopsize)
    x <- race.diagnosis.percents[[as.numeric(input$jurisdiction)]]
    y <- trans.diagnosis.percents[[as.numeric(input$jurisdiction)]]

    updateNumericInput(session, "totalmsm",
                       value = round_any(totmsm, 10))
    updateNumericInput(session, "msmprep",
                       value = round_any((totmsm * 0.247), 10))
    updateNumericInput(session, "hetprep",
                       value = round_any(((totmsm * 0.247) * (as.numeric(input$hetdiagpct) / as.numeric(input$msmdiagpct))), 10))
    updateNumericInput(session, "pwidprep",
                       value = round_any(((totmsm * 0.247) * (as.numeric(input$pwiddiagpct) / as.numeric(input$msmdiagpct))), 10))
    updateNumericInput(session, "totalprep",
                       value = round_any((totmsm * 0.247) +
                         ((totmsm * 0.247) * (as.numeric(input$hetdiagpct) / as.numeric(input$msmdiagpct))) +
                         ((totmsm * 0.247) * (as.numeric(input$pwiddiagpct) / as.numeric(input$msmdiagpct))), 10))
    updateNumericInput(session, "blackmsmprep",
                       value = round_any((totmsm * 0.247) * (as.numeric(input$blackmsmdiagpct) / 100), 10))
    updateNumericInput(session, "blackhetprep",
                       value = round_any((totmsm * 0.247) * (as.numeric(input$hetdiagpct) / as.numeric(input$msmdiagpct)) * (as.numeric(input$blackhetdiagpct) / 100), 10))
    updateNumericInput(session, "blackpwidprep",
                       value = round_any((totmsm * 0.247) * (as.numeric(input$pwiddiagpct) / as.numeric(input$msmdiagpct)) * (as.numeric(input$blackpwiddiagpct) / 100), 10))
    updateNumericInput(session, "hispmsmprep",
                       value = round_any((totmsm * 0.247) * ((as.numeric(input$hispmsmdiagpct)) / 100), 10))
    updateNumericInput(session, "hisphetprep",
                       value = round_any((totmsm * 0.247) * (as.numeric(input$hetdiagpct) / as.numeric(input$msmdiagpct)) * (as.numeric(input$hisphetdiagpct) / 100), 10))
    updateNumericInput(session, "hisppwidprep",
                       value = round_any((totmsm * 0.247) * (as.numeric(input$pwiddiagpct) / as.numeric(input$msmdiagpct)) * (as.numeric(input$hisppwiddiagpct) / 100), 10))
    updateNumericInput(session, "whitemsmprep",
                       value = round_any((totmsm * 0.247) * (as.numeric(input$whitemsmdiagpct) / 100), 10))
    updateNumericInput(session, "whitehetprep",
                       value = round_any((totmsm * 0.247) * (as.numeric(input$hetdiagpct) / as.numeric(input$msmdiagpct)) * as.numeric(input$whitehetdiagpct) / 100, 10))
    updateNumericInput(session, "whitepwidprep",
                       value = round_any((totmsm * 0.247) * (as.numeric(input$pwiddiagpct) / as.numeric(input$msmdiagpct)) * as.numeric(input$whitepwiddiagpct) / 100, 10))


  ## NH and VT values for race--------------------------------------------------

  # Vermont values
    if (input$jurisdiction == 46) {
      updateNumericInput(session, "blackmsmprep",
                         value = 0)
      updateNumericInput(session, "blackhetprep",
                         value = 0)
      updateNumericInput(session, "blackpwidprep",
                         value = 0)
    }

  # New Hampshire values
    if (input$jurisdiction == 30) {
      updateNumericInput(session, "blackmsmprep",
                         value = 0)
      updateNumericInput(session, "blackhetprep",
                         value = 0)
      updateNumericInput(session, "blackpwidprep",
                         value = 0)
      updateNumericInput(session, "hispmsmprep",
                         value = 0)
      updateNumericInput(session, "hisphetprep",
                         value = 0)
      updateNumericInput(session, "hisppwidprep",
                         value = 0)
      updateNumericInput(session, "whitemsmprep",
                         value = 0)
      updateNumericInput(session, "whitehetprep",
                         value = 0)
      updateNumericInput(session, "whitepwidprep",
                         value = 0)
    }

  })

  ## Switch tabs ---------------------------------------------------------------
  #if (is.null(input$tabs)) return()

  observeEvent(input$switchtab, {
    newtab <- switch(input$tabs,
                     "Introduction" = "Estimation")
    updateTabItems(session, "tabs", newtab)
    })

  observeEvent(input$switchtab2, {
    newtab <- switch(input$tabs,
                     "Estimation" = "Introduction")
    updateTabItems(session, "tabs", newtab)
  })

  ## Warning text --------------------------------------------------------------

    # State warnings
  output$warningText1 <- renderText({
    warning.text <- ""
    if (input$jurisdiction == 46) {
      warning.text <- "African-American estimates by transmission risk group are not available for Vermont"
    }
    if (input$jurisdiction == 30) {
      warning.text <- "Race-specific estimates by transmission risk group are not available for New Hampshire"
    }

    return(warning.text)
  })

  # Changing transmission risk group assumptions
  output$warningText2 <- renderText({

    warning.text <- ""
    y <- trans.diagnosis.percents[[as.numeric(input$jurisdiction)]]
    if (is.na(input$msmdiagpct) == FALSE & is.na(input$hetdiagpct) == FALSE & is.na(input$pwiddiagpct) == FALSE) {

      if (input$msmdiagpct != y[1]) {
        warning.text <- "You are changing the jurisdiction assumptions."
      }
      if (input$hetdiagpct != y[2]) {
        warning.text <- "You are changing the jurisdiction assumptions"
      }
      if (input$pwiddiagpct != y[3]) {
        warning.text <- "You are changing the jurisdiction assumptions"
      }
    }

    return(warning.text)
  })

  # Changing race assumptions
  output$warningText3 <- renderText({

      warning.text <- ""
      x <- race.diagnosis.percents[[as.numeric(input$jurisdiction)]]

      if (is.na(input$blackmsmdiagpct) == FALSE & is.na(input$blackhetdiagpct) == FALSE & is.na(input$blackpwiddiagpct) == FALSE &
          is.na(input$hispmsmdiagpct) == FALSE & is.na(input$hisphetdiagpct) == FALSE & is.na(input$hisppwiddiagpct) == FALSE &
          is.na(input$whitemsmdiagpct) == FALSE & is.na(input$whitehetdiagpct) == FALSE & is.na(input$whitepwiddiagpct) == FALSE) {


          if (input$blackmsmdiagpct != x[1]) {
            warning.text <- "You are changing the jurisdiction assumptions"
          }
          if (input$blackhetdiagpct != x[2]) {
            warning.text <- "You are changing the jurisdiction assumptions"
          }
          if (input$blackpwiddiagpct != x[3]) {
            warning.text <- "You are changing the jurisdiction assumptions"
          }
          if (input$hispmsmdiagpct != x[4]) {
            warning.text <- "You are changing the jurisdiction assumptions"
          }
          if (input$hisphetdiagpct != x[5]) {
            warning.text <- "You are changing the jurisdiction assumptions"
          }
          if (input$hisppwiddiagpct != x[6]) {
            warning.text <- "You are changing the jurisdiction assumptions"
          }
          if (input$whitemsmdiagpct != x[7]) {
            warning.text <- "You are changing the jurisdiction assumptions"
          }
          if (input$whitehetdiagpct != x[8]) {
            warning.text <- "You are changing the jurisdiction assumptions"
          }
          if (input$whitepwiddiagpct != x[9]) {
            warning.text <- "You are changing the jurisdiction assumptions"
          }
      }

      return(warning.text)
    })

  # Sum of transmission risk group
  output$warningText4 <- renderText({

    warning.text <- ""
    if (is.na(input$msmdiagpct) == FALSE & is.na(input$hetdiagpct) == FALSE & is.na(input$pwiddiagpct) == FALSE) {

      if (sum(round(input$msmdiagpct, 4), round(input$hetdiagpct, 4), round(input$pwiddiagpct, 4)) > 100.0) {
        warning.text <- "The sum of categories must not be greater than 100%."
      }
    }
    return(warning.text)
  })

  # MSM sum
  output$warningText5 <- renderText({

    warning.text <- ""
    if (is.na(input$blackmsmdiagpct) == FALSE & is.na(input$whitemsmdiagpct) == FALSE & is.na(input$whitemsmdiagpct) == FALSE) {

      if (sum(c(round(input$blackmsmdiagpct, 4), round(input$hispmsmdiagpct, 4), round(input$whitemsmdiagpct, 4))) > 100.0) {
        warning.text <- "The sum of MSM by race must not be greater than 100%."
      }
    }
    return(warning.text)
  })

  # HET sum
  output$warningText6 <- renderText({

    warning.text <- ""
    if (is.na(input$blackhetdiagpct) == FALSE & is.na(input$whitehetdiagpct) == FALSE & is.na(input$hisphetdiagpct) == FALSE) {

      if (sum(c(round(input$blackhetdiagpct, 4), round(input$whitehetdiagpct, 4), round(input$hisphetdiagpct, 4))) > 100.0) {
      warning.text <- "The sum of HET by race must not be greater than 100%."
      }
    }

    return(warning.text)
  })

  # Diag sum
  output$warningText7 <- renderText({
    warning.text <- ""

    if (is.na(input$blackpwiddiagpct) == FALSE & is.na(input$whitepwiddiagpct) == FALSE & is.na(input$hisppwiddiagpct) == FALSE) {

      if (sum(round(input$blackpwiddiagpct, 4), round(input$whitepwiddiagpct, 4), round(input$hisppwiddiagpct, 4)) > 100.0) {
      warning.text <- "The sum of PWID by race  must not be greater than 100%."
    }
    }

    return(warning.text)
    })

  ## Generate report -----------------------------------------------------------
  output$report <- downloadHandler(
    filename = paste0("Custom PrEP Indications Report ", Sys.Date(), ".docx"),
    content = function(file) {
      src <- normalizePath("report.Rmd")

      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, "report.Rmd")

      out <- render("report.Rmd", word_document())
      file.rename(out, file)
    })
})
