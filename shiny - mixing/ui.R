library("shiny")
library("shinydashboard")
library("deSolve")
suppressMessages(library("EpiModel"))
source("fx.R")
library("plotly")

# Define UI for application that draws a histogram
shinyUI(dashboardPage(
    
    dashboardHeader(),
    dashboardSidebar(
        width = 200,
        sidebarMenu(
            menuItem("Introduction", tabName = "Introduction", icon = icon("book")),
            menuItem("Model", tabName = "Model", icon = icon("line-chart")),
            menuItem("Context", tabName = "Context", icon = icon("book")),
            menuItem("Model Structure", tabName = "ModelStructure", icon = icon("book")),
            menuItem("Detailed Instructions", tabName = "DetailedInstructions", icon = icon("book")),
            menuItem("Discussion", tabName = "Discussion", icon = icon("book")),
            menuItem("Equations", tabName = "Equations", icon = icon("cog")),
            menuItem("License and Attribution", tabName = "License", icon = icon("gavel"))
        )
    ),
    
    dashboardBody(
        tabItems(
            
            
            # Intro Page --------------------------------------------------------------
            
            tabItem(tabName = "Introduction",
                    
                    fluidRow(
                        column(width = 1),
                        column(width = 10,
                               h3("Impact of Assortative Mixing on HIV Prevalence and Incidence among MSM",
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
                
                tabName = "Context",
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
            
            tabItem(
                
                tabName = "ModelStructure", 
                fluidRow(
                    column(width = 1),
                    column(width = 10,
                           h3("Model Structure", style = "color: #2E619E;", align = "Center"),
                           hr(),
                           
                           p(strong("Model inputs overview."), "The model contains:"),
                           
                           tags$ul(
                               
                               tags$li("four initial conditions (upper left panel)"),
                               
                               tags$li("two control parameters (lower left panel)"),
                               
                               tags$li("three parameters controlling contacts (central panel)"),
                               
                               tags$li("two parameters controlling transmission (central panel)"),
                               
                               tags$li("five controlling demographics (right panel)")
                               
                               ), #end tags$ul
                           
                           p(strong("Technical"), "(for those familiar with modeling technology; others may skip ahead):"),
                           
                           tags$ul(
                               
                               tags$li("The model is structured as a compartmental model using ordinary differential equations."),
                               
                               tags$li("It is an SI model with no recovery and an open population."),
                               
                               tags$li("Although this is not an individual-based model, we frame the description below using concepts 
                                       such as individual actors and individual contacts for the sake of clarity and intuitiveness for a general audience.")
                               
                           ), #end tags$ul
                           
                           p(strong("Intuitive"), "(for everyone):"),
                           
                           tags$ol(
                               
                               tags$li("Types of people"),
                                
                                    tags$ul(        
                                        tags$li("The model simulates a population comprising two groups, named Group 1 and Group 2."),
                               
                                        tags$li("In addition to their group identity, individuals in the population are also distinguished 
                                                by infection status: they are either susceptible or infected. (There is no recovery)."),
                               
                                        tags$li("Thus, everyone in the population falls into one of four categories at any point in time, 
                                                based on their two attributes (group identity, infection status).")
                                    ),# end tags$ol
                               
                               tags$li("Initial conditions: this section of the app determine the size of the four categories of people at 
                                       the beginning of the simulation"),
        
                               tags$li("Arrivals and departures"),
                                
                                    tags$ul(
                           
                                        tags$li("Over time, people enter the population (“birth”) and leave the population (“death”). 
                                                One can also think of these are in- and out-migration if one wishes."),
                                        
                                        tags$li("These are controlled by parameters in the Demographics section of the app."),
                           
                                        tags$li("There is one overall birth rate, and a separately controllable death rate for each of the four categories."),
                               
                                        tags$li("New arrivals are all uninfected, and are split between the two groups according to the current sizes of these groups.")
                                        
                                    ),# end tags$ol
                               
                               tags$li("Contacts"),
                               
                                    tags$ul(
                                        
                                        tags$li("Members of the population come into contact with one another. Since this is a simplified HIV model, contact represents a sexual act."),
                               
                                        tags$li("This process of forming contacts is shaped by three parameters:"),
                                        
                                            tags$ul(
                               
                                                    tags$li("one determining the Group 1 rate, which determines how many contacts people in Group 1 have overall, regardless of who the contacts are with"),
                               
                                                    tags$li("one determining the Group 2 rate, which determines how many contacts people in Group 2 have overall, regardless of who the contacts are with"),
                               
                                                    tags$li("a mixing statistic, which determines the level of assortative mixing between the two groups. When this statistic is at its minimum value of 0, 
                                                            the two groups mix randomly – that is, although the pool of people having contacts may differ in size for each group (depending on group sizes 
                                                            and group-specific contact rates), once people are in the pool they pair up with others in the pool without considering group. At the other extreme, 
                                                            when this statistic is at its minimum value of 1, the groups are completely assortative – all contacts are within-group.")
                                                    
                                            ) #end tags$ol
                                        
                                    ), # end tags$ol
                               
                               tags$li("Transmission"),
                               
                                    tags$ul(
                                        tags$li("If a contact involves a discordant pair (one susceptible, one infected), then a transmission may occur."),
                               
                                        tags$li("The probability of transmission depends on the group membership of the infected partner."),
                               
                                        tags$li("If  transmission does occur, the susceptible partner leaves the susceptible population and joins the infected population.")

                                    ) # end tags$ol
                           ) # end tags$ol
                           
   ), #end column
                    column(width = 1)
                           ) # End fluidRow
                           ), # End tabItem
            
            tabItem(
                
                tabName = "DetailedInstructions",
                fluidRow(
                    column(width = 1),
                    column(width = 10,
                           h3("Detailed Instructions", style = "color: #2E619E;", align = "Center"),
                           hr(),
                           p("Users can explore the model in different ways, but here we present a method that highlights the behavior we originally developed the tool to show."),
                           
                           
                           tags$ol(
                               
                               tags$li(strong("Default parameters – equal groups, no disparity, no assortative mixing")),
                           
                                       tags$ul(
                                           
                                           tags$li("Begin by examining the default parameter values and the resulting epidemic."),
                                           
                                           tags$li("In  this case, everything about the two groups is exactly the same (e.g. group size, 
                                                   initial prevalence, death rates, transmission rates)."),
                                           
                                           tags$li("Not surprisingly, then, the epidemic trajectory that each group takes over time is 
                                                   exactly the same: they both head to an equilibrium value of 70% prevalence.")
                                           
                                       ), #end tags$ul
                               
                               tags$li(strong("Adding assortative mixing")),
                              
                                       tags$ul(
                                           
                                           tags$li("Try changing the mixing statistic to a higher value, i.e. making a higher proportion 
                                                   of the contacts occur between two people in the same group."),
                                           
                                           tags$li("No matter what value you change this to, the trajectories remain the same, since both 
                                                   groups have the exact same behaviors, demography and transmission rates.")
                                           
                                       ), #end tags$ul
                           
                               
                               tags$li(strong("Adding an initial disparity")),
                           
                                       tags$ul(
                                           
                                           tags$li("Now, change the initial prevalence levels for one or both groups; for now, use any 
                                                   allowed value except for 0."),
                                           
                                           tags$li("This represents a pre-existing disparity resulting from some previous unmeasured cause."),
                                           
                                           tags$li(strong(em("Note that the model still returns to equal disease burden"))),
                                           
                                           tags$li("Incidence begins to converge noticeably right away, and prevalence eventually becomes equal 
                                                   for the two groups.")
                                           
                                       ), #end tags$ul
                               
                               tags$li(strong("Modeling complete assortative mixing")),
                           
                                       tags$ul(
                                           
                                           tags$li("Try ramping assortative mixing all the way up to 1 – that is, the two groups are fully assortative and 
                                                   thus completely isolated from one another."),
                                           
                                           tags$li("Even though they start at different levels of prevalence, they still converge."),
                                           
                                           tags$li(strong(em("In the presence of a pre-existing disparity, assortative mixing alone, no matter how strong, 
                                                             has no ability to maintain that disparity."))),
                                           
                                           tags$li("Although it may take a long time to fully achieve equilibrium, the incidence values in particular 
                                                   typically begin to move noticeably in the direction of their equilibrium quite quickly.")
                                           
                                       ), #end tags$ul
                               
                               tags$li(strong("Introducing a clinical difference between groups")),
                           
                                       tags$ul(
                                           
                                           tags$li("Return all model parameters to their default values. You 
                                                   can do this by reloading the page in your web browser."),
                                           
                                           tags$li("Now, double the transmission rate for Group 2 so that it equals 0.10. This represents the 
                                                   probability that an infected person in Group 2 will transmit to their contact."),
            
                                           tags$li("For HIV, one might imagine this to represent a case where persons in Group 2 are less 
                                                   likely to be virally suppressed."),
                                           
                                           tags$li("Observe and record the prevalence level for each group that this model heads towards."),
                                           
                                           tags$li("You will notice there is still no difference by group; although Group 2 transmits more easily, 
                                                   the lack of assortative mixing means that their partners are equally likely to be from either group.")
                                           
                                       ), #end tags$ul

                               tags$li(strong("Adding assortative mixing on top of a clinic difference")),
                           
                                       tags$ul(
                                           
                                           tags$li("Now try adding in assortative mixing, and see what happens. Note these equilibrium 
                                                   prevalence and incidence values."),
                                           
                                           tags$li("You should see a sustained disparity with Group 2 having higher prevalence.")
                                           
                                       ), #end tags$ul
 
                               tags$li(strong("Adding back in an initial disparity")),
                           
                                       tags$ul(
                                           
                                           tags$li("Now, keeping everything else the same, once again change the initial prevalence 
                                                   levels for one or both groups."),
                                           
                                           tags$li("Notice that the prevalence and incidence values that the two models head towards 
                                                   do not change."),
                                           
                                           tags$li("The trajectories that the populations take to get there have changed because 
                                                   the starting points have changed."),
                                           
                                           tags$li("And once again the incidence values typically begin to move in the direction 
                                                   of their new equilibrium right away."),
                                           
                                           tags$li("Now try changing the mixing statistic again and see that there is a new 
                                                   equilibrium."),
                                           
                                           tags$li("And again try changing the initial prevalence levels (to anything except 0) 
                                                   and see that this does not change the equilibrium.")
                                           
                                       ), #end tags$ul

                               tags$li(strong("The irrelevance of initial conditions – the exceptions")),
                               
                                           tags$ul(
                                               
                                               tags$li(strong(em("Up until now, we have seen, under a range of scenarios, that the 
                                                                 initial infection prevalence within each group has no effect on the 
                                                                 size of the disparity that is maintained in the long-term."))),
                                               
                                               tags$li("Now, finally try changing one or more initial prevalence values to 0 and explore 
                                                       different scenarios."),
                                               
                                               tags$li("You will see that there are only two exceptions to the rule that a given set of 
                                                       parameters will always head to the same pair of group-specific prevalence values 
                                                       regardless of where prevalence begins."),
                
                                                            tags$li("One is when both groups are given an initial prevalence of 0 – in 
                                                                   this case, the epidemic can never take off at all since in our model 
                                                                   everyone who enters the population after the initial setup is uninfected."),
                                               
                                                            tags$li("The other is when one group has an initial prevalence of 0 and the mixing 
                                                                   parameter is at 1. Here the two groups are completely isolated, and the 
                                                                   group with no infection can never see an epidemic."),
                
                                                            tags$li("Of course, in reality, when two groups co-exist within the same larger 
                                                                    community, they are rarely 100% percent assortative without exception.")
                               
                           ) #end tags$ul
                           
                           ) #end tags$ol
                           
                           ) # End column
                           ) # End fluidRow
                           ), # End tabItem
            
            tabItem(
                
                tabName = "Discussion",
                
                fluidRow(
                    column(width = 1.0),
                    column(width = 10,
                           h3("Discussion", style = "color: #2E619E;", align = "Center"),
                           hr(),
                           p(strong("The limited power of assortative mixing alone")),
                           
                                        p("We have used a relatively simple model of an infectious disease with two groups to help people build intuition around a seemingly counterintuitive fact: that,", 
                                          strong("in the presence of a pre-existing disparity"), "in infection burden between two groups,", 
                                          strong("assortative mixing between the groups on its own cannot sustain that disparity in the long term."), 
                                          "Although initial incidence will be different between the two groups, these values begin to converge immediately, and both incidence and prevalence 
                                          will eventually converge across the two groups completely."),
                           
                           p(strong("Assortative mixing in conjunction with other factors")),
                           
                                        p("Differences between groups such as infectiousness (perhaps because of different levels of viral suppression), or contact rates, or mortality (also because of 
                                          different levels of viral suppression) can create sustainable disparities. And assortative mixing can accentuate the resulting disparity in these cases. But 
                                          even here, the ultimate size of the disparity does not depend on what level of disparity one begins the model at. The only exception is the very extreme case 
                                          involving initial prevalence rates of 0."),
                           
                           p(strong("History and theory – a recap")),
                           
                                       p("The principle that these tools demonstrate is not new. Indeed, it was proven mathematically by Simon and Jacquez (1992) for the case of two groups, for certain types of 
                                         diseases that share many features of HIV. In doing this work, they were building on a deep literature on the mathematical theory of epidemics. However, their proof, and 
                                         many other ones throughout that literature, involve highly complex mathematics that make them inaccessible to many practitioners. Other applied modeling work has shown 
                                         this behavior in specific circumstances (Morris et al 2009). Here we sought to demonstrate the principle for a broad audience using a user-friendly tool."),
                           
                           p(strong("Caveat 1: the slowness of equilibria")),
                           
                                       p("We provide two caveats. One is that in many cases it may take a long time to fully achieve equilibrium (even on the order of 100 years, far longer than the HIV epidemic has existed). 
                                         Readers who want to understand how existing disparities are being maintained might argue that, since we have only so far observed those disparities over a few decades, that it may still 
                                         be assortative mixing on its own that drives most of the continuing disparity that we see. However, even though prevalence in models may take a long time to fully converge, the measurable 
                                         convergence of incidence and prevalence levels between groups in the direction of that equilibrium occurs immediately. Thus, if past unmeasured factors generated a racial disparity in HIV 
                                         between Black and White MSM, and current measured factors reflect any kind of change from those past unmeasured ones, we should expect to see HIV incidence and prevalence moving in the 
                                         direction sustainable by the new factors rapidly. In practice, however, this does not appear to be happening."),
                           
                           p(strong("Caveat 2: the real world is dynamic and heterogeneous")),
                           
                                       p("Our second caveat is that, in the real world, the values behind the various parameters we consider are changing all the time. And, related to this, each parameter represents the complex outcome of 
                                         multiple phenomena, averaged across a very heterogeneous group of people. In other words, our model is a simplification of the real world. This is on purpose, to help gain intuition. None of this 
                                         negates the central insight, that assortative mixing on its own cannot sustain a pre-existing disparity."),
                           
                           p(strong("Where did my intuition go wrong?")),
                           
                                       p("Users may be asking themselves this question. To repeat one version of that intuition: if Black MSM currently have higher HIV prevalence than White MSM, and there is high assortative mixing, 
                                         then any individual HIV-negative Black MSM is more likely to have a positive partner than any individual HIV-negative White MSM. If the two groups are identical in every other way, then that 
                                         individual HIV-negative Black MSM should be more likely to become infected than his White counterpart. All of this is indeed true. However, where the intuition breaks down is in the next step, 
                                         where people assume that this means higher numbers of new infections for Black MSM indefinitely. This is because, in order to calculate new incident cases, the probability that any individual 
                                         HIV-negative person in a group becomes infected must be multiplied by the number of HIV-negative persons in that group. And, given the higher prevalence among Black MSM, that number is lower 
                                         for them than for White MSM. Incidence is a complex interaction of both the size of the HIV-positive population and the size of the HIV-negative population, and those two numbers must by 
                                         definition add up to 100% of the population for any group. Our intuition tends to focus on the size of only one group or the other."),
                           
                           h4("Literature Cited"),
                           tags$ul(
                           
                               tags$li("Morris M, Kurth AE, Hamilton DT, Moody J, Wakefield S (2009). Concurrent Partnerships and HIV Prevalence Disparities by Race: 
                                       Linking Science and Public Health Practice.", em("American Journal of Public Health,"), "99(6) pp. 1023-1031.",
                                       tags$a("Paper Link", target = "_blank", href = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2679771/")),
                               
                               tags$li("Simon CP, Jacquez JA (1992). Reproduction Numbers and the Stability of Equilibria of SI Models for Heterogeneous Populations.",
                                       em("SIAM Journal on Applied Mathematics,"), "52(2), 541-576.",
                                       tags$a("Paper Link", target = "_blank", href = "https://www.jstor.org/stable/2102425"))
                               
                                   ) # end tags$ul
                           
                           ), # End column

                    column(width = 1.0)
                    
                    ) #End fluidRow
                    ), #End tabItem
            
            tabItem(
                tabName = "Equations",
                fluidRow(
                    column(width = 1),
                    column(width = 10,
                           h3("Equations", style = "color: #2E619E;", align = "Center"),
                           hr(),
                           p("These equations below are the differential equations representing the flow of individuals in each of the four compartments:"),
                           tags$ul(
                               tags$li("The number of susceptible individuals from group 1"),
                               tags$li("The number of infected individuals from group 1"),
                               tags$li("The number of susceptible individuals from group 2"),
                               tags$li("The number of infected individuals from group 2")
                           ),
                           
                           withMathJax(),
                           tags$div(HTML("")),
                           helpText('$$\\frac{dS_{1}}{dt} = \\nu N_{1} - \\lambda_{1} S_{1} - \\mu_{S_{1}} S_{1}$$'),
                           helpText('$$\\frac{dI_{1}}{dt} = \\lambda_{1} S_{1} - \\mu_{I_{1}} I_{1}$$'),
                           helpText('$$\\frac{dS_{2}}{dt} = \\nu N_{2} - \\lambda_{2} S_{2} - \\mu_{S_{2}} S_{2}$$'),
                           helpText('$$\\frac{dI_{2}}{dt} = \\lambda_{2} S_{2} - \\mu_{I_{2}} I_{2}$$'),
                           helpText('$$N_{1} = S_{1} + I_{1}$$'),
                           helpText('$$N_{2} = S_{2} + I_{2}$$'),
                           helpText('\\(I_{j}\\) is number of people of state \\(I\\) from group \\(j\\)'), 
                           helpText('\\(\\nu\\) is the birth rate'), 
                           helpText('\\(\\lambda_{j}\\) is the force of infection for people from group \\(j\\)'),
                           helpText('\\(\\mu_{ij}\\) is the death rate for people of state \\(I\\) from group \\(j\\)'), 
                           helpText('\\(N_{i}\\) is the number of people in group \\(j\\)')
                    ), #End column
                    column(width = 1.0)
                ) #End fluidRow
            ), #
            # Model Scenarios Page --------------------------------------------------------------
            tabItem(
                tabName = "Model",
                
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
                        ), # end tabItem
            tabItem(tabName = "License", 
                    fluidRow(
                        column(width = 1),
                        column(width = 10,
                               h3("License and Attribution", style = "color: #2E619E;", align = "Center"),
                               hr(),
                               
                               h4("LICENSE"),
                               tags$blockquote("Copyright 2017, Kevin M. Weiss, Maraia R. Tremarelli, Samuel M. Jenness, and Steven M. Goodreau. 
                                               This website and source code are distributed under the terms of the", 
                                               tags$a("GPL-3", target = "_blank", href = "https://www.gnu.org/licenses/gpl-3.0.en.html"), 
                                               "license."),
                               hr(),
                               
                               h4("CITATION"),
                               tags$blockquote("Weiss KM, Tremarelli MR, Jenness SM, Goodreau SM 2017. Impact of Assortative Mixing on 
                                               HIV Prevalence and Incidence among MSM: A Web-Based Modeling Tool for Public Health Practice.", 
                                               tags$a("https://prism.shinyapps.io/mixing/", target = "_blank", href = "https://prism.shinyapps.io/mixing/")),
                               hr(),
                               
                               h4("ORIGINATING ARTICLE"),
                               tags$blockquote("Goodreau SM, Rosenberg ES, Jenness SM, Luisi N, Stansfield SE, Millett GA, Sullivan PS. Isolating the sources of 
                                               racial disparities in HIV prevalence among men who have sex with men (MSM) in Atlanta, GA: A modeling study.",
                                               em("The Lancet HIV."))
                                                              
                        ), # end column
                        column(width = 1)
                        ) # end fluidRow
                    
                        ) # end tabItem
                ) # end tabItems
            ) # end dashboardBody
            )
                           )


