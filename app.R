#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(tidyverse)
library(plotly)
library(data.table)
library(gganimate)
library(gifski)
library(shinyjs)
library(transformr)

basicSIR = function(population, initial.infected, spread.rate, recover.rate, days){
    day = 1
    susceptible = population - initial.infected
    daily_infected = initial.infected
    daily_recovered = 0
    data = tibble(Day = day,
                  Susceptible = susceptible,
                  Infected = daily_infected,
                  Recovered= daily_recovered,
                  Population = population)

    for (i in 2:days) {
        NewInfected = min(data$Susceptible[i-1], round(data$Infected[i-1] * data$Susceptible[i-1] * (spread.rate/population), 0))
        NewRecovered = round(data$Infected[i-1] * recover.rate, 0)
        data[i,] = list(i, data$Susceptible[i-1] - NewInfected, data$Infected[i-1] + NewInfected - NewRecovered, data$Recovered[i-1] + NewRecovered,
                        population)

    }

    return(data)
}

advancedSIR = function(population, initial.infected, spread.rate, recover.rate.H, recover.rate.sym, recover.rate.asym,
                       p.test.sym, p.test.asym, p.test.all, p.fp, p.tp.sym, p.tp.asym, day.q, incubation, hospitalization,
                       death.rate.H, death.rate.sym, death.rate.asym, days) {
    day = 1
    susceptible = population - initial.infected
    daily_infected = initial.infected
    daily_recovered = 0
    data = tibble(Day = day,
                  Susceptible = susceptible,
                  Infected = daily_infected,
                  Recovered= daily_recovered,
                  Population = population)
    
    
    for (i in 0:100) {
        
        if(i == 0){
            ### Initial setting (day 0)
            Isym.nv = 5 #initial infected symptomatic
            Iasym.nv = 0 #initial infected asymptomatic
            R.nv = N.nv * p.ns.nv #initial "recovered" i.e non-susceptible
            S.nv = N.nv - Isym.nv - Iasym.nv - R.nv
            FQ.nv = 0
            E.nv = 0
            D.nv = 0
            H.nv = 0
            Qsym.nv = 0
            Qasym.nv = 0
        } else {
            ### Current day calculations
            S.nv = S.nv + dS.nv
            FQ.nv = FQ.nv + dFQ.nv
            E.nv = E.nv + dE.nv
            Isym.nv = Isym.nv + dIsym.nv
            Iasym.nv = Iasym.nv + dIasym.nv
            Qsym.nv = Qsym.nv + dQsym.nv
            Qasym.nv = Qasym.nv + dQasym.nv
            H.nv = H.nv + dH.nv
            R.nv = R.nv + dR.nv
            D.nv = D.nv + dD.nv
        }
        
        Pop.nv = S.nv + FQ.nv + E.nv + Isym.nv + Iasym.nv + Qsym.nv + Qasym.nv + H.nv + R.nv + D.nv
        df.nv[i+1,] = list(i, Pop.nv, S.nv, FQ.nv, E.nv, Isym.nv, Iasym.nv, Qsym.nv, Qasym.nv, H.nv, R.nv, D.nv)
        # for debugging only
        # , new.exposed.nv, quarantine.released.today.nv, new.quarantined.nv, new.infected.all.nv, new.infected.sym.nv, confirmed.sym.nv, nonconfirmed.sym.nv, new.infected.asym.nv, confirmed.asym.nv, nonconfirmed.asym.nv, nonconfirmed.sym.to.H.nv, nonconfirmed.sym.to.R.nv, nonconfirmed.sym.to.D.nv, nonconfirmed.asym.to.R.nv, nonconfirmed.asym.to.D.nv, Qsym.to.H.nv, Qsym.to.R.nv, Qsym.to.D.nv, Qasym.to.R.nv, Qasym.to.D.nv, H.to.R.nv, H.to.D.nv)
        
        ### Immediate calculations (for next day statistics)
        new.exposed.nv = (S.nv/N) * (I.rate.sym.nv * Isym.nv + I.rate.asym.nv * Iasym.nv)
        quarantine.released.today.nv = FQ.nv * 1/day.q
        new.quarantined.nv = S.nv * p.test.all.nv * p.fp.all.nv
        new.infected.all.nv = E.rate * E.nv #total infected
        # infected symptomatic
        new.infected.sym.nv = new.infected.all.nv * p.sym.nv
        confirmed.sym.nv = Isym.nv * p.test.sym.nv * p.tp.sym.nv #tested & true positive
        nonconfirmed.sym.nv = Isym.nv - confirmed.sym.nv #not-tested & false negative
        # infected asymptomatic
        new.infected.asym.nv = new.infected.all.nv - new.infected.sym.nv
        confirmed.asym.nv = Iasym.nv * p.test.asym.nv * p.tp.asym.nv
        nonconfirmed.asym.nv = Iasym.nv - confirmed.asym.nv
        # transition between compartments (I, Q, H, R, D)
        nonconfirmed.sym.to.H.nv = nonconfirmed.sym.nv * H.rate.nv
        nonconfirmed.sym.to.R.nv = nonconfirmed.sym.nv * R.rate.sym.nv
        nonconfirmed.sym.to.D.nv = nonconfirmed.sym.nv * D.rate.sym.nv
        nonconfirmed.asym.to.R.nv = nonconfirmed.asym.nv * R.rate.asym.nv
        Qsym.to.H.nv = Qsym.nv * H.rate.nv
        Qsym.to.R.nv = Qsym.nv * R.rate.sym.nv
        Qsym.to.D.nv = Qsym.nv * D.rate.sym.nv
        Qasym.to.R.nv = Qasym.nv * R.rate.asym.nv
        H.to.R.nv = H.nv * R.rate.h.nv
        H.to.D.nv = H.nv * D.rate.h.nv
        ### Calculate changes in each compartment
        dS.nv = - new.exposed.nv - new.quarantined.nv + quarantine.released.today.nv
        dFQ.nv = new.quarantined.nv - quarantine.released.today.nv
        dE.nv = new.exposed.nv - new.infected.all.nv
        dIsym.nv = new.infected.sym.nv - confirmed.sym.nv - nonconfirmed.sym.to.H.nv - nonconfirmed.sym.to.R.nv - nonconfirmed.sym.to.D.nv
        dIasym.nv = new.infected.asym.nv - confirmed.asym.nv - nonconfirmed.asym.to.R.nv
        dQsym.nv = confirmed.sym.nv - Qsym.to.H.nv - Qsym.to.R.nv - Qsym.to.D.nv
        dQasym.nv = confirmed.asym.nv - Qasym.to.R.nv
        dH.nv = nonconfirmed.sym.to.H.nv + Qsym.to.H.nv - H.to.D.nv  - H.to.R.nv
        dR.nv = H.to.R.nv + nonconfirmed.sym.to.R.nv + Qsym.to.R.nv + Qasym.to.R.nv +  nonconfirmed.asym.to.R.nv
        dD.nv = H.to.D.nv + nonconfirmed.sym.to.D.nv + Qsym.to.D.nv
    }
}
# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    tabsetPanel(
        tabPanel("Simple SIR Model", fluid = TRUE,
            sidebarLayout(
                sidebarPanel(
                    sliderInput("Transmission",
                                "Transmission Rate (Beta)",
                                min = 0.3,
                                max = 2,
                                value = 0.8),
                    sliderInput("Recovery",
                                "Recovery Rate (Gamma)",
                                min = 0.01,
                                max = 1,
                                value = 0.3),
                    sliderInput("Population",
                                "Initial Population",
                                min = 100,
                                max = 5000,
                                value = 500),
                    sliderInput("Initial_Infected",
                                "Initial Infected",
                                min = 1,
                                max = 10,
                                value = 5),
                    sliderInput("Days",
                                "Select the number of Days:",
                                min = 5,
                                max = 200,
                                value = 50),
                    checkboxInput("FullAnimation",
                                  "Check if you want to see the whole animation at once. Don't change the inputs after clicking this",
                                  value = T),
                    actionButton("make_a_step", "Skip one Day"),
                    actionButton("skip_5_days", "Skip 5 Days"),
                    actionButton("reset", "Reset")),
                mainPanel(
                    imageOutput("AnimationPlot"),
                    dataTableOutput("DataTable")
                )
            )
        ),
        tabPanel("Advanced SIR", fluid = TRUE,
            sidebarLayout(
                sidebarPanel(
                    sliderInput("Transmission",
                                "Transmission Rate (Beta)",
                                min = 0.3,
                                max = 3,
                                value = 0.8),
                    # sliderInput("Recovery",
                    #             "Recovery Rate (Gamma)",
                    #             min = 0.01,
                    #             max = 1,
                    #             value = 0.3),
                    # sliderInput("Population",
                    #             "Initial Population",
                    #             min = 100,
                    #             max = 5000,
                    #             value = 500),
                    # sliderInput("Initial_Infected",
                    #             "Initial Infected",
                    #             min = 1,
                    #             max = 10,
                    #             value = 5),
                    sliderInput("Symptomsrate",
                                "Symptomatic ratio",
                                min =  0.3,
                                max = 1.00,
                                value = 0.5),
                    sliderInput("Percent_Testingasym",
                                "Asymptomatic Testing Percentage",
                                min = 0,
                                max = 1.00,
                                value = 0.2),
                    sliderInput("TPP_asym",
                                "True postive for asymptomatic",
                                min = 0.8,
                                max = 1.00,
                                value = 0.9),
                    sliderInput("Percent_Testingsym",
                                "Symptomatic Testing Percentage",
                                min = 0,
                                max = 1.00,
                                value = 0.2),
                    sliderInput("TPP_sym",
                                "True postive for symptomatic",
                                min = 0.8,
                                max = 1.00,
                                value = 0.9),
                    sliderInput("Hospitalization",
                                "Hospitalization Rate",
                                min = 0.01,
                                max = .6,
                                value = 0.1),
                    sliderInput("FPP",
                                "False Positve Percentage",
                                min = 0,
                                max = 0.02,
                                value = 0.01),
                    sliderInput("Days",
                                "Select the number of Days:",
                                min = 5,
                                max = 200,
                                value = 50),
                ),
                mainPanel(
                    imageOutput("AnimationPlot2"),
                    dataTableOutput("DataTable2")
                )
            )
        )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    #    data <- reactive({
    #      Day = 0
    #      Susceptible = input$Population
    #      Infected = input$Initial_Infected
    #      Recovered = 0
    #      Population = input$Population
    #    data = data.frame(Day, Susceptible, Infected, Recovered, Population)
    #
    # })
    
    
    
    
    n_steps <- reactiveVal(value = 1)
    
    observeEvent(input$make_a_step,{
        new_steps <- n_steps() + 1
        n_steps(new_steps)
    })
    observeEvent(input$reset, {
        n_steps(0)
    })
    observeEvent(input$skip_5_days, {
        new_steps <- n_steps() + 5
        n_steps(new_steps)
    })
    
    sub_Fun <- function(data, steps) {
        validate(need(steps <= input$Days, "You've reached the last time point"))
        data[1:steps,]
    }
    
    output$DataTable <- renderDataTable({
        dataSet <- basicSIR(input$Population, input$Initial_Infected, input$Transmission, input$Recovery, input$Days)
        dataSet
        
    })
    
    
    output$AnimationPlot<- renderImage({
        if (input$FullAnimation == TRUE) {
            
            dataSet <- basicSIR(input$Population, input$Initial_Infected, input$Transmission, input$Recovery, input$Days)
            graph1 <- ggplot(dataSet, aes(x = Day)) +
                geom_line(aes(y = Susceptible, color = "Susceptible"), size = 1.3) +
                geom_line(aes(y = Infected, color = "Infected"), size = 1.3) +
                geom_line(aes(y = Recovered, color = "Recovered"), size = 1.3) +
                scale_color_discrete(name = "Compartment") +
                labs(y = "Population", title = "SIR Model Animation") +
                theme(legend.position = "bottom",
                      plot.background = element_rect(fill = "#FFFF99"),
                      panel.background = element_rect(fill = NA),
                      panel.grid.major = element_line(colour = "black"),
                      panel.grid.minor = element_blank()) +
                geom_point(aes(y = Susceptible)) +
                geom_point(aes(y = Infected)) +
                geom_point(aes(y = Recovered))
            
            outfile <- tempfile(fileext='.gif')
            graph1.animation <- graph1 +
                transition_reveal(Day)
            anim_save("outfile.gif", animate(graph1.animation, height = 400, width = 600, fps = 8, duration = 10, end_pause = 60, res = 100))
            list(src = "outfile.gif",
                 contentType = 'image/gif'
                 # width = 400,
                 # height = 300,
                 # alt = "This is alternate text"
            )
        }
        else {
            dataSet <- basicSIR(input$Population, input$Initial_Infected, input$Transmission, input$Recovery, input$Days)
            graph1 <- ggplot(sub_Fun(dataSet, n_steps()), aes(x = Day)) +
                geom_line(aes(y = Susceptible, color = "Susceptible"), size = 1.3) +
                geom_line(aes(y = Infected, color = "Infected"), size = 1.3) +
                geom_line(aes(y = Recovered, color = "Recovered"), size = 1.3) +
                scale_color_discrete(name = "Compartment") +
                labs(y = "Population", title = "SIR Model Animation") +
                theme(legend.position = "bottom",
                      plot.background = element_rect(fill = "#FFFF99"),
                      panel.background = element_rect(fill = NA),
                      panel.grid.major = element_line(colour = "black"),
                      panel.grid.minor = element_blank()) +
                # geom_point(aes(y = Susceptible)) +
                # geom_point(aes(y = Infected)) +
                # geom_point(aes(y = Recovered)) +
                scale_x_continuous(limits = c(1, input$Days))
            ggsave("outfile.png", graph1, device = "png", dpi = 100, width = 6, height = 4, units = "in")
            list(src = "outfile.png",
                 countentType = 'image/png')
        }
        
    }, deleteFile = TRUE)
    
}
# Run the application
shinyApp(ui = ui, server = server)
