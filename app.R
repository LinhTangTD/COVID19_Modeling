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

basicSIR = function(population, initial.infected, spread.rate, recover.rate, days){
    day = 1
    susceptible = population
    daily_infected = initial.infected
    daily_recovered = 0
    data = data.frame(Day = day,
                    Susceptible = susceptible,
                    Infected = daily_infected,
                    Recovered= daily_recovered,
                    Population = population)

    for (i in 2:days) {
        NewInfected = round(data$Infected[i-1] * data$Susceptible[i-1] * (spread.rate/population), 0)
        NewRecovered = round(data$Infected[i-1] * recover.rate, 0)
        data[i,] = list(i, data$Susceptible[i-1] - NewInfected, data$Infected[i-1] + NewInfected - NewRecovered, data$Recovered[i-1] + NewRecovered,
                        population)

    }

    return(data)
}
Sir.Plot2 = function(population, initial.infected, spread.rate, recover.rate, days){
    day = 1
    susceptible = population
    daily_infected = initial.infected
    daily_recovered = 0
    data = data.frame(Day = day,
                      Susceptible = susceptible,
                      Infected = daily_infected,
                      Recovered= daily_recovered,
                      Population = population)

    for (i in 2:days) {
        NewInfected = round(data$Infected[i-1] * data$Susceptible[i-1] * (spread.rate/population), 0)
        NewRecovered = round(data$Infected[i-1] * recover.rate, 0)
        data[i,] = list(i, data$Susceptible[i-1] - NewInfected, data$Infected[i-1] + NewInfected - NewRecovered, data$Recovered[i-1] + NewRecovered,
                        population)

        SIR.Plot(data)

    }


}

SIR.Plot = function(model){
    g <- ggplot(data  = model, aes(x = Day)) +
        geom_line(aes(y = Susceptible, color = "Susceptible")) +
        geom_line(aes(y = Infected, color = "Infected")) +
        geom_line(aes(y = Recovered, color = "Recovered")) +
        scale_color_discrete(name = "Compartment") +
        labs(y = "Population", title = "SIR Model") +
        theme_minimal()
    ggplotly(g)
}
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Simple SIR Model Simulation - Senay, Bowen, Britney, and Linh"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("Transmission",
                        "Transmission Rate (Beta)",
                        min = 0.5,
                        max = 3,
                        value = 1),
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
                        value = 100),
            submitButton("Animate", icon("refresh"))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotlyOutput("GraphPlot"),
           dataTableOutput("DataTable")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    plotData <- reactive ({
        data <- basicSIR(input$Population, input$Initial_Infected, input$Transmission, input$Recovery, input$Days)

    })
    output$DataTable <- renderDataTable({
        data <- plotData()
        data
    })

    output$GraphPlot<- renderPlotly({
        data <- plotData()
        SIR.Plot(data)

    })
}

# Run the application
shinyApp(ui = ui, server = server)
