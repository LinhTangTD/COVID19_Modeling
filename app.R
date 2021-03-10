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

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Simple SIR Model Simulation"),

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
                        max = 1000,
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
           plotOutput("GraphPlot"),
           tableOutput("DataTable")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {


    output$DataTable <- renderTable({
        data = data.frame(Day = numeric(),
                          Susceptible = numeric(),
                          Infected = numeric(),
                          Recovered = numeric(),
                          Population = numeric())
        data[1,] = c(1, input$Population - input$Initial_Infected, input$Initial_Infected, 0, input$Population)

        for (i in 2:input$Days) {
            NewInfected = round(data$Infected[i-1] * data$Susceptible[i-1] * (input$Transmission/input$Population), 0)
            NewRecovered = round(data$Infected[i-1] * input$Recovery, 0)
            data[i,] = list(i, data$Susceptible[i-1] - NewInfected, data$Infected[i-1] + NewInfected - NewRecovered, data$Recovered[i-1] + NewRecovered,
                            input$Population)

        }
        data <- arrange(data, desc(Day))
        data
    })

    output$GraphPlot<- renderPlot({
        data = data.frame(Day = numeric(),
                          Susceptible = numeric(),
                          Infected = numeric(),
                          Recovered = numeric(),
                          Population = numeric())
        data[1,] = c(1, input$Population - input$Initial_Infected, input$Initial_Infected, 0, input$Population)

        for (i in 2:input$Days) {
            NewInfected = round(data$Infected[i-1] * data$Susceptible[i-1] * (input$Transmission/input$Population), 0)
            NewRecovered = round(data$Infected[i-1] * input$Recovery, 0)
            data[i,] = list(i, data$Susceptible[i-1] - NewInfected, data$Infected[i-1] + NewInfected - NewRecovered, data$Recovered[i-1] + NewRecovered,
                            input$Population)

        }

        ggplot(data, aes(x = Day)) +
            geom_line(aes(y = Susceptible), color = "blue", size = 1) +
            geom_line(aes(y = Infected), color = "red", size = 1) +
            geom_line(aes(y = Recovered), color = "green", size = 1)

    })
}

# Run the application
shinyApp(ui = ui, server = server)
