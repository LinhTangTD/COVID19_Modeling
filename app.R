

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
# Define UI for application that draws a histogram
ui <- fluidPage(
  
    # Application title
    titlePanel("Simple SIR Model Simulation - Senay, Bowen, Britney, and Linh"),

    # Sidebar with a slider input for number of bins
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
                          "Check if you want to see the whole animation at once. Don't change the inputs after clicking this"),
            actionButton("make_a_step", "Skip one Day"),
            actionButton("skip_5_days", "Skip 5 Days"),
            actionButton("reset", "Reset")),
            #submitButton("Update Graph", icon("refresh"))),



        # Show a plot of the generated distribution
        mainPanel(
            imageOutput("AnimationPlot"),
            dataTableOutput("DataTable")
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
        transition_reveal(Day) +
        view_follow(fixed_y = TRUE)
      anim_save("outfile.gif", animate(graph1.animation, height = 400, width = 600, fps = 12, duration = 8, end_pause = 60, res = 100))
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
         geom_point(aes(y = Susceptible)) +
         geom_point(aes(y = Infected)) +
         geom_point(aes(y = Recovered))
      ggsave("outfile.png", graph1, device = "png", dpi = 100, width = 6, height = 4, units = "in")
      list(src = "outfile.png",
           countentType = 'image/png')
    }

  }, deleteFile = TRUE)

}
# Run the application
shinyApp(ui = ui, server = server)
