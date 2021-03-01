library(shiny)
library(dplyr)
library(ggplot2)
library(shinyjs)
library(plotly)
library(DT)

basicSIR = function(population, suscept.rate, initial.infected, spread.rate, recover.rate){
    day = 0
    healthy = population - initial.infected
    susceptible = healthy * suscept.rate
    daily_infected = initial.infected
    daily_recovered = 0
    current_infected = initial.infected
    cum_infected = initial.infected
    cum_recovered = 0
    # Table storing S, I, R compartments over time
    df = data_frame(Day = day, 
                    Healthy = healthy, 
                    Susceptible = susceptible, 
                    DailyInfected = daily_infected,
                    DailyRecovered = daily_recovered,
                    CurrentInfected = current_infected,
                    TotalInfected = cum_infected,
                    TotalRecovered = cum_recovered)
    while(daily_infected >= 1){
        day = day + 1
        daily_infected = current_infected * susceptible * spread.rate
        healthy = healthy - daily_infected
        susceptible = healthy * suscept.rate
        daily_recovered = recover.rate * current_infected
        current_infected = current_infected + daily_infected - daily_recovered
        cum_infected = cum_infected + daily_infected
        cum_recovered = daily_recovered + cum_recovered
        data = c(day, healthy, susceptible, daily_infected, daily_recovered, current_infected, cum_infected, cum_recovered)
        df = rbind(df, data)
    }
    return(df)
}

SIR.plot = function(model){
    ggplot(data  = model, aes(x = Day)) +
        geom_line(aes(y = Healthy, color = "Healthy")) +
        geom_line(aes(y = DailyInfected, color = "Daily Infected")) +
        geom_line(aes(y = DailyRecovered, color = "Daily Recovered")) +
        geom_line(aes(y = TotalInfected, color = "Cumulative Infected")) +
        geom_line(aes(y = TotalRecovered, color = "Cumulative Recovered")) +
        scale_color_discrete(name = "Compartment") +
        labs(y = "Population", title = "SIR Model") + 
        theme_minimal()
}

#Test
a = basicSIR(500, 0.2, 5, 0.01, 0.7)
SIR.plot(a)

ui <- fluidPage(
    
    # Application title
    titlePanel("COVID-19 Modeling with SIR"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("Pop",
                        "Population",
                        min = 0,
                        max = 10000,
                        step = 100,
                        value = 500),
            sliderInput("InitialInfected",
                        "Initially Infected Individuals",
                        min = 1,
                        max = 20,
                        step = 1, 
                        value = 5),
            sliderInput("SusceptibleRate",
                        "Rate of population susceptible to getting disease",
                        min = 0,
                        max = 1,
                        step = 0.05,
                        value = 0.2),
            sliderInput("RecoveryRate",
                        "Recovery Rate",
                        min = 0,
                        max = 1,
                        step = 0.01,
                        value = 0.7),
            sliderInput("InfectionRate",
                        "Infection Rate", 
                        min = 0.01, 
                        max = 0.3, 
                        step = 0.01,
                        value = 0.01)
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("Plot", 
                         plotOutput('Plot')),
                tabPanel("Data",
                         dataTableOutput("df"))
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$Plot <- renderPlot({
        df = basicSIR(population  = input$Pop,
                      suscept.rate = input$SusceptibleRate, 
                      initial.infected = input$InitialInfected,
                      spread.rate = input$InfectionRate, 
                      recover.rate = input$RecoveryRate)
        output$df = renderDataTable({datatable(df) %>% formatRound(colnames(df), digits = 0)}, options = list(scrollX = TRUE))
        plot = SIR.plot(df)
        return(plot)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)