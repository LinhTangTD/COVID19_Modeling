 library(dplyr)
library(ggplot2)

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