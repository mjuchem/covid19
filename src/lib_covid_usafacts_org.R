#
# Gets COVID-19 datasets from USAFacts
# https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/
#
# Author: Murilo Juchem
#

library(plyr)
library(tidyverse)
library(lubridate)
library(reshape2)
library(gdata)

load_covid_from_usafacts_by_state = function() {

  covid.confirmed <- read_csv("https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_confirmed_usafacts.csv") %>%
    melt(
      id.vars = c("countyFIPS", "County Name", "State", "StateFIPS"),
      variable.name = "Date",
      value.name = "COVID_confirmed"
    ) %>%
    mutate(
      Date = ymd(Date)
    )
  
  covid.deaths <- read_csv("https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_deaths_usafacts.csv") %>%
    melt(
      id.vars = c("countyFIPS", "County Name", "State", "StateFIPS"),
      variable.name = "Date",
      value.name = "COVID_deaths"
    ) %>%
    mutate(
      Date = ymd(Date)
    )
  
  county.population <- read_csv("https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_county_population_usafacts.csv")
  
  # local cache
  write.csv(covid.confirmed, "./data/USAFacts_COVID_Confirmed.csv")
  write.csv(covid.deaths, "./data/USAFacts_COVID_Deaths.csv")
  write.csv(county.population, "./data/USAFacts_County_Population.csv")
  
  t <- merge(
    merge(
      covid.confirmed %>%
        group_by(State, Date) %>%
        summarize(COVID_confirmed = sum(COVID_confirmed)),
      covid.deaths %>%
        group_by(State, Date) %>%
        summarize(COVID_deaths = sum(COVID_deaths)),
      by = c("State", "Date"), all.x = TRUE
    ),
    county.population %>%
      group_by(State) %>%
      summarize(population = sum(population)),
    by = "State"
  )
  
  return(t)
    
}
