#
# Gets COVID-19 datasets from covidtracking.com
# https://covidtracking.com/data
#
# Author: Murilo Juchem
#

library(tidyverse)
library(lubridate)

load_covid_states_from_covidtracking <- function() {
  
  url_states <- "https://covidtracking.com/api/v1/states/daily.csv"
  
  t <- read.csv(url_states) %>%
    mutate(date = ymd(date))
  
  t$totalTestResults[is.na(t$totalTestResults)] <- 0
  t$positive[is.na(t$positive)] <- 0
  t$negative[is.na(t$negative)] <- 0
  t$death[is.na(t$death)] <- 0
  
  t <- mutate(t,
              tested2positive = positive / totalTestResults,
              positive2death = death / positive,
              tested2death = death / totalTestResults
  )

  t$tested2positive[is.na(t$tested2positive)] <- 0
  t$positive2death[is.na(t$positive2death)] <- 0
  t$tested2death[is.na(t$tested2death)] <- 0
  
  # local cache
  write.csv(t, "./data/covidtracking_states_daily.csv")
  
  return(t)
  
}

load_covid_us_from_covidtracking <- function() {
  
  url_us <- "https://covidtracking.com/api/v1/us/daily.csv"
  
  t <- read.csv(url_us) %>%
    mutate(date = ymd(date))
  
  t$totalTestResults[is.na(t$totalTestResults)] <- 0
  t$positive[is.na(t$positive)] <- 0
  t$negative[is.na(t$negative)] <- 0
  t$death[is.na(t$death)] <- 0
  
  t <- mutate(t,
              tested2positive = positive / totalTestResults,
              positive2death = death / positive,
              tested2death = death / totalTestResults
              )

  t$tested2positive[is.na(t$tested2positive)] <- 0
  t$positive2death[is.na(t$positive2death)] <- 0
  t$tested2death[is.na(t$tested2death)] <- 0

  # local cache
  write.csv(t, "./data/covidtracking_us_daily.csv")
  
  return(t)
  
}
