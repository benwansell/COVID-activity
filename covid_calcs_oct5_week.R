# Use Government API to download COVID case data

#remotes::install_github("publichealthengland/coronavirus-dashboard-api-R-sdk")
# Use https://coronavirus.data.gov.uk/developers-guide#sdks

library(tidyverse)
library(ukcovid19)


cases_and_deaths = list(
  date = "date",
  areaName = "areaName",
  areaCode = "areaCode",
  newCasesBySpecimenDate = "newCasesBySpecimenDate",
  cumCasesBySpecimenDate = "cumCasesBySpecimenDate",
  cumCasesBySpecimenDateRate = "cumCasesBySpecimenDateRate",
  newDeaths28DaysByPublishDate = "newDeaths28DaysByPublishDate",
  cumDeaths28DaysByPublishDate = "cumDeaths28DaysByPublishDate",
  cumDeaths28DaysByPublishDateRate = "cumDeaths28DaysByPublishDateRate"
)


cov_filters_oct2 <- c("areaType=ltla", "date=2020-10-02")

uk_covid_oct2 <- get_data(
  filters = cov_filters_oct2,
  structure = cases_and_deaths
)

cov_filters_oct3 <- c("areaType=ltla", "date=2020-10-03")

uk_covid_oct3 <- get_data(
  filters = cov_filters_oct3,
  structure = cases_and_deaths
)


cov_filters_oct4 <- c("areaType=ltla", "date=2020-10-04")

uk_covid_oct4 <- get_data(
  filters = cov_filters_oct4,
  structure = cases_and_deaths
)


cov_filters_oct5 <- c("areaType=ltla", "date=2020-10-05")

uk_covid_oct5 <- get_data(
  filters = cov_filters_oct5,
  structure = cases_and_deaths
)

cov_filters_sep24 <- c("areaType=ltla", "date=2020-09-24")

uk_covid_sep24 <- get_data(
  filters = cov_filters_sep24,
  structure = cases_and_deaths
)

cov_filters_sep25 <- c("areaType=ltla", "date=2020-09-25")

uk_covid_sep25 <- get_data(
  filters = cov_filters_sep25,
  structure = cases_and_deaths
)

cov_filters_sep26 <- c("areaType=ltla", "date=2020-09-26")

uk_covid_sep26 <- get_data(
  filters = cov_filters_sep26,
  structure = cases_and_deaths
)

cov_filters_sep27 <- c("areaType=ltla", "date=2020-09-27")

uk_covid_sep27 <- get_data(
  filters = cov_filters_sep27,
  structure = cases_and_deaths
)

cov_filters_sep28 <- c("areaType=ltla", "date=2020-09-28")

uk_covid_sep28 <- get_data(
  filters = cov_filters_sep28,
  structure = cases_and_deaths
)

cov_filters_sep29 <- c("areaType=ltla", "date=2020-09-29")

uk_covid_sep29 <- get_data(
  filters = cov_filters_sep29,
  structure = cases_and_deaths
)

cov_filters_sep30 <- c("areaType=ltla", "date=2020-09-30")

uk_covid_sep30 <- get_data(
  filters = cov_filters_sep30,
  structure = cases_and_deaths
)

cov_filters_oct1 <- c("areaType=ltla", "date=2020-10-01")

uk_covid_oct1 <- get_data(
  filters = cov_filters_oct1,
  structure = cases_and_deaths
)

  



uk_covid_data <- rbind( uk_covid_sep29, uk_covid_sep30, uk_covid_oct1, uk_covid_oct2, uk_covid_oct3, uk_covid_oct4, uk_covid_oct5)

uk_covid_data %>% 
  arrange(areaName, date) %>% 
  group_by(areaName) %>% View()

uk_covid_data <- uk_covid_data %>% 
  group_by(areaName) %>% 
  mutate(week_cases = sum(newCasesBySpecimenDate), 
         week_cases_rate = week_cases/(cumCasesBySpecimenDate/cumCasesBySpecimenDateRate),
         week_deaths = sum(newDeaths28DaysByPublishDate) ) %>% 
  filter(date == "2020-10-05") %>% 
  select(-c(newCasesBySpecimenDate, newDeaths28DaysByPublishDate))


uk_covid_data %>% 
  filter(str_sub(areaCode, 1, 1)!="N") %>% 
  mutate(russell = if_else(areaName %in% c("Nottingham", "Newcastle upon Tyne", "Birmingham", "County Durham", "Manchester", 
                                             "Leeds", "Sheffield", "Southampton", "Exeter", "Liverpool", "Cardiff",
                                             "City of Edinburgh", "Glasgow City", "Bristol, City of", "Coventry", "York"), "black", "grey")) %>% 
  ggplot(aes(x = cumCasesBySpecimenDateRate, y = week_cases_rate))+
  geom_text(aes(label=areaName, color = russell))+
  scale_color_manual(values = c("black", "grey"))+ 
  xlab("Cumulative Case Rate per 100,000")+ylab("Weekly Case Rate per 100,000 (Oct 5)")+
    theme_classic()+
  theme(legend.position = "none")


