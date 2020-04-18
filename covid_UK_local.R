
# preliminaries -----------------------------------------------------------


#libraries
library(tidyverse)
library(Hmisc)
library(lubridate)
library(broom)
library(gganimate)
library(transformr)


# import google data ------------------------------------------------------



# All Google data and demographics from https://github.com/datasciencecampus/google-mobility-reports-data - now UPDATED to 04-17 data

uk_local <- read_csv("./mobility_data/csvs/international_local_area_trends_G20_20200417.csv")

uk_local <- uk_local %>% 
  filter(Country=="GB")

# Data needs to be made long to have dates as observations, then wide to forms of activity as variables

uk_long <- uk_local %>% 
  select(-Country) %>% 
  pivot_longer(-location:-category, names_to="date", values_to="difference")

uk_wide <- uk_long %>% 
  pivot_wider(names_from=category, values_from=difference) %>% 
  mutate(date = as.Date(date)) %>% 
  rename (grocery = `Grocery & pharmacy`,
          parks = Parks,
          residential = Residential,
          retail_rec = `Retail & recreation`,
          transit = `Transit stations`,
          workplace = Workplace)

# Match to demographic data - calculate age profile of each unit

#uk_lookup <- read_csv("mobility_data/geography/Google_Places_Lookup_Table_200403.csv")

#uk_lookup <- uk_lookup %>% 
 # rename(location = GPL_NM)

uk_population <- read_csv("./mobility_data/geography/Google_Places_All_UK_2018_Population_Estimates.csv")

uk_population <- uk_population %>% 
  mutate(pop_under_10 = rowSums(select(., A_0:A_9))*100/ALL_AGES,
         pop_10_19 = rowSums(select(., A_10:A_19))*100/ALL_AGES,
         pop_20_29 = rowSums(select(., A_20:A_29))*100/ALL_AGES,
         pop_30_39 = rowSums(select(., A_30:A_39))*100/ALL_AGES,
         pop_40_49 = rowSums(select(., A_40:A_49))*100/ALL_AGES,
         pop_50_59 = rowSums(select(., A_50:A_59))*100/ALL_AGES,
         pop_60_69 = rowSums(select(., A_60:A_69))*100/ALL_AGES,
         pop_70_79 = rowSums(select(., A_70:A_79))*100/ALL_AGES,
         pop_80_89 = rowSums(select(., A_80:A_89))*100/ALL_AGES,
         pop_over_90 = 100*A_90_PLUS/ALL_AGES,
         pop_over_70 = pop_70_79+pop_80_89+pop_over_90,
         pop_under_30 = pop_under_10+pop_10_19+pop_20_29) %>% 
    select(GPL_CD:ALL_AGES, pop_under_10:pop_under_30) %>% 
  rename(location = GPL_NM)

#Merge

uk_final <- uk_wide %>% 
  left_join(uk_population, by = "location")


# ONS Lookup to match local authority districts to counties: http://geoportal.statistics.gov.uk/datasets/363d7613224e4d359558969133a458e6_0
county_lad_england <- read_csv("./other_data/Local_Authority_District_to_County_December_2018_Lookup_in_England.csv")

county_lad_england <- county_lad_england %>% 
  rename(Area_Code = LAD18CD)


# import GDP data ---------------------------------------------------------



# GDP per capita and growth by district or county or London: Taken from https://www.ons.gov.uk/economy/grossdomesticproductgdp/datasets/regionalgrossdomesticproductlocalauthorities

gdp_percap <- read_csv("other_data/gdp_percap.csv", 
                       skip = 1)

gdp_percap<- gdp_percap %>% 
  select(`NUTS1 Region`, `LA code`, `LA name`, `20183`) %>% 
  rename(region = `NUTS1 Region`,
         Area_Code = `LA code`,
         Area_Name = `LA name`,
         gdp_per_cap = `20183`) %>% 
  filter(!is.na(region)) %>% 
  slice(1:382)


gdp_pc_growth <- read_csv("other_data/gdp_percap_growth.csv", 
                       skip = 1)

gdp_pc_growth<- gdp_pc_growth %>% 
  select( `LA code`,  `20183`) %>% 
  rename(Area_Code = `LA code`,
         gdp_pc_growth = `20183`) %>% 
  filter(!is.na(Area_Code))

population <- read_csv("other_data/pop_by_la.csv", 
                       skip = 1)

population<- population %>% 
  select( `LA code`,  `2018`) %>% 
  rename(Area_Code = `LA code`,
         population = `2018`) %>% 
  filter(!is.na(Area_Code))

gdp_data <- gdp_percap %>% 
  left_join(gdp_pc_growth, by="Area_Code") %>% 
  left_join(population, by = "Area_Code")

gdp_data <- gdp_data %>% 
  left_join (county_lad_england, by = "Area_Code")

# Counties

gdp_data_co <- gdp_data %>% 
  group_by(CTY18CD, CTY18NM) %>%
  summarise(county_gdp_pc = weighted.mean(gdp_per_cap, population, na.rm=T),
            county_gdp_pc_growth = weighted.mean(gdp_pc_growth, population, na.rm=T))  %>% 
  rename(GSS_CD = CTY18CD)

gdp_data_la <- gdp_data %>% 
  mutate(la_gdp_pc = gdp_per_cap,
         la_gdp_pc_growth = gdp_pc_growth)  %>% 
  select(Area_Code, Area_Name, la_gdp_pc, la_gdp_pc_growth) %>% 
  rename(GSS_CD = Area_Code)

uk_final <- uk_final %>% 
  left_join(gdp_data_co, by ="GSS_CD") %>% 
  left_join(gdp_data_la, by ="GSS_CD")



uk_final <- uk_final %>% 
  mutate(final_gdp_cap  = if_else(!is.na(la_gdp_pc), la_gdp_pc, county_gdp_pc),
         final_gdp_pc_growth  = if_else(!is.na(la_gdp_pc_growth), la_gdp_pc_growth, county_gdp_pc_growth),
         final_gdp_cap = case_when(location=="Glasgow City" ~ 35934,
                                  location=="Fife" ~25701,
                                  location=="Dorset" ~ 24267,
                                  location=="Greater London" ~ 54685,
                                  location=="North Lanarkshire" ~ 25453,
                                  location=="Nottinghamshire" ~21543,
                                  location=="Perth and Kinross" ~ 31413,
                                  location=="Shropshire"~ 23309,
                                  TRUE ~ final_gdp_cap),
         final_gdp_pc_growth = case_when(location=="Glasgow City" ~ 1.2,
                                   location=="Fife" ~-4.8,
                                   location=="Dorset" ~ 1.7,
                                   location=="Greater London" ~ 0.01,
                                   location=="North Lanarkshire" ~ 0.5,
                                   location=="Nottinghamshire" ~1.88,
                                   location=="Perth and Kinross" ~ -4.2,
                                   location=="Shropshire"~ -1.5,
                                   TRUE ~ final_gdp_pc_growth))

# uk_final %>% 
#   group_by(location) %>% 
#   summarise(county_gdp = mean(county_gdp_pc, na.rm=T),
#             la_gdp = mean(la_gdp_pc, na.rm=T),
#             final_gdp = mean(final_gdp_cap, na.rm=T)) %>% 
#   View()

# Local authorities


# import brexit data ------------------------------------------------------



# Local authority level Brexit results from Electoral Commission: 
#https://www.electoralcommission.org.uk/who-we-are-and-what-we-do/elections-and-referendums/past-elections-and-referendums/eu-referendum/results-and-turnout-eu-referendum

uk_brexit <- read_csv("./other_data/EU-referendum-result-data.csv")

uk_brexit <- uk_brexit %>% 
  left_join (county_lad_england, by = "Area_Code")



#Calculate Brexit vote at both local authority (in data) and county (weighted average of local authorities in county)

# Counties

uk_brexit_co <- uk_brexit %>% 
  group_by(CTY18CD, CTY18NM) %>% 
  summarise(county_remain = weighted.mean(Pct_Remain, Electorate, na.rm=T),
            county_pop = sum(Electorate))  %>% 
  rename(GSS_CD = CTY18CD)

# Local authorities

uk_brexit_la <- uk_brexit %>%
  mutate(la_remain = Pct_Remain) %>% 
  select(Area_Code, Area, la_remain) %>% 
  rename(GSS_CD= Area_Code)

# Join population, Brexit vote (countries and local authorities) to main dataset

uk_final <- uk_final %>%
  left_join(uk_brexit_co, by = "GSS_CD") %>% 
  left_join(uk_brexit_la, by = "GSS_CD")

# uk_final %>% 
#   group_by(location) %>% 
#   summarise(county_remain = mean(county_remain, na.rm=T),
#             la_remain = mean(la_remain, na.rm=T)) %>% 
#   View()

# This gives locations their local authority Brexit vote if they are LAs and their county Brexit vote if not.
# Also have to manually enter vote for a few mismatches. 
# Greater London is grand average of Outer and Inner London (themselves averages across boroughs)

uk_final <- uk_final %>% 
  mutate(final_remain  = if_else(!is.na(la_remain), la_remain, county_remain),
         final_remain = if_else(location =="Greater London", 60.17 , final_remain),
         final_remain = case_when(location=="Glasgow City" ~ 66.59,
                                  location=="Fife" ~58.59,
                                  location=="Dorset" ~ 43.38,
                                  location=="North Lanarkshire" ~ 61.66,
                                  location=="Nottinghamshire" ~39.84,
                                  location=="Perth and Kinross" ~ 61.09,
                                  location=="Shropshire"~ 43.13,
                                  TRUE ~ final_remain),
         log_population = log(ALL_AGES),
         log_area = log(AREALHECT),
         density = ALL_AGES/AREALHECT)

# Pops up dataset of averages to check - should just miss Northern Ireland

# uk_final %>% 
#   group_by(location) %>% 
#   summarise(remain = mean(final_remain, na.rm=T)) %>% 
#   View()


# basic scatters ----------------------------------------------------------



# Basic scatters

uk_final %>% 
  filter(date == "2020-03-13") %>% 
  filter(location!="Na h-Eileanan an Iar") %>% 
  ggplot(aes(x=final_remain, y=workplace))+
  geom_text(aes(label=location))+
  #geom_point(aes(size = AREALHECT))+
  theme_classic()+
  ylab("Percentage Reduction in Time at Workplace (March 13th)")+xlab("Remain Vote at LA / County")

ggsave("./fig/brexit_social_distancing_mar_13.png", width = 10, height = 10)



uk_final %>% 
  filter(date == "2020-04-03") %>% 
  filter(location!="Na h-Eileanan an Iar") %>% 
  ggplot(aes(x=final_remain, y=workplace))+
  geom_text(aes(label=location))+
  #geom_point(aes(size = AREALHECT))+
  theme_classic()+
  ylab("Percentage Reduction in Time at Workplace (April 3rd)")+xlab("Remain Vote at LA / County")

ggsave("./fig/brexit_social_distancing_apr_3.png", width = 10, height = 10)

uk_final %>% 
  filter(date == "2020-04-09") %>% 
  filter(location!="Na h-Eileanan an Iar") %>% 
  ggplot(aes(x=final_remain, y=workplace))+
  geom_text(aes(label=location))+
  #geom_point(aes(size = AREALHECT))+
  theme_classic()+
  ylab("Percentage Reduction in Time at Workplace (April 9th)")+xlab("Remain Vote at LA / County")

ggsave("./fig/brexit_social_distancing_apr_9.png", width = 10, height = 10)


uk_final %>% 
  filter(date == "2020-03-13") %>% 
  filter(location!="Na h-Eileanan an Iar") %>% 
  ggplot(aes(x=final_gdp_cap, y=workplace))+
  geom_text(aes(label=location))+
  #geom_point(aes(size = AREALHECT))+
  theme_classic()+
  ylab("Percentage Reduction in Time at Workplace (March 13th)")+xlab("Remain Vote at LA / County")

ggsave("./fig/brexit_social_distancing_gdp_mar_13.png", width = 10, height = 10)


uk_final %>% 
  filter(date == "2020-04-03") %>% 
  filter(location!="Na h-Eileanan an Iar") %>% 
  ggplot(aes(x=final_gdp_cap, y=workplace))+
  geom_text(aes(label=location))+
  #geom_point(aes(size = AREALHECT))+
  theme_classic()+
  ylab("Percentage Reduction in Time at Workplace (April 3rd)")+xlab("GDP per capita at LA / County")

ggsave("./fig/brexit_social_distancing_gdp_apr_3.png", width = 10, height = 10)

uk_final %>% 
  filter(date == "2020-04-09") %>% 
  filter(location!="Na h-Eileanan an Iar") %>% 
  ggplot(aes(x=final_gdp_cap, y=workplace))+
  geom_text(aes(label=location))+
  #geom_point(aes(size = AREALHECT))+
  theme_classic()+
  ylab("Percentage Reduction in Time at Workplace (April 9th)")+xlab("GDP per capita at LA / County")

ggsave("./fig/brexit_social_distancing_gdp_apr_9.png", width = 10, height = 10)

uk_final %>% 
  filter(date == "2020-03-13") %>% 
  filter(location!="Na h-Eileanan an Iar") %>% 
  ggplot(aes(x=density, y=workplace))+
  geom_text(aes(label=location))+
  #geom_point(aes(size = AREALHECT))+
  theme_classic()+
  scale_x_log10()+
  ylab("Percentage Reduction in Time at Workplace (March 13th)")+xlab("Population Density at LA / County")

ggsave("./fig/brexit_social_distancing_dens_mar_13.png", width = 10, height = 10)


uk_final %>% 
  filter(date == "2020-04-03") %>% 
  filter(location!="Na h-Eileanan an Iar") %>% 
  ggplot(aes(x=density, y=workplace))+
  geom_text(aes(label=location))+
  #geom_point(aes(size = AREALHECT))+
  theme_classic()+
  ylab("Percentage Reduction in Time at Workplace (April 3rd)")+xlab("Population Density at LA / County")

ggsave("./fig/brexit_social_distancing_den_apr_3.png", width = 10, height = 10)

uk_final %>% 
  filter(date == "2020-04-09") %>% 
  filter(location!="Na h-Eileanan an Iar") %>% 
  ggplot(aes(x=density, y=workplace))+
  geom_text(aes(label=location))+
  #geom_point(aes(size = AREALHECT))+
  theme_classic()+
  ylab("Percentage Reduction in Time at Workplace (April 9th)")+xlab("Population Density at LA / County")

ggsave("./fig/brexit_social_distancing_den_apr_9.png", width = 10, height = 10)


# animated scatters -------------------------------------------------------

  uk_final %>% 
  filter(location!="Na h-Eileanan an Iar") %>% 
  ggplot(aes(x=final_remain, y=workplace))+
  #geom_text(aes(label=location))+
  #geom_point(aes(size = AREALHECT))+
  geom_point(aes(size=ALL_AGES), alpha=0.6)+
  geom_smooth(method="lm", color="black")+
  theme_classic()+
  transition_time(date)+
  ease_aes("linear")+
  labs(title="Date:{frame_time}")+
  theme(plot.title = element_text(hjust = 0.5), legend.position="none")+
  ylab("Percentage Reduction in Time at Workplace")+xlab("Remain Vote at LA / County")

anim_save("./fig/remain_animation.gif")


uk_final %>% 
  filter(location!="Na h-Eileanan an Iar") %>% 
  ggplot(aes(x=final_gdp_cap, y=workplace))+
  #geom_text(aes(label=location))+
  #geom_point(aes(size = AREALHECT))+
  geom_point(aes(size=ALL_AGES), alpha=0.6)+
  geom_smooth(method="lm", color="black")+
  theme_classic()+
  transition_time(date)+
  ease_aes("linear")+
  labs(title="Date:{frame_time}")+
  theme(plot.title = element_text(hjust = 0.5), legend.position="none")+
  ylab("Percentage Reduction in Time at Workplace")+xlab("GDP per capita at LA / County")

anim_save("./fig/gdp_animation.gif")

uk_final %>% 
  filter(location!="Na h-Eileanan an Iar") %>% 
  ggplot(aes(x=log(density), y=workplace))+
  #geom_text(aes(label=location))+
  #geom_point(aes(size = AREALHECT))+
  geom_point(aes(size=ALL_AGES), alpha=0.6)+
  geom_smooth(method="lm", color="black")+
  theme_classic()+
  transition_time(date)+
  ease_aes("linear")+
  labs(title="Date:{frame_time}")+
  theme(plot.title = element_text(hjust = 0.5), legend.position="none")+
  ylab("Percentage Reduction in Time at Workplace")+xlab("Log(density) at LA / County")

anim_save("./fig/density_animation.gif")

# Animated scatters


# regional plots ----------------------------------------------------------



# By region

uk_final %>% 
  filter(date == "2020-04-09") %>% 
  filter(location!="Na h-Eileanan an Iar") %>% 
  filter(REGION_NM!="London" & REGION_NM !="Northern Ireland") %>% 
  ggplot(aes(x=final_remain, y=workplace))+
  geom_point()+
  theme_classic()+
  ylab("Percentage Reduction in Time at Workplace (April 9th)")+xlab("Remain Vote at LA / County")+
  facet_wrap(~REGION_NM)

ggsave("./fig/regions.png", width = 8, height=  8)


# regressions -------------------------------------------------------------



### Regression Analyses
 
uk_final %>% 
  filter(date == "2020-04-03") %>% 
  lm(data =., workplace ~ final_remain+pop_under_30+pop_over_70+log(density)+I(REGION_NM)) %>% 
  summary()

uk_final %>% 
  filter(date == "2020-04-03") %>% 
  lm(data =., workplace ~ final_remain+ final_gdp_cap+final_gdp_pc_growth+pop_under_30+pop_over_70+log(density)+I(REGION_NM)) %>% 
  summary()

uk_final %>% 
  filter(date == "2020-04-09") %>% 
  lm(data =., workplace ~ final_remain+ final_gdp_cap+final_gdp_pc_growth+pop_under_30+pop_over_70+log(density)+I(REGION_NM)) %>% 
  summary()

uk_final %>% 
  filter(date == "2020-04-09") %>% 
  lm(data =., residential ~ final_remain+ final_gdp_cap+final_gdp_pc_growth+pop_under_30+pop_over_70+log(density)+I(REGION_NM)) %>% 
  summary()


uk_final %>% 
  filter(date == as.Date("2020-03-17")) %>% 
  lm(data =., workplace ~ final_remain++ final_gdp_cap+final_gdp_pc_growth+pop_under_30+pop_over_70+log(density)+I(REGION_NM)) %>% 
  summary()



# time series of coefficients ---------------------------------------------


# Time series of workplace coefficients

#map for faster, tidier and grab all coeficiens from regression
coefs_map <- map_df(unique(uk_final$date), function(date_i) {
  filtered_data <- filter(uk_final, date == date_i) %>%
    lm(data =., workplace ~ final_remain+final_gdp_cap+final_gdp_pc_growth+pop_under_30+pop_over_70+log(density)+I(REGION_NM)) %>%
    broom::tidy() %>%
    rename(coef = estimate, se = std.error) %>%
    mutate(date = date_i)
})

coefs_map <- coefs_map %>% 
  mutate(upper = coef+2*se,
         lower = coef-2*se) %>% 
  filter(term %in% c("final_remain", "final_gdp_cap", "final_gdp_pc_growth", "pop_under_30", "pop_over_70", "log(density)"))

#is it the weekend
#you probably want a case_when for easter friday and monday which I assume isn't in the dataset yet
weekend_df <- data.frame(date = unique(coefs_map$date)) %>%
  mutate(day = weekdays(date)) %>%
  mutate(weekend= case_when(
    day %in% c("Saturday", "Sunday") ~ 1,
    date==as.Date("2020-04-10") ~ 1
    #day is easter monday/friday ~ 1
  ))

coefs_map %>% 
  left_join(weekend_df) %>%
  ggplot(aes(x = date, y = coef))+
  geom_line()+
  #doesn't play very well with all the faceting etc. and cba work it out - hacky fix by BA but looks ugly
  # geom_rect(aes(xmin = as.Date("2020-02-29"), xmax = as.Date("2020-03-01"), ymin = -Inf, ymax = Inf, alpha=0.1), fill="light green", alpha = 0.1) +
  # geom_rect(aes(xmin = as.Date("2020-03-07"), xmax = as.Date("2020-03-08"), ymin = -Inf, ymax = Inf), fill="light green", alpha = 0.1) +
  # geom_rect(aes(xmin = as.Date("2020-03-14"), xmax = as.Date("2020-03-15"), ymin = -Inf, ymax = Inf), fill="light green", alpha = 0.1) +
  # geom_rect(aes(xmin = as.Date("2020-03-21"), xmax = as.Date("2020-03-22"), ymin = -Inf, ymax = Inf), fill="light green", alpha = 0.1) +
  # geom_rect(aes(xmin = as.Date("2020-03-28"), xmax = as.Date("2020-03-29"), ymin = -Inf, ymax = Inf), fill="light green", alpha = 0.1) +
  # geom_rect(aes(xmin = as.Date("2020-04-04"), xmax = as.Date("2020-04-05"), ymin = -Inf, ymax = Inf), fill="light green", alpha = 0.1) +
  # geom_rect(aes(xmin = as.Date("2020-04-10"), xmax = as.Date("2020-04-12"), ymin = -Inf, ymax = Inf), fill="light green", alpha = 0.1) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "red") +
  geom_ribbon(aes(ymin=lower, ymax = upper, alpha=0.3), color="lightgray")+
  #lockdown
  geom_vline(xintercept = as.Date("2020-03-23"), linetype = "dashed", colour = "darkblue", size = 1) +
  xlab("Date")+ylab("Coefficients in Regression")+
  theme_classic()+
  theme(legend.position = "none") +
  facet_wrap(~term, scales = "free_y")

ggsave("./fig/workplace.png", height = 8, width = 8)

ggsave("./fig/regression_coefs_time_series.png", height = 8, width = 8)

#just filter out the original graph (only final_remain)
coefs_map %>% 
  left_join(weekend_df) %>%
  #for the over 70 graph obvs just filter for that here
  filter(term == "final_remain") %>%
  ggplot(aes(x = date, y = coef))+
  geom_line()+
  geom_ribbon(aes(ymin=lower, ymax = upper, alpha=0.3))+
  geom_rect(aes(xmin = date, xmax = lead(date), ymin = -Inf, ymax = Inf, fill = factor(weekend)), alpha = 0.2) +
  scale_fill_manual(values = c("green", NA)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "red") +
  geom_vline(xintercept = as.Date("2020-03-23"), linetype = "dashed", colour = "darkblue", size = 2) +
  xlab("Date")+ylab("Coefficient on Remain Vote")+
  theme_classic()+
  theme(legend.position = "none")

ggsave("./fig/remain_regression_coefs_time_series.png", height = 8, width = 8)




# different types of activity ---------------------------------------------


### Graph with different dependent variables


coefs_map_a <- map_df(unique(uk_final$date), function(date_i) {
  filtered_data <- filter(uk_final, date == date_i) %>%
    lm(data =., grocery ~ final_remain+final_gdp_cap+final_gdp_pc_growth+pop_under_30+pop_over_70+log(density)+I(REGION_NM)) %>%
    broom::tidy() %>%
    rename(coef = estimate, se = std.error) %>%
    mutate(date = date_i,
           response = "Grocery")
})

coefs_map_b <- map_df(unique(uk_final$date), function(date_i) {
  filtered_data <- filter(uk_final, date == date_i) %>%
    lm(data =., parks ~ final_remain+final_gdp_cap+final_gdp_pc_growth+pop_under_30+pop_over_70+log(density)+I(REGION_NM)) %>%
    broom::tidy() %>%
    rename(coef = estimate, se = std.error) %>%
    mutate(date = date_i,
           response = "Parks")
})

coefs_map_c <- map_df(unique(uk_final$date), function(date_i) {
  filtered_data <- filter(uk_final, date == date_i) %>%
    lm(data =., residential ~ final_remain+final_gdp_cap+final_gdp_pc_growth+pop_under_30+pop_over_70+log(density)+I(REGION_NM)) %>%
    broom::tidy() %>%
    rename(coef = estimate, se = std.error) %>%
    mutate(date = date_i,
           response = "Residential")
})

coefs_map_d <- map_df(unique(uk_final$date), function(date_i) {
  filtered_data <- filter(uk_final, date == date_i) %>%
    lm(data =., retail_rec ~ final_remain+final_gdp_cap+final_gdp_pc_growth+pop_under_30+pop_over_70+log(density)+I(REGION_NM)) %>%
    broom::tidy() %>%
    rename(coef = estimate, se = std.error) %>%
    mutate(date = date_i,
           response= "Retail / Recreation")
})

coefs_map_e <- map_df(unique(uk_final$date), function(date_i) {
  filtered_data <- filter(uk_final, date == date_i) %>%
    lm(data =., transit ~ final_remain+final_gdp_cap+final_gdp_pc_growth+pop_under_30+pop_over_70+log(density)+I(REGION_NM)) %>%
    broom::tidy() %>%
    rename(coef = estimate, se = std.error) %>%
    mutate(date = date_i,
           response = "Transit")
})

coefs_map_f<- map_df(unique(uk_final$date), function(date_i) {
  filtered_data <- filter(uk_final, date == date_i) %>%
    lm(data =., workplace ~ final_remain+final_gdp_cap+final_gdp_pc_growth+pop_under_30+pop_over_70+log(density)+I(REGION_NM)) %>%
    broom::tidy() %>%
    rename(coef = estimate, se = std.error) %>%
    mutate(date = date_i,
           response = "Workplace")
})

coefs_map_2 <- bind_rows(coefs_map_a, coefs_map_b, coefs_map_c, coefs_map_d, coefs_map_e, coefs_map_f)

coefs_map_2 <- coefs_map_2 %>% 
  mutate(upper = coef+2*se,
         lower = coef-2*se) %>% 
  filter(term %in% c("final_remain", "final_gdp_cap", "final_gdp_pc_growth", "pop_under_30", "pop_over_70", "log(density)"))

## Effects of Remain Vote on Different DVs

coefs_map_2 %>% 
  left_join(weekend_df) %>%
  filter(term=="final_remain") %>% 
  ggplot(aes(x = date, y = coef))+
  geom_line()+
  geom_hline(yintercept = 0, linetype = "dashed", colour = "red") +
  geom_ribbon(aes(ymin=lower, ymax = upper, alpha=0.3), color="lightgray")+
  #lockdown
  geom_vline(xintercept = as.Date("2020-03-23"), linetype = "dashed", colour = "darkblue", size = 1) +
  xlab("Date")+ylab("Coefficients on Remain Vote")+
  theme_classic()+
  theme(legend.position = "none") +
  facet_wrap(~response, scales = "free_y")

ggsave("./fig/remain_all_dvs.png", height = 8, width = 8)

## Effects of GDP per capita on Different DVs

coefs_map_2 %>% 
  left_join(weekend_df) %>%
  filter(term=="final_gdp_cap") %>% 
  ggplot(aes(x = date, y = coef))+
  geom_line()+
  geom_hline(yintercept = 0, linetype = "dashed", colour = "red") +
  geom_ribbon(aes(ymin=lower, ymax = upper, alpha=0.3), color="lightgray")+
  #lockdown
  geom_vline(xintercept = as.Date("2020-03-23"), linetype = "dashed", colour = "darkblue", size = 1) +
  xlab("Date")+ylab("Coefficients on GDP per capita")+
  theme_classic()+
  theme(legend.position = "none") +
  facet_wrap(~response, scales = "free_y")

ggsave("./fig/gdp_all_dvs.png", height = 8, width = 8)

## Effects of Density on Different DVs

coefs_map_2 %>% 
  left_join(weekend_df) %>%
  filter(term=="log(density)") %>% 
  ggplot(aes(x = date, y = coef))+
  geom_line()+
  geom_hline(yintercept = 0, linetype = "dashed", colour = "red") +
  geom_ribbon(aes(ymin=lower, ymax = upper, alpha=0.3), color="lightgray")+
  #lockdown
  geom_vline(xintercept = as.Date("2020-03-23"), linetype = "dashed", colour = "darkblue", size = 1) +
  xlab("Date")+ylab("Coefficients on Log Density")+
  theme_classic()+
  theme(legend.position = "none") +
  facet_wrap(~response, scales = "free_y")

ggsave("./fig/density_all_dvs.png", height = 8, width = 8)


# partial regression plots ------------------------------------------------


# Partial Regression Graph

uk_april_3 <- uk_final %>% 
  filter(date == "2020-04-03")


wp_fit<-    lm(data =uk_april_3, workplace ~ final_gdp_cap+final_gdp_pc_growth+pop_under_30+pop_over_70+log(density)+I(REGION_NM))
rem_fit<-   lm(data =uk_april_3, final_remain ~final_gdp_cap+final_gdp_pc_growth+ pop_under_30+pop_over_70+log(density)+I(REGION_NM))

uk_final_reg <- augment(wp_fit, uk_april_3)
uk_final_reg <- uk_final_reg %>% 
  select(location:.resid) %>% 
  rename(wp_fitted = .fitted,
         wp_se_fit = .se.fit,
         wp_resid = .resid)


uk_final_reg <-augment(rem_fit, uk_final_reg)
uk_final_reg <- uk_final_reg %>% 
  select(location:.resid) %>% 
  rename(rem_fitted = .fitted,
         rem_se_fit = .se.fit,
         rem_resid = .resid)

uk_final_reg %>% 
  filter(date == "2020-04-03") %>% 
  filter(location!="Na h-Eileanan an Iar") %>% 
  ggplot(aes(x=rem_resid, y=wp_resid))+
  geom_text(aes(label=location))+
  geom_smooth(method="lm", color="black", size=0.5)+
  #geom_point(aes(size = AREALHECT))+
  theme_classic()+
  ylab("Percentage Reduction in Time at Workplace - Residual (April 3)")+xlab("Remain Vote at LA / County (Residual)")

ggsave("./fig/apr_3_partial_regression.png", width=10, height=10)


uk_april_9 <- uk_final %>% 
  filter(date == "2020-04-09")


wp_fit<-    lm(data =uk_april_9, workplace ~ final_gdp_cap+final_gdp_pc_growth+pop_under_30+pop_over_70+log(density)+I(REGION_NM))
rem_fit<-   lm(data =uk_april_9, final_remain ~final_gdp_cap+final_gdp_pc_growth+ pop_under_30+pop_over_70+log(density)+I(REGION_NM))

uk_final_reg <- augment(wp_fit, uk_april_9)
uk_final_reg <- uk_final_reg %>% 
  select(location:.resid) %>% 
  rename(wp_fitted = .fitted,
         wp_se_fit = .se.fit,
         wp_resid = .resid)


uk_final_reg <-augment(rem_fit, uk_final_reg)
uk_final_reg <- uk_final_reg %>% 
  select(location:.resid) %>% 
  rename(rem_fitted = .fitted,
         rem_se_fit = .se.fit,
         rem_resid = .resid)

uk_final_reg %>% 
  filter(date == "2020-04-09") %>% 
  filter(location!="Na h-Eileanan an Iar") %>% 
  ggplot(aes(x=rem_resid, y=wp_resid))+
  geom_text(aes(label=location))+
  geom_smooth(method="lm", color="black", size=0.5)+
  #geom_point(aes(size = AREALHECT))+
  theme_classic()+
  ylab("Percentage Reduction in Time at Workplace - Residual (April 9)")+xlab("Remain Vote at LA / County (Residual)")

ggsave("./fig/apr_9_partial_regression.png", width=10, height=10)


uk_april_9 <- uk_final %>% 
  filter(date == "2020-04-09")


wp_fit<-    lm(data =uk_april_9, workplace ~ final_remain+final_gdp_pc_growth+pop_under_30+pop_over_70+log(density)+I(REGION_NM))
gdp_fit<-   lm(data =uk_april_9, final_gdp_cap ~final_remain+final_gdp_pc_growth+ pop_under_30+pop_over_70+log(density)+I(REGION_NM))

uk_final_reg <- augment(wp_fit, uk_april_9)
uk_final_reg <- uk_final_reg %>% 
  select(location:.resid) %>% 
  rename(wp_fitted = .fitted,
         wp_se_fit = .se.fit,
         wp_resid = .resid)


uk_final_reg <-augment(gdp_fit, uk_final_reg)
uk_final_reg <- uk_final_reg %>% 
  select(location:.resid) %>% 
  rename(gdp_fitted = .fitted,
         gdp_se_fit = .se.fit,
         gdp_resid = .resid)

uk_final_reg %>% 
  filter(date == "2020-04-09") %>% 
  filter(location!="Na h-Eileanan an Iar") %>% 
  ggplot(aes(x=gdp_resid, y=wp_resid))+
  geom_text(aes(label=location))+
  geom_smooth(method="lm", color="black", size=0.5)+
  #geom_point(aes(size = AREALHECT))+
  theme_classic()+
  ylab("Percentage Reduction in Time at Workplace - Residual (April 9)")+xlab("GDP per capita at LA / County (Residual)")

ggsave("./fig/apr_9_partial_regression_gdp.png", width=10, height=10)


uk_april_9 <- uk_final %>% 
  filter(date == "2020-04-09")


wp_fit<-    lm(data =uk_april_9, workplace ~ final_remain+final_gdp_cap+final_gdp_pc_growth+pop_under_30+pop_over_70+I(REGION_NM))
den_fit<-   lm(data =uk_april_9, log(density) ~final_remain+final_gdp_cap+final_gdp_pc_growth+ pop_under_30+pop_over_70+I(REGION_NM))

uk_final_reg <- augment(wp_fit, uk_april_9)
uk_final_reg <- uk_final_reg %>% 
  select(location:.resid) %>% 
  rename(wp_fitted = .fitted,
         wp_se_fit = .se.fit,
         wp_resid = .resid)


uk_final_reg <-augment(den_fit, uk_final_reg)
uk_final_reg <- uk_final_reg %>% 
  select(location:.resid) %>% 
  rename(den_fitted = .fitted,
         den_se_fit = .se.fit,
         den_resid = .resid)

uk_final_reg %>% 
  filter(date == "2020-04-09") %>% 
  filter(location!="Na h-Eileanan an Iar") %>% 
  ggplot(aes(x=den_resid, y=wp_resid))+
  geom_text(aes(label=location))+
  geom_smooth(method="lm", color="black", size=0.5)+
  #geom_point(aes(size = AREALHECT))+
  theme_classic()+
  ylab("Percentage Reduction in Time at Workplace - Residual (April 9)")+xlab("Log(Density) at LA / County (Residual)")

ggsave("./fig/apr_9_partial_regression_den.png", width=10, height=10)



# chef plots by date ------------------------------------------------------



# Different Activity Measures

coef2=NULL
se2=NULL



reg1<-uk_final %>% 
  filter(date == "2020-04-03") %>% 
  lm(data =., grocery ~ final_remain+final_gdp_cap+final_gdp_pc_growth+pop_under_30+pop_over_70+density+I(REGION_NM)) 
coef2[1]<-reg1$coefficient[2]
se2[1]<-summary(reg1)$coefficients["final_remain", "Std. Error"]

reg2<-uk_final %>% 
  filter(date == "2020-04-03") %>% 
  lm(data =., parks ~ final_remain+final_gdp_cap+final_gdp_pc_growth+pop_under_30+pop_over_70+density+I(REGION_NM)) 
coef2[2]<-reg2$coefficient[2]
se2[2]<-summary(reg2)$coefficients["final_remain", "Std. Error"]

reg3<-uk_final %>% 
  filter(date == "2020-04-03") %>% 
  lm(data =., residential ~ final_remain+final_gdp_cap+final_gdp_pc_growth+pop_under_30+pop_over_70+density+I(REGION_NM)) 
coef2[3]<-reg3$coefficient[2]
se2[3]<-summary(reg3)$coefficients["final_remain", "Std. Error"]

reg4<-uk_final %>% 
  filter(date == "2020-04-03") %>% 
  lm(data =., retail_rec ~ final_remain+final_gdp_cap+final_gdp_pc_growth+pop_under_30+pop_over_70+density+I(REGION_NM)) 
coef2[4]<-reg4$coefficient[2]
se2[4]<-summary(reg4)$coefficients["final_remain", "Std. Error"]


reg5<-uk_final %>% 
  filter(date == "2020-04-03") %>% 
  lm(data =., transit ~ final_remain+final_gdp_cap+final_gdp_pc_growth+pop_under_30+pop_over_70+density+I(REGION_NM)) 
coef2[5]<-reg5$coefficient[2]
se2[5]<-summary(reg5)$coefficients["final_remain", "Std. Error"]

reg6<-uk_final %>% 
  filter(date == "2020-04-03") %>% 
  lm(data =., workplace ~ final_remain+final_gdp_cap+final_gdp_pc_growth+pop_under_30+pop_over_70+density+I(REGION_NM)) 
coef2[6]<-reg6$coefficient[2]
se2[6]<-summary(reg6)$coefficients["final_remain", "Std. Error"]

types<-cbind(c("Grocery", "Parks", "Residential", "Retail and Rec", "Transit", "Workplace"), coef2, se2)

types<-as.data.frame(types)
types<-as.tbl(types)
types<- types %>% 
  rename(type = V1) %>% 
  mutate(coef2 = as.double(as.character(coef2)),
         se2 = as.double(as.character(se2)),
         type = as.character(type),
         upper = coef2+2*se2,
         lower= coef2-2*se2)
 
types %>% 
  ggplot(aes(x=type, y=coef2))+
  geom_pointrange(aes(ymin = lower, ymax = upper))+
  xlab("Type of Activity")+
  ylab("Coefficient on Remain Vote (April 3rd)")+
  geom_abline(slope = 0, intercept = 0 , linetype = "dotted")+
  theme_classic()

ggsave("./fig/types_activity.png", width = 10, height =10)
