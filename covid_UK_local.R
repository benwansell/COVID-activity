#libraries
library(tidyverse)
library(Hmisc)
library(lubridate)
library(broom)

# All Google data and demographics from https://github.com/datasciencecampus/google-mobility-reports-data

uk_local <- read_csv("./mobility_data/csvs/international_local_area_trends_G20_20200410.csv")

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

uk_final <- uk_wide %>% 
  left_join(uk_population, by = "location") %>% 
  left_join(uk_brexit_co, by = "GSS_CD") %>% 
  left_join(uk_brexit_la, by = "GSS_CD")

uk_final %>% 
  group_by(location) %>% 
  summarise(county_remain = mean(county_remain, na.rm=T),
            la_remain = mean(la_remain, na.rm=T)) %>% 
  View()

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

uk_final %>% 
  group_by(location) %>% 
  summarise(remain = mean(final_remain, na.rm=T)) %>% 
  View()

# Basic scatters

uk_final %>% 
  filter(date == "2020-04-03") %>% 
  filter(location!="Na h-Eileanan an Iar") %>% 
  ggplot(aes(x=final_remain, y=workplace))+
  geom_text(aes(label=location))+
  #geom_point(aes(size = AREALHECT))+
  theme_classic()+
  ylab("Percentage Reduction in Time at Workplace (April 3rd)")+xlab("Remain Vote at LA / County")

ggsave("./fig/brexit_social_distancing.png", width = 8, height = 8)

# By region

uk_final %>% 
  filter(date == "2020-04-03") %>% 
  filter(location!="Na h-Eileanan an Iar") %>% 
  filter(REGION_NM!="London" & REGION_NM !="Northern Ireland") %>% 
  ggplot(aes(x=final_remain, y=workplace))+
  geom_point()+
  theme_classic()+
  ylab("Percentage Reduction in Time at Workplace (April 3rd)")+xlab("Remain Vote at LA / County")+
  facet_wrap(~REGION_NM)

ggsave("./fig/regions.png", width = 8, height=  8)

### Regression Analyses
 
uk_final %>% 
  filter(date == "2020-04-03") %>% 
  lm(data =., workplace ~ final_remain+pop_under_30+pop_over_70+density+I(REGION_NM)) %>% 
  summary()


uk_final %>% 
  filter(date == as.Date("2020-03-17")) %>% 
  lm(data =., workplace ~ final_remain+pop_under_30+pop_over_70+density+I(REGION_NM)) %>% 
  summary()

# Time series of workplace coefficients

#map for faster, tidier and grab all coeficiens from regression
coefs_map <- map_df(unique(uk_final$date), function(date_i) {
  filtered_data <- filter(uk_final, date == date_i) %>%
    lm(data =., workplace ~ final_remain+pop_under_30+pop_over_70+density+I(REGION_NM)) %>%
    broom::tidy() %>%
    rename(coef = estimate, se = std.error) %>%
    mutate(date = date_i)
})

coefs_map <- coefs_map %>% 
  mutate(upper = coef+2*se,
         lower = coef-2*se)

#is it the weekend
#you probably want a case_when for easter friday and monday which I assume isn't in the dataset yet
weekend_df <- data.frame(date = unique(coefs_map$date)) %>%
  mutate(day = weekdays(date)) %>%
  mutate(weekend= case_when(
    day %in% c("Saturday", "Sunday") ~ 1
    #day is easter monday/friday ~ 1
  ))

coefs_map %>% 
  left_join(weekend_df) %>%
  ggplot(aes(x = date, y = coef))+
  geom_line()+
  geom_ribbon(aes(ymin=lower, ymax = upper, alpha=0.3))+
  #doesn't play very well with all the faceting etc. and cba work it out
  #geom_rect(aes(xmin = date, xmax = lead(date), ymin = -Inf, ymax = Inf, fill = "green"), alpha = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "red") +
  #lockdown
  geom_vline(xintercept = as.Date("2020-03-23"), linetype = "dashed", colour = "darkblue", size = 2) +
  xlab("Date")+ylab("Coefficient on Remain Vote")+
  theme_classic()+
  theme(legend.position = "none") +
  facet_wrap(~term, scales = "free_y")

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


# Partial Regression Graph

uk_april_3 <- uk_final %>% 
  filter(date == "2020-04-03")


wp_fit<-    lm(data =uk_april_3, workplace ~ pop_under_30+pop_over_70+density+I(REGION_NM))
rem_fit<-   lm(data =uk_april_3, final_remain ~ pop_under_30+pop_over_70+density+I(REGION_NM))

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


# Different Activity Measures

coef2=NULL
se2=NULL



reg1<-uk_final %>% 
  filter(date == "2020-04-03") %>% 
  lm(data =., grocery ~ final_remain+pop_under_30+pop_over_70+density+I(REGION_NM)) 
coef2[1]<-reg1$coefficient[2]
se2[1]<-summary(reg1)$coefficients["final_remain", "Std. Error"]

reg2<-uk_final %>% 
  filter(date == "2020-04-03") %>% 
  lm(data =., parks ~ final_remain+pop_under_30+pop_over_70+density+I(REGION_NM)) 
coef2[2]<-reg2$coefficient[2]
se2[2]<-summary(reg2)$coefficients["final_remain", "Std. Error"]

reg3<-uk_final %>% 
  filter(date == "2020-04-03") %>% 
  lm(data =., residential ~ final_remain+pop_under_30+pop_over_70+density+I(REGION_NM)) 
coef2[3]<-reg3$coefficient[2]
se2[3]<-summary(reg3)$coefficients["final_remain", "Std. Error"]

reg4<-uk_final %>% 
  filter(date == "2020-04-03") %>% 
  lm(data =., retail_rec ~ final_remain+pop_under_30+pop_over_70+density+I(REGION_NM)) 
coef2[4]<-reg4$coefficient[2]
se2[4]<-summary(reg4)$coefficients["final_remain", "Std. Error"]


reg5<-uk_final %>% 
  filter(date == "2020-04-03") %>% 
  lm(data =., transit ~ final_remain+pop_under_30+pop_over_70+density+I(REGION_NM)) 
coef2[5]<-reg5$coefficient[2]
se2[5]<-summary(reg5)$coefficients["final_remain", "Std. Error"]

reg6<-uk_final %>% 
  filter(date == "2020-04-03") %>% 
  lm(data =., workplace ~ final_remain+pop_under_30+pop_over_70+density+I(REGION_NM)) 
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
