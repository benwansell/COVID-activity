library(tidyverse)
library(broom)

a<-data.frame(
  stringsAsFactors = FALSE,
           country = c("Argentina","Austria",
                       "Belgium","Brazil","Canada","Switzerland","Chile",
                       "Colombia","Germany","Dominican Republi","Algeria","Ecuador",
                       "Egypt","Spain","Finland","France",
                       "United Kingdom of","Hungary","Indonesia","India","Ireland","Italy",
                       "Japan","Mexico","Netherlands (the)","Peru",
                       "Philippines (the)","Poland","Portugal","Romania",
                       "Russian Federation","Saudi Arabia","Sweden","Turkey","Ukraine",
                       "United States of A","South Africa"),
              Iso3 = c("ARG","AUT","BEL","BRA",
                       "CAN","CHE","CHL","COL","DEU","DOM","DZA","ECU",
                       "EGY","ESP","FIN","FRA","GBR","HUN","IDN","IND",
                       "IRL","ITA","JPN","MEX","NLD","PER","PHL","POL","PRT",
                       "ROU","RUS","SAU","SWE","TUR","UKR","USA",
                       "ZAF"),
         mean_mort = c(0.254453868,0.70305109,
                       7.861977577,13.08982944,3.267152071,1.598400593,0.220118642,
                       0.417602718,6.579157352,0.279192001,0.874736667,
                       0.805740535,0.501979768,29.23356628,0.258884579,
                       22.12519836,24.04783058,0.375261575,0.716477156,3.476038218,
                       1.581101656,18.78667641,0.476880431,7.145730972,
                       5.404974937,2.04847312,0.457063943,0.789834261,1.029355645,
                       0.599669456,2.520816803,0.476594448,2.585143805,
                       3.484348059,0.30672127,43.73626709,0.836880863),
     log_mean_mort = c(-1.368635774,-0.352325708,
                       2.062038183,2.571835518,1.183918715,0.469003499,
                       -1.513588548,-0.873224735,1.883906722,-1.275855541,
                       -0.133832395,-0.215993509,-0.689195454,3.375317574,-1.351372957,
                       3.096717119,3.180044889,-0.980131984,-0.333408922,
                       1.24589324,0.458121866,2.933147907,-0.740489483,
                       1.966515064,1.687319756,0.717094719,-0.782931983,
                       -0.235932156,0.028933018,-0.511376679,0.924582958,-0.741089344,
                       0.94978112,1.248281002,-1.181815863,3.778177738,
                       -0.178073555),
               pop = c(44.494502,8.847037,11.422068,
                       209.469333,37.058856,8.516543,18.72916,49.648685,
                       82.927922,10.627165,42.228429,17.084357,98.423595,
                       46.723749,5.51805,66.987244,66.488991,9.768785,
                       267.663435,1352.617328,4.853506,60.431283,126.5291,126.190788,
                       17.231017,31.989256,106.651922,37.978548,10.281762,
                       19.473936,144.47805,33.699947,10.183175,82.319724,
                       44.622516,327.167434,57.779622)
)

b<-data.frame(
                         check.names = FALSE,
     log_pop= c(3.795365572,2.18008256,
                                            2.435547352,5.344577312,
                                            3.612507343,2.14201045,2.930081606,
                                            3.904971838,4.417971611,2.363413572,
                                            3.743093729,2.838163137,4.589280605,
                                            3.844252586,1.708024502,4.204502106,
                                            4.197036266,2.279192209,
                                            5.589730263,7.209796906,1.579701304,
                                            4.10150671,4.840472221,4.837794781,
                                            2.846711159,3.465399981,4.669570446,
                                            3.637021542,2.330371618,2.969076872,
                                            4.973127365,3.517496347,
                                            2.320736885,4.410610676,3.798238516,
                                            5.790472031,4.056636333),
                             Median.age = c(31.532,43.483,41.928,
                                            33.481,41.124,43.053,35.339,
                                            31.307,45.744,28.002,28.521,27.93,
                                            24.606,44.858,43.128,42.338,40.467,
                                            43.336,29.744,28.426,38.246,
                                            47.288,48.358,29.171,43.314,30.984,
                                            25.687,41.678,46.158,43.171,
                                            39.586,31.797,41.078,31.549,41.178,
                                            38.308,27.621),
       over_65 = c(11.198,19.202,18.571,
                                            8.552,16.984,18.436,11.087,7.646,
                                            21.453,6.981,6.211,7.104,5.159,
                                            19.436,21.228,19.718,18.517,
                                            18.577,5.319,5.989,13.928,23.021,
                                            27.049,6.857,18.779,7.151,4.803,
                                            16.763,21.502,17.85,14.178,3.295,
                                            19.985,8.153,16.462,15.413,
                                            5.344),
                       Pollution.levels = c(11.8,12.4,12.9,11.5,
                                            6.5,10.2,21,15.2,11.7,12.9,35.2,
                                            14.9,79.3,9.5,5.9,11.6,10.5,
                                            15.6,15.6,65.2,8.3,15.3,11.4,
                                            20.1,12.1,24.3,18.4,20.5,7.9,
                                            14.3,13.7,78.4,5.9,42,18.3,7.4,
                                            23.6)
   )

c<- data.frame(
         Mean_temp = c(17.08312035,3.60226965,
                       7.198050499,25.94433403,-13.5369606,3.199521542,10.44359493,
                       25.36407089,5.824878693,23.65955734,19.73687172,
                       22.18272591,20.25822067,10.04498005,-2.867335558,
                       8.096049309,6.205169678,7.352726936,26.49332619,24.05548096,
                       7.058465958,8.776521683,5.503757954,19.65898323,
                       6.944470882,20.21702194,25.9392662,4.848646641,
                       12.17464447,5.888294697,-12.54715347,22.67894936,-2.283463001,
                       7.340251446,4.765891075,2.793249846,20.85716248),
      arrivals_mil = c(6.942,30.816,9.119,6.621,
                       21.134,10.362,5.723,3.904,38.881,6.569,2.657,2.535,
                       11.196,82.773,3.224,89.322,36.316,17.552,15.81,
                       17.423,10.926,61.5672,31.192,41.313,18.78,4.419,7.168,
                       19.622,16.186,11.72,24.551,15.334,7.44,45.768,
                       14.104,79.74592,10.472),
          Pop_dens = c(16.51475944,109.289034,
                       382.7482166,25.43142481,4.150449826,219.015538,25.71000172,
                       45.86109419,240.3716577,224.5013245,18.41134759,
                       71.03825093,102.8021528,93.73452887,18.23264339,
                       119.2086157,280.6018435,106.7088258,150.987056,464.1494102,
                       71.6765278,205.5545931,346.9338179,66.32513851,
                       508.1516311,25.75925469,367.5121072,123.5888221,111.3299159,
                       83.58031889,8.911010468,16.19483135,24.61195594,
                       109.583913,75.49154008,36.18535576,48.89059344),
          diabetes = c(5.9,6.6,4.6,10.4,7.6,5.7,
                       8.6,7.4,10.4,8.6,6.7,5.5,17.2,6.9,5.6,4.8,3.9,
                       6.9,6.3,10.4,3.2,5,5.6,13.5,5.4,6.6,7.1,6.1,
                       9.8,6.9,6.1,15.8,4.8,11.1,6.1,10.8,12.7)
    )


d<- data.frame(
         neoplasms = c(1.176868831,2.228314728,
                       2.129967521,0.775327051,4.629727772,2.110949955,
                       1.237788506,0.875953763,2.398478606,0.753248307,0.581716794,
                       0.693830173,0.543731123,2.119853473,2.353037036,
                       2.091774092,2.791155517,1.7429261,0.426835825,0.312306273,
                       2.354530107,2.277105536,2.985185404,0.894715491,
                       2.818549509,0.655925982,0.593539368,1.635208395,1.761483237,
                       1.633056952,1.754436206,0.908721275,2.433869445,
                       0.95246871,1.602203378,5.42440701,0.556269555),
               bmi = c(27.7,25.6,26.1,26.6,26.9,
                       25.2,28,26.2,26.6,26.5,25.5,27.3,29.6,25.9,25.9,
                       25,27.1,27.3,23.1,21.8,27.5,25.6,22.7,28,25.6,
                       26.7,23.2,26.7,25.6,26.9,26.2,28.5,26,27.9,26.6,
                       28.9,27.3),
      hypertension = c(22.6,21,17.5,23.3,13.2,18,
                       20.9,19.2,19.9,21.5,25.1,17.9,25,19.2,19.4,22,
                       15.2,30,23.8,25.8,19.7,21.2,17.6,19.7,18.7,13.7,
                       22.6,28.7,24.4,30,27.2,23.3,19.3,20.3,27.1,12.9,
                       26.9),
           smoking = c(21.8,29.6,28.2,13.9,14.3,
                       25.7,37.8,9,30.6,13.7,15.6,7.1,25.2,29.3,20.4,
                       32.7,22.3,30.6,39.4,11.5,24.3,23.7,22.1,14,25.8,
                       4.8,24.3,28,22.7,29.7,39.3,15.6,18.8,27.2,28.9,
                       21.8,20.3)
    )


e<-data.frame(
     Hospital_beds = c(50L,76L,62L,22L,27L,47L,
                       22L,15L,83L,16L,19L,15L,16L,30L,44L,65L,28L,70L,
                       12L,7L,28L,34L,134L,15L,47L,16L,5L,65L,34L,
                       63L,82L,27L,26L,27L,88L,29L,28L),
         WHO_index = c(0.722,0.959,0.915,0.573,
                       0.881,0.916,0.87,0.91,0.902,0.789,0.701,0.619,0.752,
                       0.972,0.881,0.994,0.925,0.743,0.66,0.617,0.924,
                       0.991,0.957,0.755,0.928,0.547,0.755,0.793,0.945,
                       0.645,0.544,0.894,0.908,0.734,0.708,0.838,0.319),
         Urban_pop = c(91,65.9,97.6,84.3,80.9,
                       73.7,88.6,75,74.3,73.8,67.5,62.7,43,78.4,83.6,78.3,
                       81.3,68.9,49.9,30.9,61.8,68.3,90.5,77.8,87.1,
                       76.9,45.3,60.9,60.6,53.8,73.7,82.1,85.1,70.7,68.7,
                       80.8,62.2),
       Gdp_per_cap = c(2.061056855,5.545468929,
                       5.140799834,1.609640096,4.813025597,6.806094105,
                       2.522252778,1.501293027,5.307454012,1.774818532,1.548178762,
                       1.173438739,1.24123094,3.971543906,4.841693603,
                       4.534239574,4.59735735,3.110250275,1.30796193,0.776288177,
                       8.320339468,4.183042633,4.279745852,1.984464567,
                       5.632894114,1.441807067,0.895108565,3.13366035,3.34154379,
                       2.820635705,2.758812544,5.533567959,5.320888436,
                       2.806885941,0.924946213,6.279458565,1.368688236)
   )


f<- data.frame(
      Covid_tests = c(4.453,54.934,85.212,NA,51.14,
                      49.728,37.706,8.184,NA,9.043,NA,4.579,NA,NA,
                      38.309,NA,59.799,21.792,1.003,3.46,74.011,70.518,2.376,
                      2.888,NA,6.087,3.805,25.724,94.2,26.309,90.826,
                      29.844,NA,28.195,9.857,70.074,15.901),
         BCG_vacc = c(93.69999695,22.5,0,88.55000305,
                      0,0,95.90000153,86.59999847,0,80.97499847,
                      84.65000153,93.94999695,88.22499847,0,64,56.82500076,0,99,
                      79.05000305,63.47499847,58.22499847,0,59.25,
                      85.02500153,0,84.17500305,85.07499695,94.30000305,75.25,
                      82.625,66.42500305,89.07499695,17.22500038,86,62.79999924,
                      0,74.05000305),
        UVR_level = c(3476L,1888L,1645L,4552L,1887L,
                      2158L,3982L,5385L,1812L,4880L,3253L,4929L,4202L,
                      2705L,1494L,1907L,1576L,1932L,5220L,4514L,1509L,
                      2444L,2521L,4974L,1662L,5906L,4928L,1749L,2585L,
                      2071L,1795L,5384L,1587L,2924L,1843L,2736L,4111L),
       stringency = c(93.48109436,85.19000244,
                      74.07343292,76.73246765,71.65020752,73.24888611,73.15000153,
                      87.03500366,68.25333405,89.80999756,77.63316345,
                      93.51999664,83.8278656,70.95444489,59.09785461,74.44837952,
                      56.70891953,75.38137817,68.05999756,89.49436188,
                      88.42713928,82.60578918,44.48974991,81.5681839,73.73036957,
                      93.75784302,96.26882172,78.28689575,83.40107727,
                      85.55750275,82.5987854,88.22161865,36.85228729,75.08468628,
                      92.20458221,58.52120209,85)
    )

aberdeen <- cbind(a, b,c , d, e, f)

#Table 2 univariable analysis

aberdeen %>% 
  lm(data =.,  log_mean_mort~arrivals_mil) %>% 
  summary(.)

#Table 2 multivariable analysis

aberdeen %>% 
  lm(data =.,  log_mean_mort~log_pop+over_65+Pollution.levels+Mean_temp+arrivals_mil+Pop_dens+neoplasms+hypertension+
       WHO_index+Urban_pop+Gdp_per_cap+UVR_level+BCG_vacc+stringency) %>% 
  summary(.)


# Dividing deaths and flights by population 

aberdeen <-  aberdeen %>% 
  mutate(deaths_per_cap = mean_mort / pop,
         log_deaths_per_cap = log(deaths_per_cap),
         flights_per_cap = arrivals_mil / pop,
         log_flights_per_cap = log(flights_per_cap))

aberdeen <- aberdeen %>% 
  mutate(region = case_when(Iso3 %in% c("ARG", "BRA", "ECU", "CHL", "COL", "PER") ~ "South America",
                            Iso3 %in% c("AUT", "BEL", "CHE", "DEU", "ESP", "FIN", "FRA", "GBR", "HUN",
                                        "IRL", "ITA", "NLD", "POL", "PRT", "ROU", "SWE", "UKR", "RUS") ~"Europe",
                            Iso3 %in% c("CAN", "DOM", "MEX", "USA") ~ "North America",
                            Iso3 %in% c("DZA", "EGY", "ZAF") ~ "Africa",
                            Iso3 %in% c("JPN", "IDN", "IND", "TUR", "SAU", "PHL") ~ "Asia"),
         gdp_quartile= ntile(Gdp_per_cap, 4)
  )

#Table 2 univariable analysis with adjusted variables

aberdeen %>% 
  lm(data =.,  log_deaths_per_cap ~flights_per_cap) %>% 
  summary(.)

aberdeen %>% 
  filter(region=="Europe") %>% 
  ggplot(aes(x = flights_per_cap, y = log_deaths_per_cap, label = Iso3))+
  geom_text()+
  geom_smooth(method = "lm")+
  theme_classic()+
  labs(x = "Flights per capita", y = "Log deaths per capita (daily)")



aberdeen %>% 
  ggplot(aes(x = flights_per_cap, y = log_deaths_per_cap, label = Iso3))+
  geom_text()+
  geom_smooth(method = "lm")+
  theme_classic()+
  facet_wrap(~region)

aberdeen %>% 
  ggplot(aes(x = flights_per_cap, y = log_deaths_per_cap, label = Iso3))+
  geom_text()+
  geom_smooth(method = "lm")+
  theme_classic()+
  facet_wrap(~gdp_quartile)

# Trivariate Analysis

aberdeen %>% 
  lm(data =.,  log_deaths_per_cap ~flights_per_cap+region) %>% 
  summary(.)

aberdeen %>% 
  filter(region=="Europe") %>% 
  lm(data =.,  log_deaths_per_cap ~flights_per_cap) %>% 
  summary(.)

aberdeen %>% 
  lm(data =.,  log_deaths_per_cap ~flights_per_cap+Gdp_per_cap) %>% 
  summary(.)

#Table 2 multivariable analysis

aberdeen %>% 
  lm(data =.,  log_deaths_per_cap ~ log_pop+over_65+Pollution.levels+Mean_temp+flights_per_cap+Pop_dens+neoplasms+hypertension+
       WHO_index+Urban_pop+Gdp_per_cap+UVR_level+BCG_vacc+stringency) %>% 
  summary(.)


#Table 2 univariable analysis with adjusted variables (logged)

aberdeen %>% 
  lm(data =.,  log_deaths_per_cap ~log_flights_per_cap) %>% 
  summary(.)

#Table 2 multivariable analysis (logged)


aberdeen %>% 
  lm(data =.,  log_deaths_per_cap ~ log_pop+over_65+Pollution.levels+Mean_temp+log_flights_per_cap+Pop_dens+neoplasms+hypertension+
       WHO_index+Urban_pop+Gdp_per_cap+UVR_level+BCG_vacc+stringency) %>% 
  summary(.)


# To make a residualized plot (slightly hacky but gets correct estimate)


no_flights <- aberdeen %>% 
  lm(data =.,  log_deaths_per_cap ~ log_pop+over_65+Pollution.levels+Mean_temp+Pop_dens+neoplasms+hypertension+
       WHO_index+Urban_pop+Gdp_per_cap+UVR_level+BCG_vacc+stringency) %>% 
  augment()

no_flights <-  no_flights %>% 
  select(.resid) %>% 
  rename(resid_deaths = .resid)



flights_aug <- aberdeen %>% 
  lm(data =.,  flights_per_cap ~ log_pop+over_65+Pollution.levels+Mean_temp+Pop_dens+neoplasms+hypertension+
       WHO_index+Urban_pop+Gdp_per_cap+UVR_level+BCG_vacc+stringency) %>% 
  augment()

flights <- flights_aug %>% 
  select(.resid) %>% 
  rename(resid_flights = .resid)

aberdeen_plus <- cbind(aberdeen, no_flights, flights)

aberdeen_plus %>% 
  lm(data = ., resid_deaths ~ resid_flights) %>% 
  summary()

aberdeen_plus %>% 
  ggplot(aes(x = resid_flights, y = resid_deaths, label = Iso3))+
  geom_text()+
  geom_smooth(method = "lm")+
  theme_classic()+
  labs(x = "Flights per capita (residualized)", y = "Log deaths per capita (residualized")
  
