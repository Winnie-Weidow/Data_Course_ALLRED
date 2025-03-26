#Library####
library(tidyverse)
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(janitor)
library(gganimate)

##Task 1####
dat1 <- read.csv("Data/BioLog_Plate_Data.csv")

names(dat1)
#"Sample ID" "Rep"       "Well"      "Dilution"  "Substrate" "Hr_24"     "Hr_48"   "Hr_144"
time_dat <- dat1 %>% 
  pivot_longer(cols = starts_with('Hr'),
               names_to = 'time',
               values_to = 'absorbance') %>% 
  mutate(time = as.numeric(str_remove(time, 'Hr_')))

View(time_dat)

str(time_dat)
###Task 2####

#"Clear_Creek" "Soil_1"      "Soil_2"      "Waste_Water"
#0.001 0.010 0.100

type_dat <- time_dat %>% 
  mutate(type = case_when(Sample.ID %in% c("Clear_Creek", "Waste_Water") ~ 'water',
                          Sample.ID %in% c("Soil_1", "Soil_2") ~ 'soil'))
View(type_dat)

####Task 3####

dil_filter_dat <- type_dat %>% 
  filter(Dilution == 0.1)
               
dil_plot <- dil_filter_dat %>%
  ggplot(aes(x = time,
             y = absorbance,
             color = type))+
  geom_smooth(se = FALSE)+
  facet_wrap(~ Substrate)+
  labs(title = 'Just dilution 0.1',
       x = 'Time',
       y = 'Absorbance',
       color = 'Type')+
  theme_minimal()

dil_plot

#####Task 4####
itaconic_dat <- type_dat %>% 
  filter(Substrate == 'Itaconic Acid')

mean_absorb_anim <- type_dat %>% 
  group_by(Sample.ID, Dilution, time) %>% 
  summarize(mean_abs = mean(absorbance)) %>% 
  ggplot(aes(x = time,
             y = mean_abs,
             color = Sample.ID))+
  geom_line()+
  facet_wrap(~ Dilution)+
  labs(x = "Time",
       y = "Mean_absorbance",
       color = 'Sample ID')+
  theme_minimal()+
  transition_reveal(time)

mean_absorb_anim



           
           