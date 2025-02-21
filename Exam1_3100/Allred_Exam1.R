#Task 1####
getwd()
covid_data <- read.csv('data/cleaned_covid_data.csv')
View(covid_data)#just for my reference.

##Task 2####
library(tidyverse)
str(covid_data)
A_states <- covid_data %>% 
  filter(grepl('^A', Province_State))

head(A_states)
dim(A_states)#2624 7

###Task 3####
library(ggplot2)

A_states %>% 
  ggplot(aes(x = Last_Update,
             y = Deaths,
             color = Province_State))+
  geom_point()+
  geom_smooth(method = 'loess', se = FALSE)+
  facet_wrap(~ Province_State, scales = 'free_y')+
  labs(title = 'Deaths Over Time by State',
       x = 'Date',
       y = 'Number of Deaths')+
  theme_minimal()

####Task 4####
state_max_fatality_rate <- covid_data %>% 
  group_by(Province_State) %>% 
  summarize(Maximum_Fatality_Ratio = max(Case_Fatality_Ratio, na.rm = TRUE)) %>% 
  arrange(desc(Maximum_Fatality_Ratio))
View(state_max_fatality_rate)

#####Task 5####
state_max_fatality_rate %>% 
  mutate(Province_State = as.factor(Province_State)) %>% 
  ggplot(aes(x = Province_State,
             y = Maximum_Fatality_Ratio))+
  geom_bar(stat = 'identity')+
  labs(title = 'Maximum Fatality Ratios by State',
       x = 'State',
       y = 'Maximum Fatality Ratio')+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

######Task 6####
cumulative_deaths <- covid_data %>% 
  group_by(Last_Update) %>% 
  summarize(Total_Deaths = sum(Deaths, na.rm = TRUE)) %>% 
  mutate(Cumulative_Deaths = cumsum(Total_Deaths)) %>% 
  ggplot(aes(x = Last_Update,
             y = Cumulative_Deaths))+
  geom_point()+
  labs(title = 'Covid Deaths in the States/Provinces',
       x = 'Time',
       y = 'Cumulative Deaths')+
  theme_minimal()
cumulative_deaths
