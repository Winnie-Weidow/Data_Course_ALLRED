#Library####
#need to make sure dplyr and tidyr are not active for janitor to function properly.
library(tidyverse)
library(janitor)

##Load Dat####
dat <- read_csv('unicef-u5mr.csv')

###Tidy Dat####
names_dat <- dat %>% 
  clean_names()
View(names_dat)

library(tidyr)
library(dplyr)

names(names_dat)

year_dat <- names_dat %>% 
  pivot_longer(cols = starts_with('u5mr_'),
               names_to = 'year',
               names_pattern = 'u5mr_(\\d+)',
               values_to = 'u5mr',
               values_drop_na = TRUE) %>% #the result was a lot of rows that did not have u5mr data associated with the year. Removed rows with NA values.
  mutate(year = as.numeric(year))
View(year_dat)
str(year_dat)

####Line Plots####

library(ggplot2)

line_plot <- year_dat %>% 
  ggplot(aes(x = year, y = u5mr, group = country_name))+
  geom_line()+
  facet_wrap(~ continent)+
  labs(title = 'Country U5MRs Over Time',
       x = 'Year',
       y = 'U5MR')+
  theme_bw()+
  scale_x_continuous(breaks = seq(1960, 2000, by = 20),
                     labels = seq(1960, 2000, by = 20))

line_plot

ggsave("ALLRED_Plot_1.png", plot = line_plot)

plot_data <- year_dat %>% 
  group_by(year, continent) %>% 
  summarize(mean_u5mr = mean(u5mr), .groups = 'drop') 
  
line_plot2 <- plot_data %>% 
  ggplot(aes(x = year, y = mean_u5mr,
             color = continent))+
  geom_line(size = 1.5)+
  theme_minimal()+
  labs(x = 'Year',
       y = 'Mean U5MR',
       legend = 'Continent')

line_plot2

ggsave("ALLRED_Plot_2.png", plot = line_plot2,
       width = 6, height = 4, dpi = 300)

#####Models####

mod1 <- glm(data = year_dat,
            formula = u5mr ~ year)
summary(mod1)
#excellent p-value (<2e-16)

mod2 <- glm(data = year_dat,
            formula = u5mr ~ year + continent)
summary(mod2)
#excellent p-values again (<2e-16)

mod3 <- glm(data = year_dat,
                    formula = u5mr ~ year * continent)
#year*continent = year + continent + year:continent
summary(mod3)
#year:continentAsia       1.767   0.0773
#this is the only one with a p-value > 0.05

######Compare Models####

library(easystats)

compare_models(mod1, mod2, mod3)
#mod1: intercept is 4639.10; each year, u5mr decreases by ~2.29. Confidence interval (  -2.37,   -2.21) indicates statistical significance.
#mod2: intercept is 4625.17; each year, u5mr decreases by ~2.25. Confidence interval is similar to mod1. Accounts for continent baseline differences, but not the trend over time by continent.
#mod3: intercept is 6388.42 (lots higher); each year, u5mr decreases by ~3.13. Shows that u5mr declines at different rates, depending on the continent. 

#mod3 seems to be the best model; it shows the interaction effects and how u5mr trends vary by continent and how the declines also vary by continent.

compare_performance(mod1, mod2, mod3) %>% 
  plot()
#mod3 seems to be the best model; the plot from the performance comparison shows that mod3 aligns with more evaluation metrics.

#######Plot Models####

library(broom)

pred1 <- augment(mod1, newdata = year_dat) %>%
  mutate(model = "mod1")
pred2 <- augment(mod2, newdata = year_dat) %>%
  mutate(model = "mod2")
pred3 <- augment(mod3, newdata = year_dat) %>%
  mutate(model = "mod3")

pred_dat <- bind_rows(pred1, pred2, pred3) %>%
  rename(predicted_u5mr = .fitted)

plot_3 <- pred_dat %>% 
  ggplot(aes(x = year,
             y = predicted_u5mr,
             color = continent))+
  geom_line(size = 1)+
  labs(title = 'Model predictions',
       x = 'Year',
       y = 'Predicted U5MR',
       legend = 'Continent')+
  facet_wrap(~ model)+
  theme_bw()

plot_3

ggsave('ALLRED_plot_3.png', plot = plot_3,
       width = 6, height = 4, dpi = 72)