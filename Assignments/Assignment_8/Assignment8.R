library(readr)
library(tidyverse)
library(easystats)
library(ggplot2)

dat <- read_csv('mushroom_growth.csv')

glimpse(dat)

dat %>% 
  ggplot(aes(x = factor(Temperature), y = GrowthRate, fill = Humidity))+
  geom_boxplot()+
  theme_minimal()
#Higher humidity seems to have a wider range and more outliers
dat %>% 
  ggplot(aes(x = factor(Temperature), y = GrowthRate, fill = Species))+
  geom_boxplot()+
  theme_minimal()
#P. cornucopiae species also has wider range and more outliers.

dat %>% 
  ggplot(aes(x = Light, y = GrowthRate))+
  geom_point()+
  facet_wrap(~Humidity)+
  geom_smooth(se = F)+
  theme_minimal()


mod1 <- glm(data = dat,
            formula = GrowthRate ~ Temperature)
summary(mod1)

mod2 <- glm(data = dat,
            formula = GrowthRate ~ Temperature*Humidity)
summary(mod2)

mod3 <- glm(data = dat,
            formula = GrowthRate ~ Light)
summary(mod3)

mod4 <- glm(data = dat,
            formula = GrowthRate ~ Light*Humidity)
summary(mod4)

compare_performance(mod1, mod2, mod3, mod4) %>% 
  plot()
#model 4 seems the best

pred1 <- predict(mod1)
pred1
actual <- dat$GrowthRate
actual

mse1 <- mean((actual - pred1)^2)
print(mse1)
#9636.742

pred2 <- predict(mod2)
pred2

mse2 <- mean((actual - pred2)^2)
print(mse2)
#7617.563

pred3 <- predict(mod3)
pred3
mse3 <- mean((actual - pred3)^2)
print(mse3)
#7702.834

pred4 <- predict(mod4)
pred4
mse4 <- mean((actual - pred4)^2)
print(mse4)
#5525.948
#model 4 has the lowest mse; this indicates that it's the best one.

dat$pred <- pred4
View(dat)

dat %>% 
  ggplot(aes(x = pred, y = GrowthRate))+
  geom_point(alpha = 0.5)+
  labs(title = "Predicted vs. Actual Growth Rate",
       x = "Predicted Growth Rate",
       y = "Actual Growth Rate")+
  theme_minimal()

#Based on the ‘Predicted vs Actual Growth Rate’ plot, there are no obvious scientifically meaningless predictions. For example, none of the predicted growth rates are negative. 
#However, the real data shows max values in the 600 range, whereas the model never predicts values greater than the 200s. 
#This  means that the model seems to consistently underpredict high actual values. This is a limitation, but it does not make predictions scientifically meaningless. 
