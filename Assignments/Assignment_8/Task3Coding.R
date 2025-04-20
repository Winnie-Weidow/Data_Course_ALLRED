#swept environment prior to running code from .txt for task 3.
library(tidyverse)
library(readr)

dat <- read_csv("non_linear_relationship.csv")
View(dat)

mod <- lm(data = dat,
          formula = response ~ poly(predictor, 5, raw = T))
summary(mod)

pred <- predict(mod)
pred

modelPerformance = data.frame(RMSE = RMSE(pred, dat$response),
                              R2 = R2(pred, dat$response))
print(lm(response ~ predictor + I(predictor^2), data = dat))
print(modelPerformance)

dat %>% 
  ggplot(aes(x = predictor, y = response))+
  geom_point()+
  stat_smooth(method = lm, formula = y ~ poly(x, 5, raw = T))