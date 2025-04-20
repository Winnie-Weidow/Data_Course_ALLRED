#Load Libraries####

library(tidyverse)
library(readr)
library(easystats)
library(ggplot2)
library(GGally)
library(modelr)


##Load Data####

grad_dat <- read_csv("GradSchool_Admissions.csv")

ggpairs(grad_dat)

View(grad_dat)
names(grad_dat)

###Visualize Data####

grad_dat %>% 
  ggplot(aes(x = gre, y = gpa))+
  geom_point()+
  theme_minimal()

####Model Data####

as.logical(grad_dat$admit)

grad_mod <- glm(data = grad_dat,
                formula = as.logical(admit) ~ (gre + gpa) * rank,
                family = 'binomial')
summary(grad_mod)
#+ is additive, : is interaction, * is addative plus interaction
#main effect: gre, gpa, ranks
#interaction: gre:rank, gpa:rank

predict(grad_mod, grad_dat, type = 'response')
grad_dat$pred <- predict(grad_mod, grad_dat, type = 'response')#response turns the prediction into a probability.

View(grad_dat)

grad_mod2 <- glm(data = grad_dat,
                 formula = as.logical(admit)~ gre + gpa*rank,
                 family = 'binomial')

compare_performance(grad_mod, grad_mod2) %>% 
  plot()#grad_mod meets more criteria

grad_dat %>% 
  mutate(outcome = case_when(pred < 0.20 ~ 'Not Admit',
                             pred > 0.40 ~ 'Admit',
                             pred >= 0.20 & pred <= 0.40 ~ "Unknown")) %>% #lower limit is first quartile from summary; upper limit is from third quartile.
  mutate(accurate = case_when(admit == 1 & outcome == 'Admit' ~ TRUE,
                              admit == 0 & outcome == 'Not Admit' ~ TRUE,
                              TRUE ~ FALSE)) %>% 
  pluck('accurate') %>% 
  sum()/nrow(grad_dat)#of all the rows, how many are accurate? calculates a proportion.
#0.3575
#not very accurate

#to automatically choose best model
library(MASS)

#first, create a full model.

full_model <- glm(data = grad_dat,
                  formula = as.logical(admit) ~ gre*gpa*rank,#do all the possibilities in a full model.
                  family = 'binomial')

summary(full_model)
stepwise_mod <- stepAIC(full_model, direction = 'both')#this function checks all possibilities and chooses the best model.

#finds the smallest AIC and largest R2

stepwise_mod$formula
#as.logical(admit) ~ gre + gpa + rank + gre:gpa
#this is the formula for the best model.

best_model <- glm(data = grad_dat,
                  formula = stepwise_mod$formula,
                  family = 'binomial')

compare_performance(grad_mod, grad_mod2, best_model) %>% 
  plot()

grad_dat$pred2 <- predict(best_model, grad_dat, type = 'response')#showed all negative values; added type = 'response' and it was fixed.
View(grad_dat)

grad_dat %>% 
  mutate(outcome2 = case_when(pred2 < 0.20 ~ 'Not Admit',
                              pred2 > 0.40 ~ 'Admit',
                              pred2 >= 0.20 & pred2 <= 0.40 ~ "Unknown")) %>% #lower limit is first quartile from summary; upper limit is from third quartile.
  mutate(accurate2 = case_when(admit == 1 & outcome2 == 'Admit' ~ TRUE,
                               admit == 0 & outcome2 == 'Not Admit' ~ TRUE,
                               TRUE ~ FALSE)) %>% 
  pluck('accurate2') %>% 
  sum()/nrow(grad_dat)
#model is only a little better.
#0.37

#you can separate the data to make the model better.
library(caret)
#createDataPartition() randomly selects data for train data set. So you can train your model.

id <- createDataPartition(grad_dat$admit, p = 0.8, list = F)
dat_train <- grad_dat[id, ]
dim(dat_train)#320   6
dim(grad_dat)#400   6

dat_test <- grad_dat[-id, ]

#now I have train and test data sets. You separate the data so you can check the accuracy with other known test data.

train_mod <- glm(data = dat_train,
                 formula = stepwise_mod$formula,
                 family = 'binomial')
dat_test$pred <- predict(train_mod, dat_test, type = 'response')

View(dat_test)
