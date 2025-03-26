install.packages("ggmap")
install.packages("leaflet")
library(ggmap)
library(leaflet)

register_google(key = "AIzaSyBfb5FFo7YHt5AdmwNSDTEe9KLLHK2oe74")

df <- read_csv('Data/wide_income_rent.csv')
View(df)

pivot_longer(cols = everything(),
             names_to = 'Measure',
             values_to = 'Value') %>% 
  View()

dat_long %>% 
  pivot_wider(names_from = 'Measure',
              values_from = 'Value') %>% 
  View()

library(tidyverse)

df %>% 
  pivot_longer(cols = everything(),
             names_to = 'variable',
             values_to = ) %>% 
  View()

df %>% 
  pivot_longer(cols = -variable,#elongates everything besides variable column
               names_to = 'State',
               values_to = 'Value') %>%#need to make a new column called "state" so I need to use pivot_longer
  pivot_wider(names_from = 'variable',
              values_from = 'Value') %>% #takes everything in variable column and separates them into their own columns.
  ggplot(aes(x = rent, y = income))+
  geom_point()+
  geom_text(aes(label = State))

table1
table2

table2.0 <- table2 %>% 
  pivot_wider(names_from = 'type',
              values_from = 'count')
table2.0

table3#rate column is chr and ugly
table3.0 <- table3 %>% 
  separate(rate, c('col1', 'col2'))
table3.0#prettier as 2 separate columns rather than a / symbol

table4a
table4b#want table4a/b to be combined to look like table1; make the tables tidy like table1.
table1

table4.0a <- table4a %>% 
  pivot_longer(cols = -country,
               names_to = "year",#1999 and 2000 years put into a new column called "year"
               values_to = "cases")#all data put into a new column called "cases"
table4.0a

table4.0b <- table4b %>% 
  pivot_longer(cols = -country,
               names_to = "year",
               values_to = "population")
table4.0b

table4_tidy <- full_join(table4.0a, table4.0b)#combines 2 full tables. Joins columns that are the same.
#This is what it did: Joining with `by = join_by(country, year)`

table5.0 <- table5 %>% #combine century and year, then separate rate into 2 columns.
  mutate(year = paste0(table5$century, table5$year)) %>% #'sep =' adds something between the things you're adding together.
  separate(rate, c('cases', 'population'), convert = T) %>% 
  select(-century) %>% #selects a column for removal.
  View()#columns 'cases' and 'population' are chr, and I need to convert them to numeric, so I added 'convert = T' to the separate function.
table5.0#now cases and population columns are integers.

#enter data to excel (or Google Sheet)
getwd()
#path: Users/winni/Data_Course_ALLRED/Data/Exercises/Data_Entry_Case_Study.txt
text <- read.delim('Exercises/Data_Entry_Case_Study.txt')

install.packages('readxl')
library(readxl)#allows you to read excel sheets and choose worksheets.
pat_dat <- read_xlsx('Data/messy_bp.xlsx', skip = 3)#skip tells it how many rows to leave out, so irrelevant rows are left out.'sheet =' lets you choose which sheet to work in.
View(pat_dat)
#separate the BP columns into systolic and dyastolic. 
#Also fix the race columns so multiple terms for 'white' are not included. 
#Create a new column called "visit" and have the data input be 1, 2, and 3.

library(tidyverse)
library(readxl)
library(dplyr)
library(tidyr)

pat_dat %>%
  uncount(3) %>%#causes each row to repeat 3 times so each patient has data for all three visits.
  mutate(visit = rep(1:3, length.out = nrow(.)), 
         Blood_Pressure = case_when(visit == 1 ~ BP...8,
                                    visit == 2 ~ BP...10,
                                    visit == 3 ~ BP...12),
         Heart_Rate = case_when(visit == 1 ~ HR...9,
                                visit == 2 ~ HR...11,
                                visit == 3 ~ HR...13)) %>% 
  select(-BP...8, 
         -BP...10, 
         -BP...12, 
         -HR...9, 
         -HR...11, 
         -HR...13) %>% 
  separate(Blood_Pressure, c('Systolic', 'diastolic')) %>% #this created 2 new columns that replaced Blood_pressure.
  View()#this fixed the visit, blood pressure, and heart rate columns.
#the data has incorrect patient IDs and needs the white, WHITE, caucasion entries standardized. 
  
#alternative way to do the same thing, using pivot_longer
bp_dat <- pat_dat %>% 
  pivot_longer(starts_with('BP'),#for all columns that start with BP,
               names_to = 'visit',#create a column titled visit (contains BP...8, BP...10, BP...12 on repeat)
               values_to = 'bp') %>% #and put the data into a new column titled 'bp'.
  mutate(visit = case_when(visit == 'BP...8' ~ 1,
                           visit == 'BP...10' ~ 2,
                           visit == 'BP...12' ~ 3)) %>% 
  separate(bp, into = c('systolic', 'diastolic')) %>% #replaces bp with 2 other columns titled 'systolic' and 'diastolic.'
  View()
hr_dat <- pat_dat %>% 
  pivot_longer(starts_with('HR'),
               names_to = 'visit',
               values_to = 'hr') %>% 
  mutate(visit = case_when(visit == 'HR...9' ~ 1,
                           visit == 'HR...11' ~ 2,
                           visit == 'HR...13' ~ 3)) %>% 
  View()#I did something wrong when creating the objects.

install.packages('janitor')
library(janitor)

clean_names()
make_clean_names()#2 useful functions in this package.

make_clean_names('# of bacteria')#"number_of_bacteria"
make_clean_names('% of bacteria')#"percent_of_bacteria"

