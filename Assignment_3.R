# 1.  Get a subset of the "iris" data frame where it's just even-numbered rows
library(tidyverse)
View(iris)
summary(iris)
#Sepal.Length    Sepal.Width     Petal.Length    Petal.Width          Species  
#Min.   :4.300   Min.   :2.000   Min.   :1.000   Min.   :0.100   setosa    :50  
#1st Qu.:5.100   1st Qu.:2.800   1st Qu.:1.600   1st Qu.:0.300   versicolor:50  
#Median :5.800   Median :3.000   Median :4.350   Median :1.300   virginica :50  
#Mean   :5.843   Mean   :3.057   Mean   :3.758   Mean   :1.199                  
#3rd Qu.:6.400   3rd Qu.:3.300   3rd Qu.:5.100   3rd Qu.:1.800                  
#Max.   :7.900   Max.   :4.400   Max.   :6.900   Max.   :2.500                  
dim(iris)#150 5
class(iris)#"data.frame"

even_rows <- iris[seq(2, nrow(iris), by = 2), ]
#object now called "even_rows". data frame is iris; in brackets, rows are specified by the sequence command. start with the second row, through n amount of rows, by increments of 2. Columns are shown as-is.
View(even_rows)
dim(even_rows)#75 5
even_rows_2 <- iris[seq(2,150,2), ]#a second, easier method of doing the same thing.
View(even_rows_2)
dim(even_rows_2)#75 5


seq(2,150,2) # here's the code to get a list of the even numbers between 2 and 150



# 2.  Create a new object called iris_chr which is a copy of iris, except where every column is a character class
iris_chr <- as.data.frame(lapply(iris, as.character))
is.character(iris_chr$Sepal.Length)#TRUE

str(iris_chr)
View(iris_chr)


# 3.  Create a new numeric vector object named "Sepal.Area" which is the product of Sepal.Length and Sepal.Width
Sepal.Area <- iris$Sepal.Length * iris$Sepal.Width


# 4.  Add Sepal.Area to the iris data frame as a new column
iris_chr <- iris %>% 
  mutate(SepalArea = Sepal.Area) %>% 
  View()


# 5.  Create a new dataframe that is a subset of iris using only rows where Sepal.Area is greater than 20 
# (name it big_area_iris)
big_area_iris <- iris %>% 
  filter(Sepal.Area > 20) %>% 
  mutate(Sepal_Area = Sepal.Length*Sepal.Width) %>% 
  View()

# 6.  Upload the last numbered section of this R script (with all answers filled in and tasks completed) 
# to canvas
# I should be able to run your R script and get all the right objects generated