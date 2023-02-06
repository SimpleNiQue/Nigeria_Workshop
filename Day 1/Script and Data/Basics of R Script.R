# Module 1: What is R?


# Creating Data in R ------------------------------------------------------

nation <- c("DRC", "Egypt", "Ethiopia", "Nigeria", "SA")
popSize <- c(71, 87, 90, 183, 54)
writers <- c(22, 34, 16, 130, 145)

nation
popSize
writers

class(nation)
class(popSize)
class(writers)

mixed <- c(45, "Egypt", 56, "Nigeria", "SA")
class(mixed)


# Objects, functions and Operators ----------------------------------------

x <- c(1, 2, 3, 4, 5)
x

y <- c(2, 1, 2, 3, 1)
x + y

# compute the mean of the vector x
mean(x) 

# 'sqrt' is the name of the square root function, and 100 is the argument
sqrt(100)  

# This results in an error (console message). 1 argument is required
sqrt()  

# Store the return value of a function
test_object <- sqrt(144) 
test_object

# omitting the second argument uses the default
round(2.71828) 

# multiple arguments are separated by comma
round(2.71828, 2)  

round(2.71828, 4)

#Quick Exercise
#a. Using the function 'rnorm', generate a set of 100 random numbers with mean 5 and standard deviation 3.
#b. Generate another set of 1000 random numbers, using the mean and standard deviation of 0 and 1, respectively (standard normal).
#c. Calculate the mean of the distributions generated in a and b above after trimming off 5% of the data.


# Data Types --------------------------------------------------------------

#vectors
#These variables (popSize and nation) are called vectors

nation <- c("DRC", "Egypt", "Ethiopia", "Nigeria", "SA")
popSize <- c(71, 87, 90, 183, 54)

#We can get the first element of the object nation using an index []
nation[1]

#multiple element
nation[1:3]


#Matrix
#Two dimensional objects

m <- matrix(1:10, nrow=5, ncol=2)
m
#We can subset matrix using an indexing of the rows and columns
m[4,] # rows
m[, 2] #columns

#we can combine multiple vectors into a matrix
Africa <- cbind(popSize, nation)
dim(Africa)
Africa

#How many rows and columns does the object Africa have?

#Dataframes
df <- data.frame(Country = c("DRC", "Egypt", "Ethiopia", "Nigeria", "SA"),
                 Population = c(71, 87, 90, 183, 54), 
                 Writers = c(22, 34, 16, 130, 145))


df
#or you can use view() to see the dataframe we have created
View(df)

#We can use the operator "$" to select a variable, or an item within a dataframe
df$Country

### Lists

out <- list(1,2,"three")

out[1]
out[[1]]

class(out[1])
class(out[[1]])

#structure of the list
str(out)

#We can also name each element of the list
y <- list(person="Mike", gender="M", company="ProgramCreek")
y


# Logical Operators -------------------------------------------------------
#Using the df dataframe we want to create another variable Area by adding 10 to the population

df$Area <- df$Population+10

df

#Quick exercise
#Create another variable called Pop_density = Population/Area

df$Pop_density <- df$Population/df$Area
df

#find out if country has a population density of more than 10

df$Pop_density >10

#Find out which countries have population density greater than 0.5 and more than 50 writers
df$Pop_density >0.5 & df$Writers>50

#Find out which countries have population density greater than 0.5 OR more than 50 writers
df$Pop_density >0.5 | df$Writers>50

# Data Handling -----------------------------------------------------------
#R packages/libraries

install.packages('tidyverse')
library(tidyverse)

install.packages("tmap")
library(tmap)

install.packages("leaflet")
library(leaflet)


install.packages("sf")
library(sf)

install.packages("raster")
library(raster)

#Setting working directory and Loading data

#we can use setwd() to choose the directory where our data is located

setwd("C:/Users/oy1r22/Documents/WorldPop Project/Nigeria Workshop/Spatial Data")

#Alternatively if our dataset is located in different directories or subfolders
#We can create different path to access the data

#Specify Drive Path
drive_path <- "C:/Users/oy1r22/Documents/WorldPop Project/Nigeria Workshop/"
input_path <- paste0(drive_path, "Spatial Data/")


#Loading data

fake_data <- read.csv(paste0(input_path, "fake_data.csv"))

#View data
View(fake_data)

#structure of variables
str(fake_data)

#dimension
dim(fake_data)

#first 6 rows of data
head(fake_data)

#last 6 rows of data
tail(fake_data)

### Finding Help

?head

##########################################################################################################
###########################################################################################################
##########################################################################################################
############### Manipulating Data & Visualizations#######################################################

# Manipulating and Managing Data


# Basic Data Manipulation -------------------------------------------------


#Let us load population data.
Nigeria_Pop <- read.csv(paste0(input_path, "population.csv"))

#Let view the data
view(Nigeria_Pop)


#Let look at the structure of the data
str(Nigeria_Pop)


#What are the names of the column headings?
names(Nigeria_Pop)


#Sub-Setting/Slicing a variable
#We want to subset states with a population of less 2million people

Nigeria_Pop[Nigeria_Pop$total < 2000000, ]

#Slice data by index. We want to select the first 10 rows of our population data
Nigeria_Pop[1:10, ]


#Summary statistics of data
summary(Nigeria_Pop)

#We can also summarize using these statistics
mean(Nigeria_Pop$total)
median(Nigeria_Pop$total)
quantile(Nigeria_Pop$total)
min(Nigeria_Pop$total)
max(Nigeria_Pop$total)


#Let create another column called monthly_temperature = temperature_mean/12
Nigeria_Pop$monthly_temperature <- Nigeria_Pop$temperature_mean/12
view(Nigeria_Pop)

#Dealing with NA Values
sum(Nigeria_Pop$gdp)

#You realize summing the gdp return NA. This is because of the NA values in the data
##To overide NA we use the function na.rm = T

#sum element
sum(Nigeria_Pop$gdp, na.rm = T)


# Advance Data Manipulation using Tidyverse -------------------------------

library(tidyverse)

#subset rows based on certain condition.
#We are subsetting observations with a population of more than 10 million people
Nigeria_Pop %>% 
  filter(total >= 10000000)

#Select certain variables
#we are selecting the variable state and total
Nigeria_Pop %>% 
  select(statename, total)

#Grouping observations based on a categorical variable
#We will group the observations by region and summarise their population per region

Nigeria_Pop %>% 
  group_by(region) %>% 
  summarise(regional_population = sum(total))

#The mutate() function is used to add another variable to our data frame
#We want to add another variable called density = total/buildings

Nigeria_Pop <- Nigeria_Pop %>% 
  mutate(density = total/buildings)

#the rename() can be used to change the name of a variable
#We want to rename density to population_density

Nigeria_Pop<- Nigeria_Pop %>% 
  rename(Pop_density = density)

#if you want to save the data as a new csv document, you can use the write.csv function
write.csv(Nigeria_Pop, paste0(input_path, 'New Nigeria Population.csv'))

# Data Visualization ------------------------------------------------------

#Barplot
barplot(Nigeria_Pop$total)

#We can also use ggplot to make nice plots
ggplot(Nigeria_Pop, aes(x = statename, y = total, fill = total)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "yellow", high = "red") +
  labs(x = "States", y = "Population Total", fill = "total") +
  ggtitle("Bar Plot of Population for the States")


#Boxplot
boxplot(Nigeria_Pop$total)

# plot the boxplot using ggplot
  ggplot(Nigeria_Pop, aes(y = total)) +
  geom_boxplot() +
  labs(x = " ", y = "Population") +
  ggtitle("Box Plot of Population")


#Histogram plot
  hist(Nigeria_Pop$total)
  
  # plot the histogram using ggplot
  ggplot(Nigeria_Pop, aes(x = total, fill = "blue")) +
    geom_histogram(bins = 20, color = "white") +
    scale_fill_manual(values = "blue") +
    labs(x = "Population", y = "Frequency") +
    ggtitle("Histogram")

  
#Density plot
plot(density(Nigeria_Pop$total))

# plot the density plot using ggplot
ggplot(Nigeria_Pop, aes(x = total, color = "blue")) +
  geom_density(fill = "blue", alpha = 0.2) +
  scale_color_manual(values = "blue") +
  labs(x = "Population", color = "Density") +
  ggtitle("Density Plot")
  
#Scatter plot of temperature and rainfall
plot(Nigeria_Pop$precipitation_mean, Nigeria_Pop$temperature_mean)

# plot the scatter plot and fit a line using ggplot
ggplot(Nigeria_Pop, aes(x = temperature_mean, y = precipitation_mean)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "Temperature", y = "Rainfall") +
  ggtitle("Scatter Plot of Temperature and Rainfall")


##############END########################################################

