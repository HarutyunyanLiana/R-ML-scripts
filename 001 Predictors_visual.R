# Liana Harutyunyan
# Machine Learning - Homework 1

# Libraries that will be needed:
library(ISLR)
library(MASS)
library(dplyr)
library(stringi)
library(ggplot2)
library(ggcorrplot)


# get the data Auto
data("Auto")

Auto <- Auto %>%
  filter(complete.cases(.))
# we can see that data has the same amount of rows as it had before filtering it, so there are no missing values


## (a) (score = 3) Which of the predictors are quantitative, and which are qualitative?
str(Auto)
# As we can see we have one qualitative predictor, which is 'name' (the type of the column is Factor)
# Also we can note that 'origin' and 'year' can be considered as categorical variables as well.
# So let's change their type to factor, and later we can use that in the plots.
unique(Auto$origin)
Auto$origin <- as.factor(Auto$origin)
Auto$year <- as.factor(Auto$year)
# The others are numeric/quantitative variables/predictors.


## (b) (score = 3) What is the range of each quantitative predictor? 
nc <- ncol(Auto)
Auto_num <- Auto[, -c(nc:(nc-2))]
sapply(Auto_num, range)



## (c) (score = 3) What is the mean and standard deviation of each quantitative predictor? 
sapply(Auto_num, mean)
sapply(Auto_num, sd)



## (d) (score = 14) Investigate the predictors graphically.

# lets plot the convariance matrix (needed libraries are mentioned at the beginning of the script)
# to see which columns are correlated most and plot the visualisations.
corr <- round(cor(Auto_num), 3)
ggcorrplot(corr, method = "circle", lab = TRUE, lab_size = 2)


# As we can see from the matrix, for example Displacement and Weight are highly correlated.
# Let's plot a scatterplot and make sure about the correlation.
ggplot(Auto, aes(x = displacement, y = weight)) + 
  geom_point(size = 2, color = 'mediumpurple4', shape = 2, alpha = 0.7) +
  geom_smooth(method = "loess") + 
  labs(title = 'Distribution between Displacement and Weight', 
       x = 'Displacement', y = 'Weight') + 
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(color = 'lavender'),
        panel.grid.minor = element_line(color = 'lavender'),
        title = element_text(color = 'plum4', face = 'bold'),
        axis.title = element_text(color = 'plum4', face = 'bold'),
        axis.ticks = element_blank())

plot(x = Auto$displacement, Auto$weight)


# For another scatterplot consider Horsepower and Mpg. Their correlation is near to -1. 
ggplot(Auto, aes(x = weight, y = mpg)) + 
  geom_point(size = 2, color = 'mediumpurple4', shape = 2, alpha = 0.7) +
  geom_smooth(method = "loess") + 
  labs(title = 'Distribution between Weight and MPG', 
       x = 'Weight', y = 'MPG') + 
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(color = 'lavender'),
        panel.grid.minor = element_line(color = 'lavender'),
        title = element_text(color = 'plum4', face = 'bold'),
        axis.title = element_text(color = 'plum4', face = 'bold'),
        axis.ticks = element_blank())

plot(x = Auto$weight, Auto$mpg)

# As we can see ggplot gives more opportunities and nicer results. So lets continue with ggplot.

# Further for considering the qualitative columns as well, let's consider a boxplot: 
ggplot(Auto, aes(x = origin, y = mpg)) + 
  geom_boxplot(varwidth=T, fill="azure4") + 
  labs(title="Miles Per Gallon depending on Origin", 
       x="Origin",
       y="Miles Per Gallon (MPG)") + 
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(color = 'azure2'),
        panel.grid.minor = element_line(color = 'azure2'),
        title = element_text(color = 'azure4', face = 'bold'),
        axis.title = element_text(color = 'azure4', face = 'bold'),
        axis.ticks = element_blank())

# We can see that as origin number increases, the MPG value of the vehicle, tends to increase as well.

# More we can plot violin plots, which are similar to box plot but show the density within groups as well.
ggplot(Auto, aes(x = year, y = mpg)) + 
  geom_violin(fill = "ivory3") + 
  labs(title="Violin plot between Year and MPG", 
       x="Year",
       y="MPG") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(color = 'ivory2'),
        panel.grid.minor = element_line(color = 'ivory2'),
        title = element_text(color = 'ivory4', face = 'bold'),
        axis.title = element_text(color = 'ivory4', face = 'bold'),
        axis.ticks = element_blank())
# Here we can see a similar tendency between Year and MPG as in previous case between Origin and MPG.



## (d) (score = 4) Suppose that we wish to predict gas mileage (mpg) on the basis of the other variables. 
## Do your plots suggest that any of the other variables might be useful in predicting mpg? Justify your answer.

# From the correlation matrix/plot, we can see that cylinders, displacement, horsepower and weight can be pretty useful to 
# predict the MPG, as they have correlation with MPG closer to -1. Moreover we have a scatterplot between MPG and weight 
# variables which prove the previous statement. Moreover, from the boxplot and violin plots we can see that 
# the year and origin variables can be useful as well.


##########################################################################################################################
data("Boston")

## (a) (score = 4) How many rows are in this data set? How many columns? What do the rows and columns represent?
str(Boston)
# As we can see from the response of the function above, we have 506 rows and 14 columns.
# Rows represent observations or sample size, and columns represent variables or predictors.
# Each row represents a suburb in Boston.

# But column names are a little bit confusing, so we can look up their explanations in the internet,
# for simplifing further work. Here are the explanations that I found:

# CRIM - per capita crime rate by town
# ZN - proportion of residential land zoned for lots over 25,000 sq.ft.
# INDUS - proportion of non-retail business acres per town.
# CHAS - Charles River dummy variable (1 if tract bounds river; 0 otherwise)
# NOX - nitric oxides concentration (parts per 10 million)
# RM - average number of rooms per dwelling
# AGE - proportion of owner-occupied units built prior to 1940
# DIS - weighted distances to five Boston employment centres
# RAD - index of accessibility to radial highways
# TAX - full-value property-tax rate per $10,000
# PTRATIO - pupil-teacher ratio by town
# B - 1000(Bk - 0.63)^2 where Bk is the proportion of blacks by town
# LSTAT - % lower status of the population
# MEDV - Median value of owner-occupied homes in $1000's


## (b) (score = 4) Make some pairwise scatterplots of the predictors (columns) in this data set. Describe your findings.


ggplot(Boston, aes(x = dis, y = lstat)) +
  geom_point(size = 3, color = 'violetred4', shape = 15, alpha = 0.5) +
  geom_smooth(method = "lm") + 
  labs(title = 'Distances to employment centres Dependance on % population lower status', 
       x = 'Weighted Distances', y = '% lower status of the population') + 
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(color = 'thistle1'),
        panel.grid.minor = element_line(color = 'thistle1'),
        title = element_text(color = 'thistle4', face = 'bold'),
        axis.title = element_text(color = 'plum4', face = 'bold'),
        axis.ticks = element_blank())
# From the graph we can see that DIS and LSTAT variables/predictors are negatively correlated.


ggplot(Boston, aes(x = rm, y = medv)) +
  geom_point(size = 2, color = 'cornflowerblue', shape = 9) +
  geom_smooth(method = "lm") + 
  labs(title = 'Average number of rooms And Median value of owner-occupied homes', 
       x = 'Number of Rooms', y = 'Medium Value') + 
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(color = 'lightcyan'),
        panel.grid.minor = element_line(color = 'lightcyan'),
        title = element_text(color = 'cornflowerblue', face = 'bold'),
        axis.title = element_text(color = 'cornflowerblue', face = 'bold'),
        axis.ticks = element_blank())
# From this one, we can see that as expected the average number of rooms in the suburb houses and medium values are positively correlated, 
# so they are associated with each other. For example, the medium value of a house could have been predicted using the average number of rooms.


ggplot(Boston, aes(x = rad, y = tax)) +
  geom_point(color = 'slategray4') +
  geom_smooth(method = "lm", color = "slategray3") + 
  labs(title = 'Radial highways accessibility"s dependence on Property-tax rate', 
       x = 'Index of accessibility to radial highways', y = 'Full-value property-tax rate') + 
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(color = 'slategray1'),
        panel.grid.minor = element_line(color = 'slategray1'),
        title = element_text(color = 'slategray4', face = 'bold'),
        axis.title = element_text(color = 'slategray4', face = 'bold'),
        axis.ticks = element_blank())
# Here again we can see a positive correlation. But after calculating the correlation's exact value, we will see that it is higher than 
# most of us would expect.
cor(Boston$rad, Boston$tax)
# it is 0.91.


## (c) (score = 4) Are any of the predictors associated with per capita crime rate? If so, explain the relationship.
# One method to do this is to fit a regression model, where response is Crime Rate. And by function summary explore the 
# p-values. 
lm_fit =lm(crim~ . , data = Boston)
summary(lm_fit)
# We can see that variables ZN, NOX, DIS, RAD, BLACK, LSTAT, MEDV are really associated with Crime Rate variable, 
# and could be useful to predict the CRIM, whereas PTRATIO, TAX, AGE, RM, CHAS, INDUS are not. 


## (d) (score = 4) Do any of the suburbs of Boston appear to have particularly high crime rates? Tax rates? Pupil-teacher ratios? 
## Comment on the range of each predictor.

# First lets see the ranges of all predictors:
sapply(Boston, range)
# The range of crime rate is [0.00632, 88.97620] => There are suburbs with particularly high crime rates.
# The range of Tax rates is [187, 711] => Here are again there are suburbs with high and low tax rates.
# The range of Pupil-teacher ratio is [12.6, 22.0] => In this case the range is not that big, and all suburbs have pupil-teacher ratio not bigger than 22.

# To have even more information, we can use summary function, and see not only the min and max of the variables, but also the quartiles.
summary(Boston[c('crim', 'tax', 'ptratio')])
# Here we can see that ptratio and tax are distributed pretty fair in their range, but CRIM variable's 3rd Q. is really low compared to it max. 
# So we can say that there are outliers in the column, and there are suburbs that have high crime rates compared to most of the others.
# To make sure, we can plot the barplot of that column.
ggplot(Boston, aes(y = crim)) + geom_boxplot() + 
  labs(title = "Boxplot of Crime Rate values in the suburbs") + 
  theme_light()


## (e) (score = 4) How many of the suburbs in this data set bound the Charles river?
Boston %>%
  group_by(chas) %>%
  summarise(count = n())
# Recall that this value is 1 if tract bounds river; 0 otherwise, we have 471 suburbs not bounding the river, and 35 suburbs bounding it. 
# So the answer to this question is 35.







