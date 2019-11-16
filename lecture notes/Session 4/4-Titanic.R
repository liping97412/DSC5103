##########################################################
# DSC5103 Statistics
# Session 4. Demo of logistic regression on binomial data (using the Titanic dataset)
# 2017.09
#
##########################################################


#############################
### prepare the dataset
#############################
?Titanic
str(Titanic)
Titanic  # it is a 4-dimensional table??

# visualize
library("graphics")
mosaicplot(Titanic, main = "Survival on the Titanic")

# convert to dataframe
Titanic.df <- as.data.frame(Titanic)
Titanic.df

# long to wide shape for logistic regression
library("tidyr")
data <- spread(Titanic.df, Survived, Freq)
data$Age <- relevel(data$Age, ref="Adult")
summary(data)
data


#############################
### logistic regression on binomial data
#############################
# model 1
glm1 <- glm(cbind(Yes, No) ~ Class + Sex + Age, data, family = binomial())
summary(glm1)

# model 2
glm2 <- glm(cbind(Yes, No) ~ Class + Sex * Age, data, family = binomial())
summary(glm2)

# model 3
# this is a saturated model with 0 degree of freedom!!
glm3 <- glm(cbind(Yes, No) ~ Class * Sex * Age, data, family = binomial())
summary(glm3)
