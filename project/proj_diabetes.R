rm(list=ls())

require(tidyverse)
require(ISLR)
require(randomForest)
require(arules)
#library(tree)

factor_levels <- c("No", "Yes")
diabetes <- read_csv('./raw_data/diabetes.csv', 
                     col_names = TRUE,
                     col_types = list(
                      `Age` = col_double(),
                      `Gender` = col_factor(),
                      `Polyuria` = col_factor(levels = factor_levels),
                      `Polydipsia` = col_factor(levels = factor_levels),
                      `sudden weight loss` = col_factor(levels = factor_levels),
                      `weakness` = col_factor(levels = factor_levels),
                      `Polyphagia` = col_factor(levels = factor_levels),
                      `Genital thrush` = col_factor(levels = factor_levels),
                      `visual blurring` = col_factor(levels = factor_levels),
                      `Itching` = col_factor(levels = factor_levels),
                      `Irritability` = col_factor(levels = factor_levels),
                      `delayed healing` = col_factor(levels = factor_levels),
                      `partial paresis` = col_factor(levels = factor_levels),
                      `muscle stiffness` = col_factor(levels = factor_levels),
                      `Alopecia` = col_factor(levels = factor_levels),
                      `Obesity` = col_factor(levels = factor_levels),
                      `class` = col_factor(levels = c('Negative', 'Positive'))
                      ))

names(diabetes) <- c('age', 'gender', 'polyuria', 'polydipsia', 'wtloss',
                     'weakness', 'polyphagia', 'genitaltrush', 
                     'visualblur', 'itching', 'irritable', 'delayheal',
                     'partparesis', 'musclestiff', 'alopecia', 'obesity',
                     'class')
diabetes <- mutate(diabetes, id = c(1:520), .before = age)


####### Exploratory Data Analysis #####
summary(diabetes)


diabetes %>% 
  group_by(class) %>%
  summarise(ave_age = mean(age),
            med_age = median(age),
            n = n())

# age distribution of diabetes class by gender
diabetes %>% 
  group_by(class, gender) %>%
  summarise(ave_age = mean(age),
            med_age = median(age),
            n = n())

ggplot(diabetes, aes(x=age, y = , fill = gender)) +
  geom_histogram(bins = 60, color = "black", fill = "white") +
  facet_wrap(~class) +
  theme_bw()


## Males who are positive for diabetes 
# 
ggplot(diabetes, aes(x = class, y = age)) +
  geom_boxplot() +
  geom_jitter(shape =1, size = 1, width = 0.2) +
  facet_wrap(~gender) +
  theme_bw()

# pees a lot
diabetes %>% 
  group_by(class, polyuria) %>%
  summarise(n = n())

# excess thirst
diabetes %>% 
  group_by(class, polydipsia) %>%
  summarise(n = n())

# weight loss
diabetes %>% 
  group_by(class, wtloss) %>%
  summarise(n = n())

# extreme hunger
diabetes %>% 
  group_by(class, polyphagia) %>%
  summarise(n = n())

# blurry vision
diabetes %>% 
  group_by(class, visualblur) %>%
  summarise(n = n())

# fatigue
diabetes %>% 
  group_by(class, weakness) %>%
  summarise(n = n())

# delay heal 
diabetes %>% 
  group_by(class, delayheal) %>%
  summarise(n = n())

# delay heal 
diabetes %>% 
  group_by(class, obesity) %>%
  summarise(n = n())

diabetesdf <- diabetesdf %>% 
  mutate(id = 1:nrow(diabetesdf), .before = age)

diabetestall <- diabetesdf %>%
  pivot_longer(cols = c(4:17), names_to = 'symptoms', values_to = 'val')

diabetestall

filter(diabetestall, class == "Positive" & symptoms == "polyuria") %>% 
  summarise(pct = n()/520)

symptoms <- unique(diabetestall$symptoms)


################ END OF EDA ##############



################ START OF LOGISTIC REGRESSION ############

# stratified hold-out following 80/20. This means 80% of the positive case and 80% of the negative case will comprise the total 80% of the train data
set.seed(100)
train80 <- sample(1:nrow(diabetes), 
                  replace = F, 
                  prob = diabetes$class, 
                  size = nrow(diabetes)*0.8)
test20 <- diabetes[-train80,]
test20.values <- diabetes$class[-train80]

# logistic regression procedure 

# FULL MODEL
train80.glm <- glm(class ~ . -id ,
                data=diabetes,
                subset=train80,
                family=binomial)
summary(train80.glm)
# there are 8 significant variables below test significance alpha=0.05
# age, genderFemale, polyuriaYes, polydispsiaNom genitaltrushYes, itchingNo, irritableYes, partparesis

# coefficients of full model logistic regression
coef(train80.glm)
# odd-ratio of full model logistic regression
exp(coef(train80.glm))

# performance logistic regression full model 
train80glm.probs <- predict(train80.glm, test20, type="response")
glm.predict <- ifelse(train80glm.probs > 0.5, "Positive", "Negative")
table(actual = test20.values, pred = glm.predict)

# prediction accuracy of 80/20 hold-out
mean(test20.values == glm.predict)


# cross-validation logistic regression model

set.seed(100)
# set number of folds 
k = 10

# randomly assign indices to folds
folds = sample(1:k, nrow(diabetes), replace = TRUE)

# evaluating the 10-fold cross-validation prediction of the model.

# zero vectors to hold accuracy values of cv method
accuracy = rep(0,k)

for(i in 1:k)
{
  # cv logistic regression model of train data
  train80.glm2 <- glm(class ~ . -id,
                      family = "binomial",
                      data = diabetes[folds!=i, ])
  
  # assign the current ith iteration as the test data (test20)
  test20 <- diabetes[folds==i, ]
  
  # obtain the probabilities using the test data (test20)
  train80glm.probs2 <- predict(train80.glm2, test20, type="response")
  
  glm.predict2 <- ifelse(train80glm.probs2 > 0.5, "Positive", "Negative")
  #glm.predict2 <- rep("Negative", nrow(diabetes[folds==i,]))
  
  #glm.predict2[train80glm.probs2 > 0.5] <- "Postive"
  
  # y-value (class variable) of the test data in the ith iteration
  test20.values <- diabetes$class[folds==i]
  
  # calculate the accuracy 
  accuracy[i] <- mean(glm.predict2==test20.values)
}

# Prediction Accuracy of CV Logistic Regression
mean(accuracy)

################ END OF LOGISTIC REGRESSION ############

# RANDOM FORESTS
set.seed(100)

# random forest model using train80 train dataset (stratified 80/20)
# mtry will be default sqrt(p) where p is the number of parameters

bag.diabetes <- randomForest(class ~ . -id, 
                             data = diabetes,
                             subset = train80,
                             importance = TRUE)

# rf 80/20 model prediction
yhat.bag <- predict(bag.diabetes, 
                    newdata=diabetes[-train80,]) # or test20

# rf 80/20 model performance
table(test20$class, yhat.bag)
mean(test20$class == yhat.bag)

importance(bag.diabetes)
varImpPlot(bag.diabetes)

## Association #naive bayes
attach(diabetes)
sink("rules.txt")
diabetes.rules <- apriori(data = diabetes,
                          parameter = list(minlen=7, supp=0.2,conf=0.8),
                          appearance = list(rhs("class=Positive"), default="lhs"))

                          
