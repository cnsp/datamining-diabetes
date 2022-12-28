rm(list=ls())

require(tidyverse)
require(broom)
require(gt)
require(ISLR)
require(randomForest)
require(arules)
require(arulesViz)
require(tree)
require(e1071)
# additional packages 
require(gtsummary)
require(sjPlot)
require(DataExplorer)
require(glmulti)
require(tictoc)
require(flextable)
require(performance)
require(kableExtra)


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
                     'weakness', 'polyphagia', 'genitalthrush', 
                     'visualblur', 'itching', 'irritable', 'delayheal',
                     'partparesis', 'musclestiff', 'alopecia', 'obesity',
                     'class')
diabetes <- mutate(diabetes, id = c(1:nrow(diabetes)), .before = age)


####### Exploratory Data Analysis #####
summary(diabetes)

plot_bar(diabetes, 
         ggtheme = theme_bw()) 
plot_bar(diabetes, 
         by = "class", 
         ggtheme = theme_bw())

# table of response 
diabetes_long <- diabetes %>% 
  pivot_longer(cols = c(3:17), names_to = "predictor", values_to = "response") 

diabetes_long %>% 
  group_by(predictor, response) %>% 
  summarise(n = n())


# stratified, by=class table summary
diabetes %>%
  select(age, gender, class) %>%
  tbl_summary(by = "class",
             statistic = 
               list(
               all_continuous() ~ "{mean} ({sd})",
               all_dichotomous() ~ "{p}%" 
               )
             ) %>% 
  add_p()

diabetes %>%
  select(age, gender, class) %>%
  group_by(class) %>%
  summarise(aveage = mean(age), sdage = sd(age))

ggplot(diabetes, aes(x=age, y = , fill = gender)) +
  geom_histogram(bins = 60, color = "black", fill = "white") +
  facet_wrap(~class) +
  theme_bw()


# distribution histogram
ggplot(diabetes, 
       aes(x = age, 
           fill = gender,
           color = gender)) +
  geom_histogram(bins = 15, alpha = 0.4) +
  facet_wrap(~class) + 
  theme_bw() +
  theme(legend.position = "top")

# distribution boxplot
ggplot(diabetes, aes(x = gender, y = age, fill = gender)) +
  geom_boxplot(alpha = 0.5) +
  #geom_jitter(shape =1, size = 1, 
  #            width = 0.2, 
  #            alpha = 0.4) + 
  stat_summary(fun = "mean", 
               shape = 18) +
  facet_wrap(~class) +
  theme_bw() + 
  theme(legend.position = "none") 

diabetesdf <- diabetes

diabetestall <- diabetesdf %>%
  pivot_longer(cols = c(4:17), names_to = 'symptoms', values_to = 'present')

diabetestall

diabetestall %>%
  select(class, age) %>%
  tbl_summary(by = "class") %>%
  add_p()


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


# stratified 80/20 FULL model logistic regression 
# FULL MODEL
train80.glm <- glm(class ~ . -id ,
                   data=diabetes,
                   subset=train80,
                   family=binomial)
summary(train80.glm)
# tidysummary
glance(train80.glm)
broom::tidy(train80.glm) %>% filter(p.value < 0.05) %>% gt() %>%
  tab_header(title = "Significant Predictors, pvalue < 0.05") %>%
  tab_footnote(footnote = "Logistic Regression")

# coefficients of stratified 80/20 FULL model logistic regression
coef(train80.glm)
# odd-ratio of stratified 80/20 FULL model logistic regression
exp(coef(train80.glm))

# performance stratified 80/20 logistic regression FULL model 
train80glm.probs <- predict(train80.glm, 
                            newdata = test20, 
                            type="response")

# predictions stratified 80/20 logistic regression FULL model
train80glm.predict <- ifelse(train80glm.probs > 0.5, "Positive", "Negative")

# confusion matrix strat 80/20 LR full model
table(actual = test20.values, predicted = train80glm.predict)
# prediction accuracy of stratified 80/20 FULL model
mean(test20.values == train80glm.predict)
#model_performance(train80.glm)

# significant predictors pvalue < 0.05
signif8020f <- broom::tidy(train80.glm) %>% 
  mutate(oddsratio = exp(estimate)) %>%
  mutate_if(is.numeric, round, digits = 3) %>%
  filter(p.value < 0.05)

m8020f <- broom::tidy(train80.glm) %>% 
  mutate(oddsratio = exp(estimate)) %>%
  mutate_if(is.numeric, round, digits = 3)


# create stratified 80/20 REDUCED model from significant predictors
train80.glm2 <- glm(class ~ gender + polyuria + polydipsia + genitalthrush + itching + irritable, 
                    data = diabetes,
                    subset = train80,
                    family = binomial)

# summary of reduced LR model using 80/20
summary(train80.glm2)

# odd ratio of stratified 80/20 REDUCED model 
exp(coef(train80.glm2))

m8020r <- broom::tidy(train80.glm2) %>%
  mutate(oddsratio = exp(estimate)) %>%
  mutate_if(is.numeric, round, digits = 3)

# performance of stratified 80/20 logistic regression reduced model
train80glm2.probs2 <- predict(train80.glm2, 
                             newdata = test20,
                             type = "response")

# prediction 80/20 LR reduced model
train80glm2.predict2 <- ifelse(train80glm2.probs2 > 0.5, "Positive", "Negative")
table(actual = test20.values, predict = train80glm2.predict2)
mean(test20.values == train80glm2.predict2)




# k=10 cross-validation logistic regression
set.seed(100)
k = 10

# randomly assign indices to folds
folds <- sample(1:k, nrow(diabetes), replace = TRUE)

# evaluating the 10-fold cross-validation prediction of the model.

# zero vectors to hold accuracy values of cv method
accuracy = rep(0,k)

for(i in 1:k)
{
  # cv logistic regression model of train data
  train.glm2 <- glm(class ~ . -id,
                      family = "binomial",
                      data = diabetes[folds!=i, ])
  
  # assign the current ith iteration as the test data
  test.cv <- diabetes[folds==i, ]
  
  # obtain the probabilities using the test data
  trainglm.probs2 <- predict(train.glm2, test.cv, type="response")
  
  glm.predict2 <- ifelse(trainglm.probs2 > 0.5, "Positive", "Negative")
  
  # y-value (class variable) of the test data in the ith iteration
  testcv.values <- diabetes$class[folds==i]
  
  # calculate the accuracy 
  accuracy[i] <- mean(glm.predict2==testcv.values)
}

# Prediction Accuracy of CV Logistic Regression
mean(accuracy)

## REDUCED CV model
accuracyred <- rep(0, 10)
for(i in 1:k)
{
  # cv logistic regression model of train data
  train.glmred <- glm(class ~ gender + polyuria + polydipsia + genitalthrush + itching + irritable,
                    family = "binomial",
                    data = diabetes[folds!=i, ])
  
  # assign the current ith iteration as the test data
  test.cvred <- diabetes[folds==i, ]
  
  # obtain the probabilities using the test data
  trainglm.probsred <- predict(train.glmred, test.cvred, type="response")
  
  glm.predictred <- ifelse(trainglm.probsred > 0.5, "Positive", "Negative")
  
  # y-value (class variable) of the test data in the ith iteration
  testcv.valuesred <- diabetes$class[folds==i]
  
  # calculate the accuracy 
  accuracyred[i] <- mean(glm.predictred==testcv.valuesred)
}

# Prediction Accuracy of CV Logistic Regression
mean(accuracyred)

# summary of logistic models
logmods <- tibble(full8020 = mean(test20.values == train80glm.predict),
                  red8020 = mean(test20.values == train80glm2.predict2),
                  fullcv = mean(accuracy),
                  redcv = mean(accuracyred))
logmods




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
table(actual = test20$class, predicted = yhat.bag)
rf8020f_confuse <- table(actual = test20$class, predicted = yhat.bag)
mean(test20$class == yhat.bag)

importance(bag.diabetes)
varImpPlot(bag.diabetes, main = "Full RF Model from 80/20 data")

# visualize a particular decision tree in random forest
plot(tree(bag.diabetes))
text(tree(bag.diabetes), cex = 0.8, pretty = TRUE)


# reduced RF model based on meandecreaseaccuracy > 20
bag.diabetesred <- randomForest(class ~ age + gender + polyuria + polydipsia + wtloss + itching + irritable + delayheal + partparesis + alopecia,
                             data = diabetes,
                             subset = train80,
                             importance = TRUE)

# reduced RF prediction
yhat.bagred <- predict(bag.diabetesred, 
                       newdata=diabetes[-train80,])
# reduced RF performance
table(actual = test20$class, predicted = yhat.bagred)
rf8020r_confuse <- table(actual = test20$class, predicted = yhat.bagred)
mean(test20$class == yhat.bagred)


####### END OF RANDOM FORESTS ######


#### NAIVE BAYES ####

## stratified 80/20 NB model
diabetes.nb <- naiveBayes(class ~ . -id -age, 
                          data = diabetes,
                          subset = train80)

## stratified 80/20 NB prediction
diabetesnb.pred <- predict(diabetes.nb, 
                           test20, 
                           type = "class")

## stratified 80/20 NB performance
table(test20.values, diabetesnb.pred)
mean(test20.values == diabetesnb.pred)

## create table of priors
diabetesnb.priors <- data.frame(t(sapply(diabetes.nb$tables, c)))[ -c(1),]
names(diabetesnb.priors) <- c("Negative.No", "Positive.No", "Negative.Yes", "Positve.Yes")

diabetesnb.priors<- diabetesnb.priors %>% 
  mutate(symptoms = rownames(diabetesnb.priors), .before = 1)

diabetesnb.priors <- tibble(diabetesnb.priors)

diabetesnb.priors <- diabetesnb.priors %>% 
  mutate_if(is.numeric, round, digits = 3)

gt(diabetesnb.priors) %>%
  tab_header(title = "Prior Probabilities",
             subtitle = "P(class | symptoms)")

diabetesnbgender.priors <- data.frame(t(sapply(diabetes.nb$tables, c)))[1,]
colnames(diabetesnbgender.priors) <- c("Negative.No", "Positive.No", "Negative.Yes", "Positve.Yes")

diabetesnbgender.priors<- diabetesnbgender.priors %>% 
  mutate(symptoms = rownames(diabetesnbgender.priors), .before = 1)
diabetesnbgender.priors <- tibble(diabetesnbgender.priors)
diabetesnbgender.priors <- diabetesnbgender.priors %>% 
  mutate_if(is.numeric, round, digits = 3)

gt(diabetesnbgender.priors) %>%
  tab_header(title = "Prior Probabilities",
             subtitle = "P(class | gender)")


## NB CROSS VALIDATION
# will use the same fold parameters and random assignment in logistic regression

# zero vectors to hold accuracy values of cv method
accuracy.nb = rep(0,k)

for(i in 1:k)
{
  # cv logistic regression model of train data
  train.nb <- naiveBayes(class ~ . -id -age,
                           data = diabetes[folds!=i, ],
                           type = "class")
  
  # assign the current ith iteration as the test data
  test.nb <- diabetes[folds == i,]
  
  # obtain prediction using test data
  trainnb.pred <- predict(train.nb, 
                          test.nb, 
                          type= "class")
  
  # obtain response values of test data
  testnb.values <- diabetes$class[folds == i]
  
  # calculate the accuracy 
  accuracy.nb[i] <- mean(testnb.values == trainnb.pred)
  
}

# Prediction Accuracy of CV Naive Bayes
mean(accuracy.nb)



#### END OF NAIVE BAYES #######

# SUMMARY OF MODEL SCORES
model_scores <- tibble(model = c(rep("LR",4), "NB", "NB", "RF", "RF"),
                       type = c("full", "reduced", "full", "reduced", rep("full", 3), "reduced"),
                       resampling = c("prop", "prop", "cv", "cv", "prop", "cv", "prop", "prop"),
                       accuracy = c(
                         round(mean(test20.values == train80glm.predict), 4)*100,
                         round(mean(test20.values == train80glm2.predict2),4)*100,
                         round(mean(accuracy),4)*100,
                         round(mean(accuracyred),4)*100,
                         round(mean(test20.values == diabetesnb.pred),4)*100,
                         round(mean(accuracy.nb), 4)*100, 
                         round(mean(test20$class == yhat.bag), 4)*100,
                         round(mean(test20$class == yhat.bagred),4)*100)
                       )


## Association Rules  ########

# prepare data. make id and age into category
diabetes2 <- diabetes

diabetes2 <- diabetes2 %>% 
  mutate(agegrp = ifelse(age <= 24, "early", 
                         ifelse(age >= 25 & age <= 54, "prime", "mature")), .before = age) %>%
  select(c(-id, -age))

diabetes2$agegrp <- as.factor(diabetes2$agegrp)
# diabetes2$id <- as.factor(diabetes2$agegrp)

transac <- as(diabetes2, "transactions")
dim(transac)
itemLabels(transac)
summary(transac)
itemFrequencyPlot(transac, topN=30, cex.names = 1)



diabetes.rules <- apriori(data = transac,
                          parameter = list(minlen=4, supp=0.3,conf=0.5, target = "rules"),
                          appearance = list(default="lhs", rhs = "class=Positive"))
summary(diabetes.rules)
inspect(diabetes.rules)


