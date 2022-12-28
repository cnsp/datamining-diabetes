

set.seed(100)
train80 <- sample(1:nrow(diabetes), 
                  replace = F, 
                  prob = diabetes$class, 
                  size = nrow(diabetes)*0.8)
test20 <- diabetes[-train80,]
test20.values <- diabetes$class[-train80]


# stratified 80/20 FULL model logistic regression 
# test FULL MODEL with different seeds

j = 10000
lr8020facc <- rep(0, j)
lr8020facc <- rep(0, j)

rf8020racc <- rep(0, j)
rf8020racc <- rep(0, j)

for (i in 1:j)
{
  set.seed(100+i)
  train_ <- sample(1:nrow(diabetes), 
                  replace = F, 
                  prob = diabetes$class, 
                  size = nrow(diabetes)*0.8)
  test <- diabetes[-train_,]
  test.values <- diabetes$class[-train_]
  
  # performance FULL model 
  trainglm.probs <- predict(train80.glm, 
                            newdata = test, 
                            type="response")
  # predictions  FULL model
  train.predict <- ifelse(trainglm.probs > 0.5, "Positive", "Negative")

  # prediction accuracy FULL model
  f8020acc[i] <- mean(test.values == train.predict)
  
  # performance REDUCED model
  trainglm.probs2 <- predict(train80.glm2, 
                            newdata = test, 
                            type="response")
  # predictions stratified 80/20 logistic regression FULL model
  train.predict2 <- ifelse(trainglm.probs2 > 0.5, "Positive", "Negative")
  
  # prediction accuracy of stratified 80/20 FULL model
  r8020acc[i] <- mean(test.values == train.predict2)
  
}

logisticmodels <- tibble(f8020 = f8020acc, 
                         r8020 = r8020acc)
logisticmodelslong <- logisticmodels %>% 
  pivot_longer(cols = c(1,2), names_to = "modgrp", values_to = "accuracy" )
logisticmodelslong$modgrp <- as.factor(logisticmodelslong$modgrp)

ggplot(logisticmodelslong, aes(x = accuracy, fill = modgrp )) +
  geom_density(alpha = 0.40) +
  labs(title = "Stratified 80/20: Full vs Reduced",
       subtitle = "Prediction accuracy of 1000 different seeds") +
  theme_classic()

ggplot(logisticmodelslong, aes(x = modgrp, y = accuracy)) +
  geom_boxplot() +
  labs(title = "Stratified 80/20: Full vs Reduced",
       x = "Model Group (f8020: Full, r8020: Reduced)",
       y = "Model Accuracy (a)") +
  theme_classic()

t.test(f8020acc, r8020acc)
t.test(accuracy ~ modgrp, logisticmodelslong)
tbl_summary(logisticmodelslong, 
            statistic = accuracy ~ "{mean} ({sd})",
            by = modgrp) %>%
  add_difference()

