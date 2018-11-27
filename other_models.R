library(glmnet)
library(caret)

data <- read.csv('data/everglades_sem_cleaned.csv')[,-1]

predictors <- model.matrix(ln_Hg_fish ~ ., data=data)
response <- data[complete.cases(data), 'ln_Hg_fish']
fit <- cv.glmnet(x = predictors, y = response)
plot(fit)
fit$lambda.min
coef(fit,s='lambda.min')
