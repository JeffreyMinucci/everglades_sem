library(glmnet)
library(caret)
library(pdp)


df <- read.csv("data/missForest_Gambusia_Output.csv")
drops <- c("STA_ID", "Date","X")  # keeping LONG and LAT in 
df <- df[!is.na(df$THG_Fish),!(names(df) %in% drops)]
df$ln_THG_Fish <- log(df$THG_Fish)
df <- df[,names(df) != "THG_Fish"]

set.seed(1454)
trainIndex <- createDataPartition(df$ln_THG_Fish, p=0.8, list = FALSE)
df_train <- df[trainIndex,]
df_test <- df[-trainIndex,]


#fitControl <- trainControl(method='repeatedcv', number = 5, repeats = 5)
fitControl <- trainControl(method='cv', number = 5)

# start parallel cluster
cl <- makeCluster(10) # 10 cores
registerDoParallel(cl)

#########################

### random forest

# best so far - Rsq = 0.62

rf <- train(ln_THG_Fish ~ ., data = df_train,
              method = "rf",
              trControl = fitControl,
              #preProcess = c("center", "scale"),
              verbose = FALSE,
              tuneLength = 20,  # try 20 different mtry values
              na.action = na.pass)
rf


# end parallel cluster
stopCluster(cl)

# results and prediction accuracy
rf_predict <- predict(rf, df_test, na.action = NULL)
postResample(pred = rf_predict, obs = df_test$ln_THG_Fish)
plot(rf) # show error vs mtry
plot(rf$finalModel) # show error vs ntrees

randomForest::varImpPlot(rf$finalModel)  # variable importance

# plot predicted vs actual
plot(df_test$ln_THG_Fish, rf_predict, xlim=c(0,7), ylim=c(0,7), xlab = "Actual ln Hg in fish", ylab = "Predicted ln Hg in fish")
abline(0,1)


#########################

### model-averaged neural networks

# best so far - Rsq = 0.55

cl <- makeCluster(10) # 10 cores
registerDoParallel(cl)

nnet_grid <- expand.grid(.decay = c(0.1, 1, 1.5, 3, 5), .size = c(2,3,4,5), .bag = c(F))

nn <- train(ln_THG_Fish ~ ., data=df_train,
            method = "avNNet",
            trControl = fitControl,
            tuneGrid = nnet_grid,
            preProcess = c("center", "scale"),
            linout=T,
            repeats = 20,
            verbose = FALSE)
nn


# end parallel cluster
stopCluster(cl)

# results and prediction accuracy
nn_predict <- predict(nn, df_test, na.action = NULL)
postResample(pred = nn_predict, obs = df_test$ln_THG_Fish)
plot(nn)

varImp(nn)  # variable importance, scaled to 100 for most important

# plot predicted vs actual
plot(df_test$ln_THG_Fish, nn_predict, xlim=c(0,7), ylim=c(0,7), xlab = "Actual ln Hg in fish", ylab = "Predicted ln Hg in fish")
abline(0,1)

#########################

### generalized linear model via penalized maximum likelihood (glmnet package)
# alpha = 0 is the lasso penalty; alpha = 1 is the lasso penalty, alpha = 0 is the ridge penalty

# best so far - Rsq = 0.55

cl <- makeCluster(10) # 10 cores
registerDoParallel(cl)

glmnet_grid = expand.grid(alpha = 0:1, lambda = seq(0.0001, 1, length = 300))
predictors <- model.matrix(ln_THG_Fish ~ .-1, data=df_train)
glmr <- train(predictors, df_train$ln_THG_Fish,
            method = "glmnet",
            metric = "RMSE",
            trControl = fitControl,
            tuneGrid = glmnet_grid
            #preProcess = c("center", "scale"),
            )
glmr


# end parallel cluster
stopCluster(cl)

# results and prediction accuracy
df_test_dummy =  model.matrix(ln_THG_Fish ~ .-1, data=df_test)
glmr_predict <- predict(glmr, df_test_dummy, na.action = NULL)
postResample(pred = glmr_predict, obs = df_test$ln_THG_Fish)
plot(glmr)
coef(glmr$finalModel, glmr$bestTune$lambda)

# plot predicted vs actual
plot(df_test$ln_THG_Fish, glmr_predict, xlim=c(0,7), ylim=c(0,7), xlab = "Actual ln Hg in fish", ylab = "Predicted ln Hg in fish")
abline(0,1)


#########################
### GAM

# best so far - Rsq = 0.49

cl <- makeCluster(10) # 10 cores
registerDoParallel(cl)

# best so far - Rsq = 
gam1 <- train(ln_THG_Fish ~ ., data = df_train,
            method = "gam",
            #trControl = fitControl, 
            trControl = trainControl(method='none'),
            tuneGrid = expand.grid(select=c(TRUE), method = c("GCV.Cp")),
            preProcess = c("center", "scale"),
            verbose = FALSE)
gam1


# end parallel cluster
stopCluster(cl)

# results and prediction accuracy
gam_predict <- predict(gam1, df_test, na.action = NULL)
postResample(pred = gam_predict, obs = df_test$ln_THG_Fish)

plot(gam1$finalModel)
summary(gam1$finalModel)
varImp(gam1)  # variable importance

# plot predicted vs actual
plot(df_test$ln_THG_Fish, gam_predict, xlim=c(0,7), ylim=c(0,7), xlab = "Actual ln Hg in fish", ylab = "Predicted ln Hg in fish")
abline(0,1)


##########################
# MARS - multivariate adaptive regression splines

# best so far - Rsq = 0.58

cl <- makeCluster(10) # 10 cores
registerDoParallel(cl)

mars_grid = expand.grid(nprune=seq(2,30,2), degree = c(1,2,3))
mars <- train(ln_THG_Fish ~ ., data = df_train,
              method = "earth",
              trControl = fitControl, 
              #trControl = trainControl(method='none'),
              tuneGrid = mars_grid,
              preProcess = c("center", "scale"))
mars

# end parallel cluster
stopCluster(cl)

plot(mars)  # vizualize tuning

# results and prediction accuracy
mars_predict <- predict(mars, df_test, na.action = NULL)
postResample(pred = mars_predict, obs = df_test$ln_THG_Fish)

plot(mars$finalModel)
summary(mars$finalModel)
varImp(mars)  # variable importance

# plot predicted vs actual
plot(df_test$ln_THG_Fish, mars_predict, xlim=c(0,7), ylim=c(0,7), xlab = "Actual ln Hg in fish", ylab = "Predicted ln Hg in fish")
abline(0,1)

#partial dependence plots
partial(mars, pred.var = "MEHG_SW",plot=T, rug=T)
partial(mars, pred.var = "Alk_Phos_SW", plot=T, rug=T)
partial(mars, pred.var = "LONG",plot=T, rug = T)

