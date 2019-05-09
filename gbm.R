library(caret)
library(gbm)
library(doParallel)

df <- read.csv("data/missForest_Gambusia_Output.csv")
drops <- c("STA_ID", "Date","X")
df <- df[!is.na(df$THG_Fish),!(names(df) %in% drops)]
df$ln_THG_Fish <- log(df$THG_Fish)
df <- df[,names(df) != "THG_Fish"]

# split to training and test datasets
set.seed(1454)
trainIndex <- createDataPartition(df$ln_THG_Fish, p=0.9, list = FALSE)
df_train <- df[trainIndex,]
df_test <- df[-trainIndex,]

fitControl <- trainControl(method='repeatedcv', number = 10, repeats = 10)

# start parallel cluster
cl <- makeCluster(10) # 10 cores
registerDoParallel(cl)

### GBM
# define our grid of hyperparameters to try
gbmGrid <- expand.grid(interaction.depth = c(1,3),
                       n.trees = (1:75)*50,  # no cost to try a lot of these, as they are all derived from one run
                       shrinkage = 0.01,
                       n.minobsinnode = c(10))

gbm1 <- train(ln_THG_Fish ~ ., data = df_train,
              method = "gbm",
              trControl = fitControl,
              tuneGrid = gbmGrid,
              #preProcess = c("center", "scale"),
              verbose = FALSE,
              na.action = na.pass)
gbm1


# end parallel cluster
stopCluster(cl)

# results and prediction accuracy
gbm_predict <- predict(gbm1, df_test, na.action = NULL)
postResample(pred = gbm_predict, obs = df_test$ln_THG_Fish)
plot(gbm1)

varImp(gbm1)  # variable importance, scaled to 100 for most important

plot(gbm1$finalModel, i.var = "Alk_Phos_SW", ntrees=100)
plot(gbm1$finalModel, i.var = "MEHG_SW", ntrees=100)
