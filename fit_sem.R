library(lavaan)


data <- read.csv('data/everglades_sem_imputed_cleaned.csv')[,-1]
data <- data[!is.na(data$ln_Hg_fish),]

data <- scale(data)

## Split into training and test data - Not currently used for SEM 
train_size <- floor(0.90 * nrow(data))
set.seed(123)
train_ind <- sample(seq_len(nrow(data)), size = train_size)
train <- data[train_ind, ]
test <- data[-train_ind, ]



## copying final model in publication
model <- '#regressions
          ln_Hg_fish ~ MeHg_water + MeHg_peri + DOC + sulfate + sulfide
          MeHg_water ~ MeHg_peri + DOC + sulfate
          MeHg_peri ~ sulfate + DOC + MeHg_soil + Hg_soil
          MeHg_soil ~ sulfide + sulfate + DOC + soil_drywt
          Hg_soil ~ soil_drywt

          #Variances
          ln_Hg_fish ~~ ln_Hg_fish
          MeHg_peri ~~ MeHg_peri
          Hg_soil ~~ Hg_soil
          MeHg_soil ~~ MeHg_soil
          MeHg_water ~~ MeHg_water

          #covariances
          MeHg_soil ~~ Hg_soil
          ln_Hg_fish ~~ Hg_soil'

sem_model <- sem(model, data)#, se = 'bootstrap')
summary(sem_model, fit.measures=T, standardized=TRUE)



## adding in TP and TN, following fig 3 in publication
model2 <- '#regressions
          ln_Hg_fish ~ MeHg_water + MeHg_peri + DOC + sulfate + sulfide + TN + TP
          MeHg_water ~ MeHg_peri + DOC + sulfate
          MeHg_peri ~ sulfate + DOC + MeHg_soil + Hg_soil + TP + TN
          MeHg_soil ~ sulfide + sulfate + DOC + soil_drywt + TP + TN
          Hg_soil ~ soil_drywt
          TN ~ DOC + TP
          

          #variances
          ln_Hg_fish ~~ ln_Hg_fish
          MeHg_peri ~~ MeHg_peri
          Hg_soil ~~ Hg_soil
          MeHg_soil ~~ MeHg_soil
          MeHg_water ~~ MeHg_water

          #covariances
          MeHg_soil ~~ Hg_soil
          ln_Hg_fish ~~ Hg_soil'

sem_model2 <- sem(model2, data)
summary(sem_model2, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)

predicted <- lavPredict(sem_model2,type= "yhat")[,"ln_Hg_fish"]
r.sq <- cor(data[,"ln_Hg_fish"], predicted)^2
r.sq
