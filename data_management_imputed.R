####
# Script for cleaning the new, imputed everglades SEM dataset
#
# Jeff Minucci 5/9/19
####

data <- read.csv('data/missForest_Gambusia_Output.csv')

#rename variables and log transform (except sulfate?)
data$MeHg_peri <- log(data$MEHG_Peri_AVG)
data$DOC <- log(data$TOC_SW)
data$soil_drywt<- log(data$AFDW_Soil+0.01)
data$sulfide <- log(data$H2S_PW)
data$sulfate <- log(data$SO4_SW)
data$MeHg_soil <- log(data$MEHG_soil)
data$MeHg_water <- log(data$MEHG_SW)
data$Hg_soil <- log(data$THG_soil)
data$ln_Hg_fish <- log(data$THG_Fish)
data$TN <- log(data$Tot_Nitrogen_SW)
data$TP <- log(data$Tot_Phos_SW)

to_keep <- c('ln_Hg_fish', 'DOC', 'soil_drywt', 'sulfide', 'sulfate', 'MeHg_soil', 'MeHg_water', 'Hg_soil', 'MeHg_peri',
             'TN', 'TP')
sem_data <- data[,to_keep]

write.csv(sem_data,'data/everglades_sem_i,puted_cleaned.csv')
