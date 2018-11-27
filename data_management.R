####
# Script for cleaning the everglades SEM dataset
#
# Jeff Minucci 11/27/18
####

data <- read.csv('data/Everglades_SEM_Data.csv',stringsAsFactors = FALSE)

#fix div/0 errors in meHG periphython
data$MEHGPPAVG[data$MEHGPPAVG=="#DIV/0!"] <- NA
data$MEHGPPAVG <- as.numeric(data$MEHGPPAVG)
data$MeHg_peri <- log(data$MEHGPPAVG)

#combine various doc and toc measurements
colnames(data)[5:6] <- c('DOC1', 'DOC2')
data$DOC <- log(rowMeans(data[,c('DOC1', 'DOC2', 'TOC1', 'TOC2')], na.rm=T))

#rename variables and log transform (except sulfate?)
data$soil_drywt<- log(data$AFDWSD +0.01)
data$sulfide <- log(data$H2SPWE)
data$sulfate <- log(data$SO4SW)
data$MeHg_soil <- log(data$MEHGSDcombined)
data$MeHg_water <- log(data$MEHGSW)
data$Hg_soil <- log(data$THGSDcombined)
data$ln_Hg_fish <- data$ln_THGFSF
data$TN <- log(data$TNSW)
data$TP <- log(data$TPSW)

to_keep <- c('ln_Hg_fish', 'DOC', 'soil_drywt', 'sulfide', 'sulfate', 'MeHg_soil', 'MeHg_water', 'Hg_soil', 'MeHg_peri',
             'TN', 'TP')
sem_data <- data[,to_keep]

write.csv(sem_data,'data/everglades_sem_cleaned.csv')
