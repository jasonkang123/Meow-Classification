
install.packages(c("tuneR", "seewave", "psych", "clusterSim", "nFactors", "caret", "e1071", "class", "pROC", "stringr", "tidyverse"))
library(tuneR)
library(seewave)
library(psych)
library(clusterSim)
library(nFactors)
library(caret)
library(e1071)
library(class)
library(pROC)
library(dplyr)

filenames <- list.files(path = "C:/Users/Kangs/Documents/cataudio/catstrain/", pattern = "wav", full.names = TRUE) #load path of files
d<- lapply(filenames, readWave) #read all the wav files into d
names(d) <- gsub(".*/(.*)\\..*", "\\1", filenames) # use file name as vector name
left<-sapply(d, function(x) x@left) # take left of wav file descriptive statistics

spectro(d$cat_1)

# manual label of sound classification
x<- left$cat_1 #vector
x1<- 1 #file number
amplitude <- Mod(fft(x)[1:round(length(fft(x))/2,0)]) #get amplitude for each data point
featuredata<-cbind(describe(x)[,3:13],describe(amplitude)[,3:13],"Stop", x1) #extract statistical measures
write.table(featuredata,"C:/Users/Kangs/Documents/cataudio/catstrain/test/cat_audio_notfinaldata.csv",quote = FALSE, sep = ",", row.names = FALSE, col.names = TRUE, append = FALSE ) #store into csv

#import feature data table
featureData<-read.csv("C:/Users/Kangs/Documents/cataudio/catstrain/test/cat_audio_finaldata.csv", header = TRUE, sep = ",")
ft<-cbind(featureData[,1:22],featureData$label)

## 75% of the sample size
smp_size <- floor(0.50 * nrow(ft))

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(ft)), size = smp_size)

train <- scale(ft[train_ind,1:22 ], center=TRUE)
test <- scale(ft[-train_ind,1:22 ], center=TRUE)

trainlabel<-ft[train_ind,23 ]
testlabel<-ft[-train_ind,23 ]

#Support Vector Machine for classification
model_svm <- svm(trainlabel ~ train )

#Use the predictions on the data
pred <- predict(model_svm, test)
confusionMatrix(pred,testlabel)