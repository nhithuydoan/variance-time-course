library(tidyverse)
library(dplyr)
if(!require('zoo')) {
  install.packages('zoo')
  library('zoo')
}

if(!require("readr")) {
  install.packages("readr")
  library("readr")
}
library(data.table)

library(ggplot2)
library(R.matlab)
library("writexl")
process.file2 <- function(filename){
  
  ## Read file, choose variables, changes names
  file <- R.matlab::readMat(filename)
  correctAns <- t(file$correctAns)
  acc <- (file$acc)
  RT <- (file$RT)
  MT <- (file$MT)
  TT <- RT+MT
  maxDeviation <- (file$maxDeviation)
  data <- data.frame(cbind(correctAns,acc,TT,maxDeviation))
  data[,c(3:4)][data[,c(3:4)] == 0] <- 'NA'
  data[,c(3:4)] <- na.approx(data[,c(3:4)])
  
  ##Transform them to z-score
  z_score_RT = abs(scale(RT))
  z_score_TT = abs(scale(TT))
  z_score_Dev = abs((maxDeviation-mean(maxDeviation))/sd(maxDeviation))
  
  
  ##Select different variables
  all_data <- data.frame(cbind(correctAns,acc,RT,MT,z_score_TT,z_score_Dev,TT))
  colnames(all_data) <- c("correctAns","acc","RT","MT","z_TT","z_maxDeviation","TT")
  
  data_RT <- data.frame(cbind(correctAns,acc,z_score_RT))
  colnames(data_RT) <- c("correctAns","acc","z_RT")
  
  data_TT <- data.frame(cbind(correctAns,acc,z_score_TT))
  colnames(data_TT) <- c("correctAns","acc","z_TT")
  
  data_Dev <- data.frame(cbind(correctAns,acc,z_score_Dev))
  colnames(data_Dev) <- c("correctAns","acc","z_maxDeviation")
  
  return(list(all_data,data_RT,data_TT,data_Dev))
  
}

#################
### Function takes data file from the last function, create a kernel function,
### and produce a new column, showing if each trial is in/out of zone
#################
makets <- function(data){
  
  ts_RT <- ts(data)
  x.info = attr(ts_RT , "tsp") 
  tt = seq(0, 439, by=1)
  yy= data[,3]
  
  ks1 = ksmooth(tt, yy, "normal", 20 , x.points=tt)
  ks1 <- data.frame(ks1)
  
  data_1 <- cbind(data,ks1[,2])
  names(data_1)[names(data_1) == 'ks1[, 2]'] <- 'Smoothed_VTC'
  
  med = median(data_1$Smoothed_VTC)
  
  # # Divide into in the zone and out of the zone
  data_1["Smoothed_VTC"][data_1["Smoothed_VTC"] == med |data_1["Smoothed_VTC"] > med] <- 'Out_zone'
  data_1["Smoothed_VTC"][data_1["Smoothed_VTC"] < med] <- 'In_zone'
  
  
  return(data_1)
}
###########################
####
#### Mean rate for each go
###########################
analyze <- function(data,variable.name){
  
  # Remove the first 100 variables
  data <- data %>% slice(-c(1:100))
  
  ## In no-go trials
  error.no.go.in = abs(1-mean(data$acc[data$correctAns==0&data$Smoothed_VTC=='In_zone'],na.rm=T))
  error.no.go.out = abs(1-mean(data$acc[data$correctAns==0&data$Smoothed_VTC=='Out_zone'],na.rm=T))
  
  no.go.table <- cbind(error.no.go.in,error.no.go.out,variable = variable.name)
  
  ## In go trials
  error.go.in = abs(1-mean(data$acc[data$correctAns==1&data$Smoothed_VTC=='In_zone'],na.rm=T))
  error.go.out = abs(1-mean(data$acc[data$correctAns==1&data$Smoothed_VTC=='Out_zone'],na.rm=T))
  
  go.table <- cbind(error.go.in,error.go.out,variable = variable.name)
  
  table <- cbind(no.go.table,go.table)
}

test <- process.file2("MovementData_12.mat")
# testfile <- test[[1]]
# write_xlsx(testfile, "C:\\Users\\nhith\\OneDrive\\Documents\\Research\\Moher\\tryTT.xlsx")
movement_1_TT <- makets(test[[3]])
movement_1_Dev <- makets(test[[4]])

errorTT <- analyze(movement_1_TT ,'TT') 
errorDev <- analyze(movement_1_Dev ,'Dev')

newdata<- data.frame(rbind(errorTT,errorDev))
newdata <- newdata %>% select(-variable) %>% mutate(Participant = 12L)

all.data <- append(c,newdata)
all.data <- data.frame(all.data)
a <- all.data %>% select(c(7:12))
all.data  <- all.data %>% select(-c(7:12))
colnames(a) <- colnames(all.data)
hello <- rbind(all.data,a)
c <- unique(hello)
#write_xlsx(c, "C:\\Users\\nhith\\OneDrive\\Documents\\Research\\Moher\\dataTT.xlsx")

# error.table1<- data.frame(rbind(errorTT,errorDev))
# error.table1 <- error.table1 %>% select(-variable) %>% mutate(Participant = 1)



