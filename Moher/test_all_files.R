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
if(!require('plotly')) {
  install.packages('plotly')
  library('plotly')
}
if(!require('plotly')) {
  install.packages('plotly')
  library('plotly')
}
if(!require('smooth')) {
  install.packages('smooth')
  library('smooth')
}

# for (s in 1:length(list)){
#   test <- process.file(s) 
#   movement_1_RT <- makets(test[[2]])
#   movement_1_MT <- makets(test[[3]])
#   movement_1_Dev <- makets(test[[4]])
#   return(movement_1_RT )
# }
# 
# 
# list <- c("MovementData_1.mat","MovementData_2.mat","MovementData_3.mat","MovementData_4.mat",
#           "MovementData_5.mat","MovementData_6.mat","MovementData_7.mat","MovementData_8.mat",
#           "MovementData_9.mat","MovementData_10.mat","MovementData_11.mat","MovementData_12.mat",
#           "MovementData_13.mat","MovementData_14.mat","MovementData_15.mat","MovementData_16.mat",
#           "MovementData_17.mat","MovementData_18.mat","MovementData_19.mat","MovementData_20.mat",
#           "MovementData_21.mat","MovementData_22.mat","MovementData_23.mat","MovementData_24.mat",
#           "MovementData_25.mat","MovementData_26.mat","MovementData_27.mat","MovementData_28.mat",
#           "MovementData_29.mat","MovementData_30.mat","MovementData_31.mat","MovementData_32.mat",
#           "MovementData_33.mat","MovementData_34.mat","MovementData_35.mat","MovementData_36.mat",
#           "MovementData_37.mat","MovementData_38.mat","MovementData_39.mat","MovementData_40.mat",
#           "MovementData_41.mat")

######################
##### Function takes file and convert all variables to z-score, and produce 
##### an output for kernel function
######################
process.file <- function(filename){
  
  ## Read file, choose variables, changes names
  
  # file <- R.matlab::readMat(filename)
  # correctAns <- t(file$correctAns)
  # acc <- (file$acc)
  # RT <- (file$RT)
  # MT <- (file$MT)
  # maxDeviation <- (file$maxDeviation)
  # data <- data.frame(cbind(correctAns,acc,RT,MT,maxDeviation))
  # colnames(data) <- c("correctAns","acc","RT","MT","Dev")
  # data[,c(3:5)][data[,c(3:5)] == 0 | data[,c(3:5)] < 0.1 ] <- 'NA'
  # data[,c(3:5)] <- na.approx(data[,c(3:5)])
  # data[,'zRT'] <- abs(scale(data[,'RT']))
  # data[,'zMT'] <- abs(scale(data[,'MT']))
  # data[,'zDev'] <- abs(scale(data[,'Dev']))
  # 
  # all_data <- data
  # # data_RT <- data %>% select("correctAns","acc","zRT")
  # data_RT <- data %>% select("correctAns","acc","zRT")
  # data_MT <- data %>% select("correctAns","acc","zMT")
  # data_Dev <- data %>% select("correctAns","acc","zDev")
  # return(list(all_data,data_RT,data_MT,data_Dev))
  file <- R.matlab::readMat(filename)
  correctAns <- t(file$correctAns)
  acc <- (file$acc)
  RT <- (file$RT)
  MT <- (file$MT)
  maxDeviation <- (file$maxDeviation)
  data <- data.frame(cbind(correctAns,acc,RT,MT,maxDeviation))
  colnames(data) <- c("correctAns","acc","RT","MT","Dev")
  data[,c(3:5)][data[,c(3:5)] == 0 | data[,c(3:5)] < 0.1 ] <- 'NA'
  data[,c(3:5)] <- na.approx(data[,c(3:5)])
  data[,'zRT'] <- abs(scale(data[,'RT']))
  data[,'zMT'] <- abs(scale(data[,'MT']))
  data[,'zDev'] <- abs(scale(data[,'Dev']))
  
  all_data <- data
  data_RT <- data %>% select("correctAns","acc","zRT")
  data_MT <- data %>% select("correctAns","acc","zMT")
  data_Dev <- data %>% select("correctAns","acc","zDev")
  return(list(all_data,data_RT,data_MT,data_Dev))
  
}
# hi2<- process.file("MovementData_5.mat")
# f <- hi2[[2]]
# write_xlsx(f, "C:\\Users\\nhith\\OneDrive\\Documents\\Research\\Moher\\5RT_testing.xlsx")

#################
### Function takes data file from the last function, create a kernel function,
### and produce a new column, showing if each trial is in/out of zone
#################
makets <- function(data){
  require(plotly)
  
  ts_RT <- ts(data)
  x.info = attr(ts_RT , "tsp") 
  tt = seq(0, 439, by=1)
  yy= data[,3]
  
  ks1 = ksmooth(tt, yy, "normal", 20 , x.points=tt)
  ks1 <- data.frame(ks1)
  
  data_1 <- cbind(data,ks1[,2])
  names(data_1)[names(data_1) == 'ks1[, 2]'] <- 'Smoothed_VTC'
  
  med = median(data_1$Smoothed_VTC, na.rm = TRUE)
  
  # # Divide into in the zone and out of the zone
  data_1["Smoothed_VTC"][data_1["Smoothed_VTC"] == med |data_1["Smoothed_VTC"] > med] <- 'Out_zone'
  data_1["Smoothed_VTC"][data_1["Smoothed_VTC"] < med] <- 'In_zone'
  return(data_1)
}

### h = 2 can 2 c???

test <- process.file("MovementData_1.mat")
data <- test[[2]]
require(plotly)

ts_RT <- ts(data)
x.info = attr(ts_RT , "tsp")
tt = seq(0, 439, by=1)
yy= data[,3]
z <- sd(yy,na.rm = TRUE)
#bw = 2*sqrt(2*log(2))*z
data.fmt = list(color=rgb(0.8,0.8,0.8,0.8), width=4)
line.fmt = list(dash="solid", width = 1.5, color= 'firebrick')

ks1 = ksmooth(tt, yy, "normal", 20 , x.points=tt, bandwidth = 0)
p.ks = plot_ly(x=tt, y=yy, type="scatter", mode="lines", line=data.fmt, name="Raw data")
p.ks = add_lines(p.ks, x=ks1$x, y=ks1$y, line=line.fmt,name="Bandwidth = FWHM")
p.ks %>% layout(p.ks, title = "Kernel smoother", xaxis = list(title = 'Trials'),
             yaxis = list(title = 'Normalized RT'))
print(p.ks)

ks1 <- data.frame(ks1)
# 
# data_1 <- cbind(data,ks1[,2])
# names(data_1)[names(data_1) == 'ks1[, 2]'] <- 'Smoothed_VTC'
# 
# med = median(data_1$Smoothed_VTC, na.rm = T)
# 
# # # Divide into in the zone and out of the zone
# data_1["Smoothed_VTC"][data_1["Smoothed_VTC"] == med |data_1["Smoothed_VTC"] > med] <- 'Out_zone'
# data_1["Smoothed_VTC"][data_1["Smoothed_VTC"] < med] <- 'In_zone'
# return(data_1)
###########################
####
#### Mean rate for each go
###########################
analyze <- function(data,variable.name){
  
  # Remove the first 100 variables
  data <- data %>% slice(-c(1:100))
  
  ## In no-go trials
  error.no.go.in = abs(1-mean(data$acc[data$correctAns==0&data$Smoothed_VTC=='In_zone'], na.rm = TRUE))
  error.no.go.out = abs(1-mean(data$acc[data$correctAns==0&data$Smoothed_VTC=='Out_zone'], na.rm = TRUE))
  
  no.go.table <- cbind(error.no.go.in,error.no.go.out,variable = variable.name)
  
  ## In go trials
  error.go.in = abs(1-mean(data$acc[data$correctAns==1&data$Smoothed_VTC=='In_zone'],na.rm=T))
  error.go.out = abs(1-mean(data$acc[data$correctAns==1&data$Smoothed_VTC=='Out_zone'],na.rm=T))
  
  go.table <- cbind(error.go.in,error.go.out,variable = variable.name)
  
  table <- cbind(no.go.table,go.table)
}

########################
test <- process.file("MovementData_1.mat")
movement_1_RT <- makets(test[[2]])
#write_xlsx(movement_1_RT, "C:\\Users\\nhith\\OneDrive\\Documents\\Research\\Moher\\5RT.xlsx")
movement_1_MT <- makets(test[[3]])
movement_1_Dev <- makets(test[[4]])
errorRT <- analyze(movement_1_RT ,'RT')  
errorMT <- analyze(movement_1_MT ,'MT') 
errorDev <- analyze(movement_1_Dev ,'Dev')

newdata<- data.frame(rbind(errorRT,errorMT,errorDev))
newdata <- newdata %>% select(-variable) %>% mutate(Participant = 5L)
all.data <- append(c,newdata)
all.data <- data.frame(all.data)
a <- all.data %>% select(c(7:12))
all.data  <- all.data %>% select(-c(7:12))
colnames(a) <- colnames(all.data)
hello <- rbind(all.data,a)
c <- unique(hello)

#write_xlsx(c, "C:\\Users\\nhith\\OneDrive\\Documents\\Research\\Moher\\error_rate_first20_cut100trials.xlsx")

error.table1<- data.frame(rbind(errorRT,errorMT,errorDev))
error.table1 <- error.table1 %>% select(-variable) %>% mutate(Participant = 2)


# newdata<- data.frame(rbind(errorRT,errorMT,errorDev))
# newdata <- newdata %>% select(-variable) %>% mutate(Participant = 2)
# all.data <- append(error.table1,newdata)
# all.data <- data.frame(all.data)
# a <- all.data %>% select(c(7:12))
# all.data  <- all.data %>% select(-c(7:12))
# colnames(a) <- colnames(all.data)
# hello <- rbind(all.data,a)
# c <- unique(hello)

