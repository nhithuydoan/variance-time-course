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
library(gsignal)


######################
##### Function takes file and convert all variables to z-score, and produce 
##### an output for kernel function
######################
process.file <- function(filename){
  
  ## Read file, choose variables, changes names
  
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
#############################
####### x= time (1-440), y = zRT
######
############################
test <- process.file("MovementData_1.mat")
abc <- test[[2]]

#################
### Function takes data file from the last function, create a kernel function,
### and produce a new column, showing if each trial is in/out of zone
#################
# makets <- function(data){
#   require(plotly)
#   
#   ts_RT <- ts(data)
#   x.info = attr(ts_RT , "tsp") 
#   tt = seq(0, 439, by=1)
#   yy= data[,3]
#   
#   ks1 = ksmooth(tt, yy, "normal", 20 , x.points=tt)
#   ks1 <- data.frame(ks1)
#   
#   data_1 <- cbind(data,ks1[,2])
#   names(data_1)[names(data_1) == 'ks1[, 2]'] <- 'Smoothed_VTC'
#   
#   med = median(data_1$Smoothed_VTC, na.rm = TRUE)
#   
#   # # Divide into in the zone and out of the zone
#   data_1["Smoothed_VTC"][data_1["Smoothed_VTC"] == med |data_1["Smoothed_VTC"] > med] <- 'Out_zone'
#   data_1["Smoothed_VTC"][data_1["Smoothed_VTC"] < med] <- 'In_zone'
#   return(data_1)
# }


# I am working on this
#Check the math, gausian, bandwidth, and the threshold 
require(plotly)
abc <- test[[2]]

ts_RT <- ts(abc)
x.info = attr(ts_RT , "tsp") 
tt = seq(0, 439, by=1)
yy= abc[,3]

ks1 = ksmooth(tt, yy, "normal",5 , x.points=tt)
ks1 <- data.frame(ks1)

data.fmt = list(color=rgb(0.8,0.8,0.8,0.8), width=4)
line.fmt = list(dash="solid", width = 1.5, color= 'firebrick')

require(plotly)

p.ks = plot_ly(x=tt, y=yy, type="scatter", mode="lines", line=data.fmt, name="Raw data")
p.ks = add_lines(p.ks, x=ks1$x, y=ks1$y, line=line.fmt,name="Avg. 20 trials")
p.ks %>% layout(p.ks, title = "Kernel smoother", xaxis = list(title = 'Trials'), 
             yaxis = list(title = 'Normalized RT')) 
print(p.ks)

###########################
######## Mean rate for each go/ no-go trials
###########################
analyze <- function(data,variable.name){
  
  # Remove the first 100 variables
  #data <- data %>% slice(-c(1:100))
  
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
abc <- test[[2]]
movement_1_RT <- makets(test[[2]])
#write_xlsx(movement_1_RT, "C:\\Users\\nhith\\OneDrive\\Documents\\Research\\Moher\\5RT.xlsx")
movement_1_MT <- makets(test[[3]])
movement_1_Dev <- makets(test[[4]])
errorRT <- analyze(movement_1_RT ,'RT')  
errorMT <- analyze(movement_1_MT ,'MT') 
errorDev <- analyze(movement_1_Dev ,'Dev')

newdata<- data.frame(rbind(errorRT,errorMT,errorDev))
newdata <- newdata %>% select(-variable) %>% mutate(Participant = 10L)
all.data <- append(c,newdata)
all.data <- data.frame(all.data)
a <- all.data %>% select(c(7:12))
all.data  <- all.data %>% select(-c(7:12))
colnames(a) <- colnames(all.data)
b <- rbind(all.data,a)
c <- unique(b)

#write_xlsx(c, "C:\\Users\\nhith\\OneDrive\\Documents\\Research\\Moher\\NEW_10sub_nocut.xlsx")

# error.table1<- data.frame(rbind(errorRT,errorMT,errorDev))
# error.table1 <- error.table1 %>% select(-variable) %>% mutate(Participant = 1)


# newdata<- data.frame(rbind(errorRT,errorMT,errorDev))
# newdata <- newdata %>% select(-variable) %>% mutate(Participant = 2)
# all.data <- append(error.table1,newdata)
# all.data <- data.frame(all.data)
# a <- all.data %>% select(c(7:12))
# all.data  <- all.data %>% select(-c(7:12))
# colnames(a) <- colnames(all.data)
# hello <- rbind(all.data,a)
# c <- unique(hello)
                                                             2))
