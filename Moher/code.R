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
#install.packages("writexl")
library("writexl")
######################
##### Function takes file and convert all variables to z-score, and produce 
##### an output for kernel function
######################
process.file <- function(filename){
  #require(tidyverse)
  ## Read file, choose variables, changes names
  file <- R.matlab::readMat(filename)
  correctAns <- t(file$correctAns)
  acc <- (file$acc)
  RT <- (file$RT)
  MT <- (file$MT)
  maxDeviation <- (file$maxDeviation)
  data <- data.frame(cbind(correctAns,acc,RT,MT,maxDeviation))
  data[,c(3:5)][data[,c(3:5)] == 0] <- 'NA'
  data[,c(3:5)] <- na.approx(data[,c(3:5)])

  ##Transform them to z-score
  z_score_RT = abs((RT-mean(RT))/sd(RT))
  z_score_MT = abs((MT-mean(MT))/sd(MT))
  z_score_Dev = abs((maxDeviation-mean(maxDeviation))/sd(maxDeviation))

  ##Select different variables
  all_data <- data.frame(cbind(correctAns,acc,z_score_RT,z_score_MT,z_score_Dev))
  colnames(all_data) <- c("correctAns","acc","z_RT","z_MT","z_maxDeviation")

  data_RT <- data.frame(cbind(correctAns,acc,z_score_RT))
  colnames(data_RT) <- c("correctAns","acc","z_RT")

  data_MT <- data.frame(cbind(correctAns,acc,z_score_MT))
  colnames(data_MT) <- c("correctAns","acc","z_MT")

  data_Dev <- data.frame(cbind(correctAns,acc,z_score_Dev))
  colnames(data_Dev) <- c("correctAns","acc","z_maxDeviation")

  return(list(all_data,data_RT,data_MT,data_Dev))

}

test <- process.file("MovementData_1.mat") 

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
   
  # Divide into in the zone and out of the zone
  data_1["Smoothed_VTC"][data_1["Smoothed_VTC"] == med |data_1["Smoothed_VTC"] > med] <- 'Out_zone'
  data_1["Smoothed_VTC"][data_1["Smoothed_VTC"] < med] <- 'In_zone'

  return(data_1)
}

movement_1_RT <- makets(test[[2]])
movement_1_MT <- makets(test[[3]])
movement_1_Dev <- makets(test[[4]])

analyze <- function(data,variable.name){
  
  # Remove the first 100 variables
  # data <- data %>% slice(-c(1:100))
  
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

errorRT <- analyze(movement_1_RT ,'RT')  
errorMT <- analyze(movement_1_MT ,'MT') 
errorDev <- analyze(movement_1_Dev ,'Dev')

#################### Run the code 
test <- process.file("MovementData_20.mat")
movement_1_RT <- makets(test[[2]])
movement_1_MT <- makets(test[[3]])
movement_1_Dev <- makets(test[[4]])
# errorRT <- cal_error(movement_1_RT ,'RT')
# errorMT <- cal_error(movement_1_MT ,'MT')
# errorDev <- cal_error(movement_1_Dev ,'Dev')


# error.table1<- data.frame(rbind(errorRT,errorMT,errorDev))
# error.table1 <- error.table1 %>% select(-variable) %>% mutate(Participant = 1)

newdata<- data.frame(rbind(errorRT,errorMT,errorDev))
newdata <- newdata %>% select(-variable) %>% mutate(Participant = 20)
all.data <- append(c,newdata)
all.data <- data.frame(all.data)
a <- all.data %>% select(c(7:12))
all.data  <- all.data %>% select(-c(7:12))
colnames(a) <- colnames(all.data)
hello <- rbind(all.data,a)
c <- unique(hello)


#write.table(c, file = "error_rate_2.txt", sep = " ")
# all.data <- hello

  
class(error_rate)

write_xlsx(error_rate, "C:\\Users\\nhith\\OneDrive\\Documents\\Research\\Moher\\error_rate_excel.xlsx")






error.table1 <- error.table1 %>% gather(Error,Value,error.no.go.in,error.no.go.out)
error.table1$Error <- factor(error.table1$Error)
levels(error.table1$Error) <- c("In the zone", "Out of the zone")
#round(error.table1$Value,2)

error.table1$Value <- sapply(error.table1$Value, as.numeric)
ggplot(error.table1,aes(variable,Value, fill = Error))+
  geom_col(position = 'dodge')+labs(x= 'Variable',y='Error rate',fill = "",
                                    title = "Comission error rate",
                                    subtitle = "Data from Movement_1")+
  scale_fill_brewer(palette = "Paired")+theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

# ################### 
# ###
# ###  Go trials
# ###
# ##################
# cal_error <- function(file,variable.name){
#   #go: in the zone rate and out of the zone
#   go <- file  %>% filter(correctAns == 1)
# 
#   #go in the zone error rate
#   go.in <- go %>% filter(Smoothed_VTC == 'In_zone') %>%
#     count(acc) %>% filter(acc == 0)
#   error.go.in <- (go.in$n)/394
# 
#   # go out of the zone error rate
#   go.out <- go %>% filter(Smoothed_VTC == 'Out_zone') %>%
#     count(acc) %>% filter(acc == 0)
#   error.go.out <- (go.out$n)/394
# 
#   table <- cbind(error.go.in,error.go.out,variable = variable.name)
# }
# 
# errorRT2 <- cal_error(movement_1_RT ,'RT')  
# errorMT2 <- cal_error(movement_1_MT ,'MT') 
# errorDev2 <- cal_error(movement_1_Dev ,'Dev')
# error.table2 <- data.frame(rbind(errorRT2,errorMT2,errorDev2)) 
# 
# error.table2 <- error.table2 %>% gather(Error,Value,error.go.in,error.go.out)
# error.table2$Error <- factor(error.table2$Error)
# levels(error.table2$Error) <- c("In the zone", "Out of the zone")
# error.table2$Value <- sapply(error.table2$Value, as.numeric)
# ggplot(error.table2,aes(variable,Value, fill = Error))+
#   geom_col(position = 'dodge')+labs(x= 'Variable',y='Error rate',fill = "",
#                                     title = "Omission error rate",
#                                     subtitle = "Data from Movement_1")+
#   scale_fill_brewer(palette = "Paired")+theme_classic()+
#   theme(plot.title = element_text(hjust = 0.5))
# 
# 
# 
# 
# 
