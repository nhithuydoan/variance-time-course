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

## Import data
individual_data <-  read.delim("128_Block2.txt", header = FALSE, sep = "\t", dec = ".")
colnames(individual_data) <- c("V1","RT","ACC","Trial_type","Go1","V2","V3","V4","V5")
individual_data <- data.table(individual_data)

## 1. replace missing data with the RTs of the two surrounding trials (linearly interpolated)
individual_data$RT[individual_data$RT == 0] <- 'NA'
individual_data$RT <- na.approx(individual_data$RT)

## 2. z-transform all values: make the 
individual_data <- individual_data %>% mutate(z_score = abs((RT-mean(RT))/sd(RT))) %>%
  select(RT,z_score, Go1, ACC, Trial_type,everything())

#combine with the main data
#individual_data <- data.frame(cbind(individual_data,sec))
individual_data <- data.frame(individual_data)


ts_RT <- ts(individual_data)
x.info = attr(ts_RT , "tsp") 
tt = seq(0, 219, by=1)
data.fmt = list(color=rgb(0.8,0.8,0.8,0.8), width=4)
line.fmt = list(dash="solid", width = 1.5, color= 'firebrick')

# fig.add_scattergl(x=xs, y=df.y.where(df.y >= 50), line={'color': 'red'})
# fig.add_scattergl(x=xs, y=df.y, line={'color': 'black'})

yy= individual_data$z_score

require(plotly)
ks1 = ksmooth(tt, yy, "normal", 20 , x.points=tt)
p.ks = plot_ly(x=tt, y=yy, type="scatter", mode="lines", line=data.fmt, name="Raw data")
p.ks = add_lines(p.ks, x=ks1$x, y=ks1$y, line=line.fmt,name="Avg. 20 trials")
p.ks %>% layout(p.ks, title = "Kernel smoother", xaxis = list(title = 'Trials'), 
                yaxis = list(title = 'Normalized RT')) 
print(p.ks)


ks1 <- data.frame(ks1)
#colnames(ks1) <- c("","Smoothed_VTC")
individual_data <- cbind(individual_data,ks1[,2])
names(individual_data)[names(individual_data) == 'ks1[, 2]'] <- 'Smoothed_VTC'
a = median(individual_data$Smoothed_VTC)
individual_data["Smoothed_VTC"][individual_data["Smoothed_VTC"] == a | individual_data["Smoothed_VTC"] > a] <- 'Out_zone'
individual_data["Smoothed_VTC"][individual_data["Smoothed_VTC"] < a] <- 'In_zone'

# individual_data["Smoothed_VTC"][individual_data["Smoothed_VTC"] == 0.6656803 | individual_data["Smoothed_VTC"] > 0.6656803] <- 'Out_zone'
# individual_data["Smoothed_VTC"][individual_data["Smoothed_VTC"] < 0.6656803] <- 'In_zone'

table(individual_data$Smoothed_VTC)
#write.table(individual_data, file = "New128_Block2.txt", sep = " ")


library(R.matlab)

######################
##### Function takes file and convert all variables to z-score, and produce 
##### an output for kernel function
######################
process.file <- function(filename){
  require(tidyverse)
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

test <- process.file("MovementData_2.mat") 
#View(test[[2]])
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
  #data_1$VTC <- data_1$Smoothed_VTC
  med = median(data_1$Smoothed_VTC)
  
  # # Divide into in the zone and out of the zone
  data_1["Smoothed_VTC"][data_1["Smoothed_VTC"] == med |data_1["Smoothed_VTC"] > med] <- 'Out_zone'
  data_1["Smoothed_VTC"][data_1["Smoothed_VTC"] < med] <- 'In_zone'
  
  
  return(data_1)
}

# median(movement_1_RT$z_RT[movement_1_RT$Smoothed_VTC=='In_zone'])
# 
# ts_RT <- ts(test[[2]])
# x.info = attr(ts_RT , "tsp") 
# tt = seq(0, 439, by=1)
# yy= test[[2]][,3]
# data.fmt = list(color=rgb(0.8,0.8,0.8,0.8), width=4)
# line.fmt = list(dash="solid", width = 1.5, color= 'firebrick')
# 
# yy= test[[2]]$z_RT
# 
# require(plotly)
# ks1 = ksmooth(tt, yy, "normal", 20 , x.points=tt)
# p.ks = plot_ly(x=tt, y=yy, type="scatter", mode="lines", line=data.fmt, name="Raw data")
# p.ks = add_lines(p.ks, x=ks1$x, y=ks1$y, line=line.fmt,name="Avg. 20 trials")
# p.ks %>% layout(p.ks, title = "Kernel smoother", xaxis = list(title = 'Trials'), 
#                 yaxis = list(title = 'Normalized RT')) 
# print(p.ks)


movement_1_RT <- makets(test[[2]])
movement_1_MT <- makets(test[[3]])
movement_1_Dev <- makets(test[[4]])


################### 
###
###  No-go trials
###
##################
cal_error <- function(file,variable.name){
  require(tidyverse)
  #no.go: in the zone rate and out of the zone
  no.go <- file  %>% filter(correctAns == 0)
  
  #No go in the zone error rate
  no.go.in <- no.go %>% filter(Smoothed_VTC == 'In_zone') %>%
    count(acc) %>% filter(acc == 0)
  error.no.go.in <- (no.go.in$n)/46
  
  #No go out of the zone error rate
  no.go.out <- no.go %>% filter(Smoothed_VTC == 'Out_zone') %>%
    count(acc) %>% filter(acc == 0)
  error.no.go.out <- (no.go.out$n)/46
  
  no.go.table <- cbind(error.no.go.in,error.no.go.out,variable = variable.name)
  
  #go: in the zone rate and out of the zone
  go <- file  %>% filter(correctAns == 1)
  
  #go in the zone error rate
  go.in <- go %>% filter(Smoothed_VTC == 'In_zone') %>%
    count(acc) %>% filter(acc == 0)
  error.go.in <- (go.in$n)/394
  
  # go out of the zone error rate
  go.out <- go %>% filter(Smoothed_VTC == 'Out_zone') %>%
    count(acc) %>% filter(acc == 0)
  error.go.out <- (go.out$n)/394
  
  go.table <- cbind(error.go.in,error.go.out,variable = variable.name)
  
  table <- cbind(no.go.table,go.table)
  
  
}

errorRT <- cal_error(movement_1_RT ,'RT')  
errorMT <- cal_error(movement_1_MT ,'MT') 
errorDev <- cal_error(movement_1_Dev ,'Dev')

# error.table1<- data.frame(rbind(errorRT,errorMT,errorDev))
# error.table1 <- error.table1 %>% select(-variable) %>% mutate(Participant = 2)
# error.table1 <- error.table1 %>% select(-Participant) 
# error.table1<- t(error.table1)

newdata<- data.frame(rbind(errorRT,errorMT,errorDev))
newdata <- newdata %>% select(-variable) 
newdata <- t(newdata )

all.data <- append(error.table1,newdata)
all.data <- data.frame(all.data)
all.data$error.no.go.in <- t(all.data$error.no.go.in)
View(all.data$error.no.go.in)
all.data[nrow(all.data) + 3,] <- newdata






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
y <- na.omit(abc$zRT)
x <- seq(1, seq_length(y), by=1)

test.data <- data.frame(x,y)

peak <- NULL
for(i in 1:length(y)) {
  isLeftEdge <- i <= 10
  isRightEdge <- i + 10 > length(y)
  if(isLeftEdge) {
    window.x <- x[1:(i+10)]
    window.y <- y[1:(i+10)]
  } else if(isRightEdge) {
    window.x <- x[(i-10):(length(y))]
    window.y <- y[(i-10):(length(y))]
  } else {
    window.x <- x[(i-10):(i+10)]
    window.y <- y[(i-10):(i+10)]
  }
  peak[i] <- fwhm(seq_len(length(window.y)),window.y)
}
test.data <- cbind(test.data,peak)
plot(y)
plot(peak)

ggplot(data = test.data,aes(x,y)) + geom_line()
ggplot(data = test.data,aes(x,peak)) + geom_line()