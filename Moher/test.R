library(ggplot2)
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

#Values
Mean = 0

SD = 1
Values = seq(-3,3, 0.1)

#Function
Gaussian_F = function(Mean, Standard_deviation, Observed_values){
  y = 
    exp(-(Observed_values-Mean)^2/(2*Standard_deviation^2))/
    (Standard_deviation*sqrt(2*pi))
  
  return(y)
}
curve = Gaussian_F(Mean, SD, Values)

#Data
GBell = data.frame(Y = curve, X = Values)

#Plot
ggplot(GBell, aes(x = X, y = Y))+
  geom_line()+
  labs(x="Observed values", y="f(x)")+
  geom_vline(xintercept = 0)+
  annotate(
    "text", x=0.35, y=0.2, label="bold(Mean)", parse=T
  )+
  annotate(
    "text", x=0.35, y=0.15, label="bold(Mode)", parse=T
  )+
  annotate(
    "text", x=0.43, y=0.1, label="bold(Median)", parse=T
  )+
  theme_classic()+
  theme(axis.title.x = element_text(face = "bold"), 
        axis.title.y = element_text(face = "bold"),
        text = element_text(
          family="Times New Roman")
  )

ggplot(iris, aes(iris$Petal.Length, fill=iris$Species))+
  geom_histogram(binwidth = 0.05)+
  labs(x="Petal Length", y = "Frequency", fill="Species")+
  theme_classic()+
  theme(
    axis.title.x = element_text(face = "bold"), 
    axis.title.y = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    text = element_text(
      family="Times New Roman")
  )

#########################
library(tidyverse)
#Data
means=iris %>%
  group_by(Species) %>%
  summarise(M = mean(Petal.Length), SD = sd(Petal.Length), N = n())

#Sample size of 50. T Distribution intervals (find the 95% interval)
means$error=qt(0.975, df=50-1)*means$SD/sqrt(means$N)
means$upper=means$M+means$error
means$lower=means$M-means$error

ggplot(data=means)+
  geom_crossbar(
    aes(x=Species, y=M, ymin=lower, ymax=upper, fill=Species), 
    show.legend = F)+
  scale_color_manual(values=c("blue", "green", "yellow"))+
  labs(x="Species", y="Mean and CI 95%")+
  theme_classic()+
  theme(
    axis.title.x = element_text(face = "bold"), 
    axis.title.y = element_text(face = "bold"),
    text = element_text(
      family="Times New Roman")
  )
########################
ggplot(iris, aes(Petal.Length, fill=Species, colour = Species))+
  geom_density(alpha=0.1, size=0.8)+
  scale_fill_manual(values=c("navy", "darkgreen", "darkred"))+
  scale_color_manual(values=c("navy", "darkgreen", "darkred"))+
  geom_vline(xintercept = c(means$M))+
  annotate("rect",xmin=means$lower, xmax=means$upper, ymin=0, ymax=Inf, alpha=0.2)+
  annotate("text", x = 1.8, y=2.7, label="bold(Mean)", parse=T)+
  annotate("text", x = 1.87, y=2.5, label="CI 95%")+
  labs(x="Petal Length", y = "Density", fill="Species")+
  theme_classic()+
  theme(
    axis.title.x = element_text(face = "bold"), 
    axis.title.y = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    text = element_text(
      family="Times New Roman")
  )

#histogram is frequency, this bell plot is density. Density is the probability
# that a random variable falls in a range of values

#smooth band curve: h


#One point Gaussian Kernel
View(iris)
point = iris$Petal.Length[[7]]
point

#Some range
range = seq(-3, 6, 0.01)

#A simple bandwidth
b = 1

#The estimation
GaussianPoint = data.frame(
  X = range,
  Y = 
    exp(-(range-point)^2/(2*b^2))/(b*sqrt(2*pi))/
    length(point)
)
#A song of Plots and Points
ggplot()+
  geom_area(data = GaussianPoint, aes(x=X, y=Y), fill = "blue", alpha=0.5)+
  geom_point(aes(x=1.4, y=max(GaussianPoint$Y)))+
  theme_classic()+
  theme(
    axis.title.x = element_text(face = "bold"), 
    axis.title.y = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    text = element_text(
      family="Times New Roman", size=15)
  )+ labs(title = "Gaussian Density of a point")


#Two-point examples
#A point
point1 = iris$Petal.Length[7]
point2 = iris$Petal.Length[65]
point1
## [1] 1.4
point2
## [1] 3.6
#An exaggerated range
range = seq(-10, 10, 0.01)

#A simple bandwidth
b = 1

#Number of points
n = 2

#The estimation of points
GaussianPoints = data.frame(
  X = range,
  Y1 = exp(-(range-point1)^2/(2*b^2))/(b*sqrt(2*pi)),
  Y2 = exp(-(range-point2)^2/(2*b^2))/(b*sqrt(2*pi))
)

#Kernel density
GaussianPoints = GaussianPoints%>%
  mutate(Kernel = (Y1+Y2)/n)


#Two-point plot
ggplot()+
  geom_area(data = GaussianPoints, aes(x=X, y=Y1, fill = "Point 1"), alpha=0.5)+
  geom_area(data = GaussianPoints, aes(x=X, y=Y2, fill = "Point 2"), alpha=0.5)+
  geom_area(data = GaussianPoints, aes(x=X, y=Kernel, fill = "Kernel"), alpha=0.7)+
  scale_fill_manual(values = c("red", "yellow", "blue"))+
  geom_point(aes(x=1.4, y=max(GaussianPoints$Y1)))+
  geom_point(aes(x=3.6, y=max(GaussianPoints$Y2)))+
  labs(x = "Range", y="Densities", fill="Estimations")+
  theme_classic()+
  theme(
    axis.title.x = element_text(face = "bold"), 
    axis.title.y = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    text = element_text(
      family="Times New Roman", size=15)
  )

# All the points
#The function
Gauss_K = function(Values, Range, h=1){
  library(dplyr) 
  
  #Result
  densities = data.frame()
  #Temporal variable
  Temp = data.frame()
  #One by one variable
  V = vector()
  
  for (i in 1:length(Values))
  {
    #The value
    V = Values[i]
    #Gaussian Function
    Temp =
      data.frame(
        Density = exp(-(Range-V)^2/(2*h^2))/(h*sqrt(2*pi)),
        Range = Range,
        Value = as.factor(paste0("X",i))
      )
    densities = rbind(densities, Temp)
  }
  Densities1 = densities
  Densities2 = densities%>%
    #Sum K by range value!
    group_by(Range)%>%
    summarise(Bell_sum = sum(Density))%>%
    #Normalization
    mutate(Kernel_Density = Bell_sum /length(Values))
  #Data Frame with points
  Points = Densities1%>%
    group_by(Value)%>%
    summarise(Y = max(Density))%>%
    mutate(Value = Values)
  
  
  return(
    list(Densities1, Densities2, Points)
  )
}
set.seed(1234)
#iris %>% group_by(Petal.Length)
S = sample(iris$Petal.Length,4)
S
R = seq(min(S)-5, max(S)+5, 0.01)
Points5 = Gauss_K(S, R, h = 0.5)

ggplot(Points5[[2]], aes(x=Range, y = Kernel_Density))+
  geom_area(data = Points5[[1]], aes(x= Range, y=Density, fill = Value), 
            alpha = 0.3, show.legend = F, position = "identity")+
  geom_area(col = "blue")+
  geom_point(data = Points5[[3]], aes(x= Value, y= Y))+
  labs(x="Range", y="Density")+
  theme_classic()+
  theme(
    axis.title.x = element_text(face = "bold"), 
    axis.title.y = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    text = element_text(
      family="Times New Roman", size=15)
  )
#######################################
####
####
#######################################
DF <- data.frame(Site = c("Site1", "Site2", "Site3", "site4"),
                 var1 = c(1,2,0,4), var2 = c(3,6,4,0), 
                 var3 = c(0,6,3,2))
DF
library(dplyr)
DF <-  DF |> mutate(across(.cols = var1:var3, 
                           .fns = ~ifelse(.x == 0, mean(.(x+1)+(x-1), .x)))
DF
#
library(data.table)
library(zoo)
A <- data.table(id = c(1:10),
                Val = c(12,8,7,NA,8,2,3,NA, NA,20))
A


A[, list(Val = na.approx(Val))]


data <- structure(list(ATime = structure(1:15, .Label = c("00:00", "00:05",
                                                          "00:10", "00:15", "00:20", "00:25", "00:30", "00:35", "00:40", 
                                                          "00:45", "00:50", "00:55", "01:00", "01:05", "01:10", "01:15", 
                                                          "01:20", "01:25", "01:30", "01:35", "01:40", "01:45", "01:50", 
                                                          "01:55", "02:00", "02:05", "02:10", "02:15", "02:20", "02:25"), class = "factor"), 
                       ASpeed5 = c(34, 40, 38, 55, 56, 60, 66, 49, 48, 29, 67, 78, 
                                   39, 53, 73), BSpeed5 = c(23, 46, 63, 64, 72, 61, 49, 48, 
                                                            63, 68, 62, 27, 35, 45, 59)), row.names = c(NA, 15L), class = "data.frame")
library(tidyverse)
library(dplyr)

x = seq(1,256,1)
gaus_signal = 1250 * dnorm(x, mean = 100, sd = 10)
noise = rnorm(256, mean = 0, sd = 10)
noisy_signal = gaus_signal + noise
plot(x = x, y = noisy_signal, type = "l", lwd = 2, col = "blue", xlab = "x", ylab = "signal")
lines(x = x, y = gaus_signal, lwd = 2)
s_to_n = max(gaus_signal)/sd(noisy_signal[c(1:50,201:250)])
s_to_n

# #ex_12 <- read_excel()
# earning_2012 <- compile("median-earnings-2012.xlsx")
# earning_2012$major <- earning_2012$major %>% str_replace_all("\\.+","")
# colnames(earning_2012) <- c("Major","Total","Men","Women")
# earning_2012_addSTem <- earning_2012 %>% gather(Gender, Number, Men,Women)


data(camphora)
camphora <- crop(camphora,200,200)
par(mfrow=c(2,2))
image(rot90c(noise.filter(camphora,3,"median")),col=gray(c(0:255)/255), 
      main="median", useRaster=TRUE, axes=FALSE, asp=1)
image(rot90c(noise.filter(camphora,3,"mean")),col=gray(c(0:255)/255), 
      main="mean", useRaster=TRUE, axes=FALSE, asp=1)
image(rot90c(noise.filter(camphora,3,"gaussian")),col=gray(c(0:255)/255), 
      main="gaussian", useRaster=TRUE, axes=FALSE, asp=1)

# ggplot(individual_data_RT, aes(x=sec))+
#   geom_line(aes(y=z))+ labs(x = "Time (mins)", y = "Normalized RT")+
#   theme_classic()+stat_density(adjust = 1, kernel = "gaussian")
# 
# ggplot(individual_data_RT, aes(x=sec))+
#   geom_line(aes(y=RT))+ labs(x = "Time (mins)", y = "RT (not normalized)")+
#   theme_classic()
# 

######################
###Gauss Kernel function
######################
Gauss_K = function(Values, Range, h=1){
  library(dplyr) 
  
  #Result
  densities = data.frame()
  #Temporal variable
  Temp = data.frame()
  #One by one variable
  V = vector()
  
  for (i in 1:length(Values))
  {
    #The value
    V = Values[i]
    #Gaussian Function
    Temp =
      data.frame(
        Density = exp(-(Range-V)^2/(2*h^2))/(h*sqrt(2*pi)),
        Range = Range,
        Value = as.factor(paste0("X",i))
      )
    densities = rbind(densities, Temp)
  }
  Densities1 = densities
  Densities2 = densities%>%
    #Sum K by range value!
    group_by(Range)%>%
    summarise(Bell_sum = sum(Density))%>%
    #Normalization
    mutate(Kernel_Density = Bell_sum /length(Values))
  #Data Frame with points
  Points = Densities1%>%
    group_by(Value)%>%
    summarise(Y = max(Density))%>%
    mutate(Value = Values)
  
  
  return(
    list(Densities1, Densities2, Points)
  )
}

S = (individual_data_RT[1:10,])$z
S
R = seq(min(S), max(S), 0.01)
Points5 = Gauss_K(S, R, h = 1)
#View(Points5)

ggplot(Points5[[2]], aes(x=Range, y = Kernel_Density))+
  geom_area(data = Points5[[1]], aes(x= Range, y=Density, fill = Value), 
            alpha = 0.3, show.legend = F, position = "identity")+
  geom_area(col = "blue")+
  geom_point(data = Points5[[3]], aes(x= Value, y= Y))+
  labs(x="Range", y="Density")+
  theme_classic()+
  theme(
    axis.title.x = element_text(face = "bold"), 
    axis.title.y = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    text = element_text(
      family="Times New Roman", size=15)
  )

# Create a time variable
time = function(no_trials){
  ############################
  ## Function to create a time that matches with the RT time
  ## Take the number of trials as input
  ############################
  n = c(2:no_trials)
  s= 0
  df <- data.frame()
  for (i in 0:length(n))
  {s = (s + 2) #Assume each trial is 2 seconds
  n = s/60
  df <- rbind(df,n)
  print(n)
  }
  return(df)
}

#sec <- time(220)
#sec



#######test files
ts_RT <- ts(test[[2]])
x.info = attr(ts_RT , "tsp") 
tt = seq(0, 439, by=1)
yy= test[[2]]$z_RT

ks1 = ksmooth(tt, yy, "normal", 20 , x.points=tt)
ks1 <- data.frame(ks1)
data_1 <- cbind(test[[2]],ks1[,2])
names(data_1)[names(data_1) == 'ks1[, 2]'] <- 'Smoothed_VTC'
b = median(data_1$Smoothed_VTC)
data_1["Smoothed_VTC"][data_1["Smoothed_VTC"] > b] <- 'Out_zone'
data_1["Smoothed_VTC"][data_1["Smoothed_VTC"] < b|data_1["Smoothed_VTC"] == b ] <- 'In_zone'
table(data_1$Smoothed_VTC)

movement_file <- readMat("MovementData_1.mat")
correctAns <- t(movement_file$correctAns) 
acc <- (movement_file$acc)
RT <- (movement_file$RT)
#z_score <- abs((RT-mean(RT))/sd(RT))
MT <- (movement_file$MT)
maxDeviation <- (movement_file$maxDeviation)

data <- data.frame(cbind(correctAns,acc,RT,MT,maxDeviation))
colnames(data) <- c("correctAns","ACC","RT","MT","maxDeviation")

data[,c(3:5)][data[,c(3:5)] == 0] <- 'NA'
data[,c(3:5)] <- na.approx(data[,c(3:5)])

#data <- data %>% mutate(z_score_chiuchiu= abs((MT-mean(MT))/sd(MT)),
#                       z_score_Dev = abs((maxDeviation-mean(maxDeviation))/sd(maxDeviation)))
# z_score_MT <- abs((data$MT-mean(data$MT))/sd(MT))
# z_score_Dev <- abs((maxDeviation-mean(maxDeviation))/sd(maxDeviation))
data$zRT <- abs(scale(data$RT))


#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
DF <- data.frame(TIME = rep(seq(1:4), each = 3), 
                 Variable = rep(c("Req", "Recv", "Total"), 4),
                 Value = c(12, 10, 18, 9, 10, 14, 18, 20, 23, 4, 2, 8))
DF

ReqAndRecv <- DF %>% filter(Variable %in% c("Req", "Recv"))
Total <- DF %>% filter(Variable == "Total")
ggplot() + geom_col(aes(TIME, Value, fill = Variable), data = ReqAndRecv, 
                    position = "dodge") +
  geom_line(aes(TIME, Value, color = Variable), data = Total)

#######################
no.go <- movement_1_MT %>% filter(correctAns == 0) %>% count(acc) %>% filter(acc == 0)
error.nogo <- (no.go$n)/46

go <- movement_1_MT%>% filter(correctAns == 1)%>% count(acc) %>% filter(acc == 0)
error.go <- (go$n)/394

table <- cbind(error.go,error.nogo,variable = 'MT')

cal_error <- function(file,variable.name){
  require(tidyverse)
  no.go <- file %>% filter(correctAns == 0) %>% count(acc) %>% filter(acc == 0)
  error.nogo <- (no.go$n)/46
  
  go <- file %>% filter(correctAns == 1)%>% count(acc) %>% filter(acc == 0)
  error.go <- (go$n)/394
  
  table <- data.frame(cbind(error.go,error.nogo,variable = variable.name))
  return(table)
}

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



# Test the rela between RT and z-score: 45 degrees line
#plot(individual_data_RT$RT,individual_data_RT$z_score)
#View(individual_data_RT)

#combine with the main data
#individual_data <- data.frame(cbind(individual_data,sec))
individual_data <- data.frame(individual_data)


ts_RT <- ts(individual_data)
x.info = attr(ts_RT , "tsp") 
tt = seq(0, 219, by=1)
#data.fmt = list(color=rgb(0.8,0.8,0.8,0.8), width=4)
#line.fmt = list(dash="solid", width = 1.5, color= 'firebrick')

# fig.add_scattergl(x=xs, y=df.y.where(df.y >= 50), line={'color': 'red'})
# fig.add_scattergl(x=xs, y=df.y, line={'color': 'black'})

yy= individual_data$z_score

#require(plotly)
ks1 = ksmooth(tt, yy, "normal", 20 , x.points=tt)
#p.ks = plot_ly(x=tt, y=yy, type="scatter", mode="lines", line=data.fmt, name="Raw data")
#p.ks = add_lines(p.ks, x=ks1$x, y=ks1$y, line=line.fmt,name="Avg. 20 trials")
#p.ks %>% layout(p.ks, title = "Kernel smoother", xaxis = list(title = 'Trials'), 
#              yaxis = list(title = 'Normalized RT')) 
#print(p.ks)


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

# ### Out of zone for no go trials 
# #no.go: in the zone rate and out of the zone #46
# no.go <- movement_1_RT  %>% filter(correctAns == 0) 
# 
# #No go in the zone error rate: #21
# no.go.in <- no.go %>% filter(Smoothed_VTC == 'In_zone') %>% 
#   count(acc) %>% filter(acc == 0)
# error.no.go.in <- (no.go.in$n)/46
# #No go out of the zone error rate #25
# no.go.out <- no.go %>% filter(Smoothed_VTC == 'Out_zone') %>% 
#   count(acc) %>% filter(acc == 0) 
# error.no.go.out <- (no.go.out$n)/46
# 
# table <- cbind(error.no.go.in,error.no.go.out,variable = 'RT')
# 
# 
# 
# no.go <- movement_1_MT %>% filter(correctAns == 0) %>% count(acc) %>% filter(acc == 0)
# error.nogo <- (no.go$n)/46
# 
# go <- movement_1_MT%>% filter(correctAns == 1)%>% count(acc) %>% filter(acc == 0)
# error.go <- (go$n)/394
# 
# table <- cbind(error.go,error.nogo,variable = 'MT')


median(movement_1_RT$z_RT[movement_1_RT$Smoothed_VTC=='Out_zone'])

df <- data.frame("c1" = c(41, 42, 43, 44),
                 "c2" = c(45, 46, 47, 48),
                 "c3" = c(49, 50, 51, 52))
#add row
df[nrow(df) + 1,3] <- c(c1(10, 20, 30)
#print
print(df)


###################
###
###  calculating rates for each go
###
##################
cal_error <- function(file,variable.name){
  #require(tidyverse)
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

## Pictures
require(plotly)

ts_RT <- ts(data)
x.info = attr(ts_RT , "tsp") 
tt = seq(0, 439, by=1)
yy= as.data.frame(data$zRT)

ks1 = ksmooth(tt, yy$V1, "normal", 20 , x.points=tt)
data.fmt = list(color=rgb(0.8,0.8,0.8,0.8), width=4)
line.fmt = list(dash="solid", width = 1.5, color= 'firebrick')
p.ks = plot_ly(x=tt, y=yy$V1, type="scatter", mode="lines", line=data.fmt, name="Raw data")
print(p.ks)
p.ks = add_lines(p.ks, x=ks1$x, y=ks1$y, line=line.fmt,name="Avg. 20 trials")
p.ks = p.ks %>% layout(p.ks, title = "Kernel smoother", xaxis = list(title = 'Trials'),
                       yaxis = list(title = 'Normalized RT'))
print(p.ks)
---------------------------------
library(gsignal)
fwhm(
  x = seq_len(length(y)),
  y,
  ref = c("max", "zero", "middle", "min", "absolute"),
  level = 0.5
)
# x <- seq(-pi,pi, 0.001)
# y <- cos(x)

x <- (-pi, pi, 0.01)
y <- cos(x)
w <- fwhm(x, y)
m <- x[which.max(y)]
f <- m - w/2
t <- m + w/2
plot(x, y, type="l",
     panel.first = {
       usr <- par('usr')
       rect(f, usr[3], t, usr[4], col = rgb(1, 0, 0, 0.4), border = NA)
     })
abline(h = max(y) / 2, lty = 2, col = "gray")



