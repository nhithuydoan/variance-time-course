# Cities & Mountains -- simple RT/ACC analysis #

# load relevant packages
library(tidyverse)
library(dplyr)
library('zoo')
library('readr')
require(plotly)

# load one subj data (4 runs)
r1_data = read.table('~/Downloads/SA_pilot/Sub4/MVKKey_414/414_MouseVersionKeyData.txt',F)
r2_data = read.table('~/Downloads/SA_pilot/Sub4/MVKKey_424/424_MouseVersionKeyData.txt',F)
r3_data = read.table('~/Downloads/SA_pilot/Sub4/MVKKey_434/434_MouseVersionKeyData.txt',F)
r4_data = read.table('~/Downloads/SA_pilot/Sub4/MVKKey_444/444_MouseVersionKeyData.txt',F)

# cat into one data.frame
data = rbind(r1_data,r2_data,r3_data,r4_data)

names(data) = c('trial','RT','acc','TType')

# code missing vals as NA
data$acc = ifelse(data$acc=='NaN',NA,data$acc)
data$RT = ifelse(data$RT==(-1),NA,data$RT)

# new indices for block (run), subject #, condition (1 = easy, 2 = hard)
data$block = c(rep(1,length(r1_data$V1)),rep(2,length(r2_data$V1)),rep(3,length(r3_data$V1)),rep(4,length(r4_data$V1)))
data$sub = rep('2',length(data$trial))
data$condit = rep('2',length(data$trial))

## should be calculated within run if consistent with Esterman et al. 2013.

#cut last trial we can't interpolate over
data=data[-600,]
#data=data[-1,]
data=data[-599,] 

## linear interpolation of missing RT vals
data$RT=na.approx(data$RT)

## Z-scoring reaction time within subject (can also do within BLOCK)
data$zrt=scale(data$RT) # make sure it is absolute value of RT-z
data$zrt=abs(data$zrt)

#plot(1:length(data$trial),data$zrt,pch=16,col='darkgray',cex=0.5)
#lines(1:length(data$trial),data$zrt,col='darkgray')

# for the kernel smoothing it is 
seq = 1:length(data$trial)
data.fmt = list(color=rgb(0.8,0.8,0.8,0.8), width=2.5)
line.fmt = list(dash="solid", width = 4, color= 'red')
yy = data$zrt

ks1 = ksmooth(seq, yy, "normal", 20 , x.points=seq) # using a 20-trial kernel in this ex.

## lets assign colors depending on whether they were in or out of the zone
#allcolors = rep('darkorange',length(seq))
#allcolors = ifelse(ks1$y<median(ks1$y),'blue',allcolors)

plot(seq,yy,type='lines',lwd=1,col='darkgray',main="Kernel Smoother",ylab='Normalized RT',xlab='Trial Number',cex.lab=1.25,ylim=c(0,4))
lines(seq,ks1$y,type='lines',lwd=4,col='red')
abline(h = median(ks1$y),lty=3,lwd=1.5)


## further analysis ##
vtc = ks1$y
data = cbind(data,vtc)
cutoff = median(data$vtc)

data$zone = ifelse(data$vtc>cutoff,'OUT','IN')

acc_omit = mean(data$acc[data$TType==1],na.rm=T) # 97% correct on city
acc_comm = mean(data$acc[data$TType==0],na.rm=T) # 84% correct on mountain

# in zone vs. out?
acc_omit_Z = mean(data$acc[data$TType==1&data$zone=='IN'],na.rm=T) # 98.5% IN ZONE
acc_omit_O = mean(data$acc[data$TType==1&data$zone=='OUT'],na.rm=T) # 95.7$ OUT ZONE

acc_omit_Z = abs(acc_omit_Z-1)
acc_omit_O = abs(acc_omit_O-1)

#plot(c(acc_omit_Z,acc_omit_O),col=c('darkorange','blue'),pch=17,cex=7,ylim=c(.85,1.0),xaxt='n',xlab=c("In Zone","Out Zone"),xlim=c(0.5,2))

barplot(c(acc_omit_Z,acc_omit_O),col=c('blue','darkorange'),ylab='Omission Error Rate',ylim=c(0,0.2),main='Omission (City) Errors: In vs. Out',xlim=c(0,2.5),xpd=FALSE)
legend(0.2,0.2,c('In the zone','out of the zone'),pch=15,cex=1.25,col=c('blue','darkorange'))
#axis(side=1,at=c(0.84,0.84),labels=c("group 1", "group 2"))

### Comission errors
acc_comm_Z = mean(data$acc[data$TType==0&data$zone=='IN'],na.rm=T) # 96.4% IN ZONE
acc_comm_O = mean(data$acc[data$TType==0&data$zone=='OUT'],na.rm=T) # 74.3% OUT ZONE

acc_comm_Z = abs(acc_comm_Z-1)
acc_comm_O = abs(acc_comm_O-1)


#plot(c(acc_omit_Z,acc_omit_O),col=c('darkorange','blue'),pch=17,cex=7,ylim=c(.85,1.0),xaxt='n',xlab=c("In Zone","Out Zone"),xlim=c(0.5,2))

quartz()
barplot(c(acc_comm_Z,acc_comm_O),col=c('blue','darkorange'),ylab='Lapse Rate',ylim=c(0,0.8),main='Comission (Mountain) Errors: In vs. Out',xlim=c(0,2.5),xpd=FALSE)
legend(0.2,0.8,c('In the zone','out of the zone'),pch=15,cex=1.25,col=c('blue','darkorange'))

#axis(side=1,at=c(0.84,0.84),labels=c("group 1", "group 2"))


## do the error rates(particularly commission errors) change as a function of time, ie vigilance decrement?

length(data$zone[data$zone=='OUT'&data$trial<75])
length(data$zone[data$zone=='OUT'&data$trial>75]) # 134 vs. 162 early//late (WITHIN)

length(data$zone[data$zone=='OUT'&data$block<=2])
length(data$zone[data$zone=='OUT'&data$block>=3]) # 183 vs. 116 early//late (BETWEEN)


mean(data$acc[data$zone=='OUT'&data$trial<75&data$TType==0]) # 60%
mean(data$acc[data$zone=='OUT'&data$trial>75&data$TType==0]) # 85%

mean(data$acc[data$zone=='OUT'&data$block<=2&data$TType==0]) # 68% actually worse early on errors.
mean(data$acc[data$zone=='OUT'&data$block>=3&data$TType==0]) # 84%

