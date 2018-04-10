library(zoo)
library(ggplot2)
library(dplyr)
library(ACMEOscillation)

setwd('/home/robinson/Desktop/scripts/R scripts/')
source("oscillations.R")

setwd('/home/robinson/Data/confocal_extensometer/FR_project/2017_08_28/_2017_12_19_fr.lif - box1_b5 - C=0')
x<-read.csv('tracking.csv')
x$Length<-(x$Y1-x$Y0)
ggplot(data=x,aes(x=Time,y=Length))+geom_line()+theme_bw()

#all_extrema <- findLengthExtrema(x, pauseLength=35, noiseWidth=5) # if many sections noise width is 5 here
all_extrema <- findLengthExtrema(x,pauseLength=35, noiseWidth=0) # if only one section  noise <3 if sample is stiff 
ggplot(data=x,aes(x=Time,y=Length))+geom_line()+theme_bw()
plotExtrema(x, all_extrema)
localMinMax <- function (xz, width=11) {
  pos = width %/% 2  # integer division ...
  MIN <-rollapply(xz, width, function(x) which.min(x)==pos, fill=c(FALSE, NA, FALSE))
  MAX <-rollapply(xz, width, function(x) which.max(x)==pos, fill=c(FALSE, NA, FALSE))
  data.frame(MIN=MIN, MAX=MAX)
}

minMax <- localMinMax(xz)

# Find distances between successive peaks
# pos is the index (time?) of the positions
# peak is a boolean array valued TRUE on the peaks
# returns a data.frame with columns "interval" and "pos"
# "pos" is the position of the first element in the interval
peaksDistance <- function(peak, pos=NULL) {
  idx <- c(1:length(pos))
  peakPos <- if(is.null(pos)) idx[peak] else pos[idx[peak]]
  n <- length(peakPos)
  data.frame(interval=peakPos[2:n] - peakPos[1:n-1], pos=peakPos[1:n-1])
}

dMin <- peaksDistance(minMax$MIN, pos=x$Time)
dMax <- peaksDistance(minMax$MAX, pos=x$Time)
dMinMax <- data.frame(interval=c(dMin$interval, dMax$interval),
                      pos=c(dMin$pos, dMax$pos),
                      type=as.factor(c(rep("MIN", nrow(dMin)), rep("MAX", nrow(dMax)))))



new2<-all_extrema
new2$Strain<-0

lines=nrow(new2)
for(i in 2:lines){
  if(new2$kind[i]=='max'){
    new2$Strain[i]<-(new2$Length[i]-new2$Length[i-1])/new2$Length[i-1]
  }
}

write.csv(new2,'new2.csv')
new3<-new2[c(abs(new2$Strain)>0),]
write.csv(new3,'new3.csv')


ggplot(data=new2,aes(x=Time,y=Strain))+geom_line()+theme_bw()
ggplot(data=new3,aes(x=Time,y=Strain))+geom_point()+theme_bw()

a=new3$Strain
print(new3$Strain)
Strain=mean(new3$Strain)
print(Strain)

a=a[a<0.1]
#a=a[a>0.01]
a=a[a>0.05]
mean(a)

