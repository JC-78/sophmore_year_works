
library(tidyverse)
library(ggplot2)
library(lattice)
library(caret)
dat<-read.csv("/Users/joonghochoi/Desktop/hackathon_scouting.csv")
dummies <- dummyVars(~ ., data=dat[,-7])
View(dummies)
unique(dat$Home.Team)
View(dat)
#correlation heatmap

data1<-dat%>%filter(Event=="Play")
my_data<-data1[,c('X.Coordinate','Y.Coordinate','X.Coordinate.2','Y.Coordinate.2')]
my_data1<-my_data%>%rename('Player1.X'=X.Coordinate,'Player1.Y'=Y.Coordinate,
                           'Player2.X'=X.Coordinate.2,'Player2.Y'=Y.Coordinate.2)
k<-cor(my_data1)
head(k)
library(reshape2)
melted_cormat <- melt(k)
head(melted_cormat)

library(ggplot2)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()


#Frequency histogram of Events

date<-data$game_date
a<-which(substr(date,1,4)=="2019")
b<-which(substr(date,1,4)=="2020")
c<-union(a,b)
data2<-data[c,]

ggplot(data2, aes(x=Event)) +
  geom_bar()



