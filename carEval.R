library(e1071) #Consists of naive bayes model, predict function etc

library(caTools) #Used for splitting
bdata<-read.csv(file.choose())
View(bdata)

str(bdata)
bdata<-na.omit(bdata)
bdata$ï..buying<-as.factor(bdata$ï..buying)
levels(bdata$ï..buying)<-c("low","med","high","vhigh")

bdata$maint<-as.factor(bdata$maint)
levels(bdata$maint)<-c("low","med","high","vhigh")

bdata$persons<-as.factor(bdata$persons)

bdata$lug_boot<-as.factor(bdata$lug_boot)
levels(bdata$lug_boot)<-c("small","med","big")

bdata$doors<-as.factor(bdata$doors)

bdata$safety<-as.factor(bdata$safety)
levels(bdata$safety)<-c("low","med","high")

bdata$car.class<-as.factor(bdata$car.class)
str(bdata)

#add seed variable.

split<-sample.split(bdata$car.class,SplitRatio = 0.9)
train<-subset(bdata,split=T) 
test<-subset(bdata,split=F)

#We used ~. as we want to use all the data of the data frame for this analysis

car.fit<-naiveBayes(car.class~.,data=train)
car.fit

pr3<-predict(car.fit,train)
pr3
table(pr3,train$car.class)
#acc acc and unacc unacc depict the correct predictions
#Others are incorrect predictions

accuracy<-(466+1168)/(466+1168+42+52)
accuracy           
