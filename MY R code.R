read.csv("C:/Users/user/Desktop/houses.csv")->houses
str(houses)
#data cleaning
library(dplyr)
houses%>%select(c(-1,-2))->houses
houses
#pre processing
houses$air_cond<-factor(houses$air_cond,labels=c("NO","YES"))
houses$construction<-factor(houses$construction, labels =c("NO","YES"))
houses$waterfront<-factor(houses$waterfront, labels = c("NO","YES"))
houses$fuel<-factor(houses$heat, labels=c("Gas","Electric","Oil"))
houses$sewer<-factor(houses$sewer, labels = c("None","Privet","Public"))
houses$heat<-factor(houses$heat, labels=c("Hot Air","Hot water","Electric"))
houses

#visualization
library(ggplot2)
ggplot(data=houses,aes(x=price))+geom_histogram(bins=40)
ggplot(data=houses,aes(x=price))+geom_histogram(bins=40,fill="lightblue",col="blue")
ggplot(data=houses,aes(y=price,x=waterfront,fill=waterfront))+geom_boxplot()
ggplot(data=houses,aes(y=price,x=air_cond,fill=air_cond))+geom_boxplot()

ggplot(houses,aes(x=living_area,y=price))+geom_point()+geom_smooth(method = "lm",se=F)
ggplot(houses,aes(x=age,y=price))+geom_point(col="purple")+geom_smooth(method = "lm",se=F)
ggplot(houses,aes(x=living_area,y=price,col=factor(rooms)))+geom_point()+geom_smooth(method ="lm",se=F)+labs(col="rooms")
ggplot(houses,aes(x=age,y=price,col=factor(rooms)))+geom_point()+geom_smooth(method ="lm",se=F)+labs(col="rooms")



#splitting data
library(caTools)
sample.split(houses$price,SplitRatio = 0.65)->split_index
train<-subset(houses,split_index==T)
test<-subset(houses,split_index==F)
nrow(train)
nrow(test)


#model building
mod1<-lm(price~.,data=train)
predict(mod1,test)->result
compare_result<-cbind(actual=test$price,predicted=result)
as.data.frame(compare_result)->compare_result
error<-compare_result$actual-compare_result$predicted
cbind(compare_result,error)->compare_result
sqrt(mean(compare_result$error^2))->rmse1
rmse1
summary(mod1)

mod2<-lm(price~.-fireplaces-sewer-fuel,data=train)
predict(mod2,test)->result2
compare_result2<-cbind(actual=test$price,predicted=result2)
as.data.frame(compare_result2)->compare_result2
error<-compare_result2$actual-compare_result2$predicted
cbind(compare_result2,error)->compare_result2
sqrt(mean(compare_result2$error^2))->rmse2
rmse2
summary(mod2)


