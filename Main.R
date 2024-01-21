rm(list = ls())

library(corrplot)
filename <- file.choose()

Weekly <- read.csv(filename)
summary(Weekly)
corrplot(cor(Weekly[,-9]), method="square")

#B
# Assuming "Up" is coded as 1 and "Down" is coded as 0
Weekly$Direction <- as.numeric(Weekly$Direction == "Up")
Weekly.fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Weekly, family = binomial)


summary(Weekly.fit)

#C
logWeekly.prob= predict(Weekly.fit, type='response')
logWeekly.pred =rep("Down", length(logWeekly.prob))
logWeekly.pred[logWeekly.prob > 0.5] = "Up"
table(logWeekly.pred, Weekly$Direction)

#D
train = (Weekly$Year<2009)
Weekly.0910 <-Weekly[!train,]
Weekly.fit<-glm(Direction~Lag2, data=Weekly,family=binomial, subset=train)
logWeekly.prob= predict(Weekly.fit, Weekly.0910, type = "response")
logWeekly.pred = rep("Down", length(logWeekly.prob))
logWeekly.pred[logWeekly.prob > 0.5] = "Up"
Direction.0910 = Weekly$Direction[!train]
table(logWeekly.pred, Direction.0910)
mean(logWeekly.pred == Direction.0910)

#E
library(MASS)
Weeklylda.fit<-lda(Direction~Lag2, data=Weekly,family=binomial, subset=train)
Weeklylda.pred<-predict(Weeklylda.fit, Weekly.0910)
table(Weeklylda.pred$class, Direction.0910)
mean(Weeklylda.pred$class==Direction.0910)

#F
Weeklyqda.fit = qda(Direction ~ Lag2, data = Weekly, subset = train)
Weeklyqda.pred = predict(Weeklyqda.fit, Weekly.0910)$class
table(Weeklyqda.pred, Direction.0910)
mean(Weeklyqda.pred==Direction.0910)

#G
library(class)
Week.train=as.matrix(Weekly$Lag2[train])
Week.test=as.matrix(Weekly$Lag2[!train])
train.Direction =Weekly$Direction[train]
set.seed(1)
Weekknn.pred=knn(Week.train,Week.test,train.Direction,k=1)
table(Weekknn.pred,Direction.0910)
mean(Weekknn.pred == Direction.0910)

#H

#I
#Logistic Regression with Interaction Lag2:Lag4
Weekly.fit<-glm(Weekly$Direction~Lag2:Lag4+Lag2, data=Weekly,family=binomial, subset=train)
logWeekly.prob= predict(Weekly.fit, Weekly.0910, type = "response")
logWeekly.pred = rep("Down", length(logWeekly.prob))
logWeekly.pred[logWeekly.prob > 0.5] = "Up"
Direction.0910 = Weekly$Direction[!train]
table(logWeekly.pred, Direction.0910)

mean(logWeekly.pred == Direction.0910)

#LDA with Interaction Lag2:Lag4
Weeklylda.fit<-lda(Direction~Lag2:Lag4+Lag2, data=Weekly,family=binomial, subset=train)
Weeklylda.pred<-predict(Weeklylda.fit, Weekly.0910)
table(Weeklylda.pred$class, Direction.0910)

mean(Weeklylda.pred$class==Direction.0910)

Weeklyqda.fit = qda(Direction ~ poly(Lag2,2), data = Weekly, subset = train)
Weeklyqda.pred = predict(Weeklyqda.fit, Weekly.0910)$class
table(Weeklyqda.pred, Direction.0910)

mean(Weeklyqda.pred==Direction.0910)

#K=5
Week.train=as.matrix(Weekly$Lag2[train])
Week.test=as.matrix(Weekly$Lag2[!train])
train.Direction =Weekly$Direction[train]
set.seed(1)
Weekknn.pred=knn(Week.train,Week.test,train.Direction,k=5)
table(Weekknn.pred,Direction.0910)

mean(Weekknn.pred == Direction.0910)

#K=10
Week.train=as.matrix(Weekly$Lag2[train])
Week.test=as.matrix(Weekly$Lag2[!train])
train.Direction =Weekly$Direction[train]
set.seed(1)
Weekknn.pred=knn(Week.train,Week.test,train.Direction,k=10)
table(Weekknn.pred,Direction.0910)

mean(Weekknn.pred == Direction.0910)


#K=100
Week.train=as.matrix(Weekly$Lag2[train])
Week.test=as.matrix(Weekly$Lag2[!train])
train.Direction =Weekly$Direction[train]
set.seed(1)
Weekknn.pred=knn(Week.train,Week.test,train.Direction,k=100)
table(Weekknn.pred,Direction.0910)
mean(Weekknn.pred == Direction.0910)

