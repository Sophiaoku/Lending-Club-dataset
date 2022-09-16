loans <- read.csv('loan_data.csv')
str(loans)
summary(loans)

#minor data cleaning 
loans$credit.policy <- factor(loans$credit.policy)
loans$inq.last.6mths <- factor(loans$inq.last.6mths)
loans$delinq.2yrs <- factor(loans$delinq.2yrs)
loans$pub.rec <- factor(loans$pub.rec)
loans$not.fully.paid <- factor(loans$not.fully.paid)

#exploratory data analysis

library(ggplot2)

pl <- ggplot(loans,aes(x=fico)) +
  geom_histogram(aes(fill=not.fully.paid),color='black',bins=40,alpha=0.5) +
  scale_fill_manual(values = c('yellow','red')) + theme_bw()

pl

pl2 <- ggplot(loans,aes(x=factor(purpose))) +
  geom_bar(aes(fill=not.fully.paid),position = "dodge") +
  theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

pl2


ggplot(loans,aes(int.rate,fico))+geom_point(aes(color=not.fully.paid),alpha=0.3) + theme_bw()

#model 
#using a 70/30 split

library(caTools)

set.seed(101)

sample = sample.split(loans$not.fully.paid, 0.7)

train = subset(loans, sample == TRUE)

test = subset(loans, sample == FALSE)


library(e1071)

model1 <- svm(not.fully.paid ~ .,data=train)
summary(model1)

predicted.values <- predict(model1,test[1:13])
table(predicted.values,test$not.fully.paid)

#460 false positives from confusion matrix

tune.results <- tune(svm,train.x=not.fully.paid~., data=train,kernel='radial',ranges=list(cost=c(1,10), gamma=c(0.1,1)))
#PSA depending on your PC, this will take some time.

tune.results

model <- svm(not.fully.paid ~ .,data=train,cost=100,gamma = 0.1)
model

predicted.values <- predict(model,test[1:13])
table(predicted.values,test$not.fully.paid)

