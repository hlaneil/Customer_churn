library(tidyverse)
library(readxl)
library(lattice)
library(ggplot2)
library(gtools)
library(rpart)
library(rpart.plot)
library(pROC)
library(caret)
cuschurn<-read.csv("WA_Fn-UseC_-Telco-Customer-Churn.csv",stringsAsFactors= T)
sum(is.na(cuschurn))
cuschurn<-na.omit(cuschurn)
cuschurn$SeniorCitizen<-as.factor(cuschurn$SeniorCitizen)
rownames(cuschurn) <- cuschurn[,1] 
cuschurn[,1] <- NULL 
summary(cuschurn)
cuschurn%>%
  ggplot(aes(x=tenure,y=MonthlyCharges, col=Churn))+
  geom_point(shape=1)

cuschurn%>%
  ggplot(aes(x=gender, fill=Churn))+
  geom_bar()
cuschurn%>%
  ggplot(aes(x=SeniorCitizen, fill=Churn))+
  geom_bar()
cuschurn%>%
  ggplot(aes(x=Partner, fill=Churn))+
  geom_bar()
cuschurn%>%
  ggplot(aes(x=PhoneService, fill=Churn))+
  geom_bar()
cuschurn%>%
  ggplot(aes(x=MultipleLines, fill=Churn))+
  geom_bar()
cuschurn%>%
  ggplot(aes(x=InternetService, fill=Churn))+
  geom_bar()
cuschurn%>%
  ggplot(aes(x=OnlineSecurity, fill=Churn))+
  geom_bar()
cuschurn%>%
  ggplot(aes(x=OnlineBackup, fill=Churn))+
  geom_bar()
cuschurn%>%
  ggplot(aes(x=DeviceProtection, fill=Churn))+
  geom_bar()
cuschurn%>%
  ggplot(aes(x=TechSupport, fill=Churn))+
  geom_bar()
cuschurn%>%
  ggplot(aes(x=StreamingTV, fill=Churn))+
  geom_bar()
cuschurn%>%
  ggplot(aes(x=StreamingMovies, fill=Churn))+
  geom_bar()
cuschurn%>%
  ggplot(aes(x=Contract, fill=Churn))+
  geom_bar()
cuschurn%>%
  ggplot(aes(x=PaperlessBilling, fill=Churn))+
  geom_bar()
cuschurn%>%
  ggplot(aes(x=PaymentMethod, fill=Churn))+
  geom_bar()


cuschurn$Churn = ifelse(cuschurn$Churn == "Yes", 1, 0)

cuschurn$tenure<- quantcut(cuschurn$tenure, q=10)
cuschurn$MonthlyCharges<- quantcut(cuschurn$MonthlyCharges, q=10)
cuschurn$TotalCharges<- quantcut(cuschurn$TotalCharges, q=10)

groups <- c(quo(gender), quo(SeniorCitizen), quo(Partner), quo(PhoneService), quo(MultipleLines), quo(InternetService), quo(OnlineSecurity), quo(OnlineBackup), quo(DeviceProtection), quo(TechSupport), quo(StreamingTV), quo(StreamingMovies), quo(Contract), quo(PaperlessBilling), quo(PaymentMethod),quo(tenure), quo(MonthlyCharges), quo(TotalCharges) ) 

for (i in seq_along(groups)) {
  cuschurn%>% 
    group_by(!!groups[[i]]) %>% # Unquote with !!
    summarise(churn_rate = round(mean(Churn),3))%>%
    mutate(Diff = churn_rate - lag(churn_rate))%>%
    print()
}

set.seed(1)
index<-sample(nrow(cuschurn),nrow(cuschurn)*0.2)  
test<-cuschurn%>% 
  filter(row_number() %in% index)%>%
  select("Churn","gender","SeniorCitizen","Partner","InternetService","OnlineSecurity","OnlineBackup","DeviceProtection","TechSupport","StreamingTV","StreamingMovies","Contract","PaperlessBilling","PaymentMethod")
train<-cuschurn[-index,c("Churn","gender","SeniorCitizen","Partner","InternetService","OnlineSecurity","OnlineBackup","DeviceProtection","TechSupport","StreamingTV","StreamingMovies","Contract","PaperlessBilling","PaymentMethod")]

ct_train_full_tree<-rpart(Churn~gender+SeniorCitizen+Partner+InternetService+OnlineSecurity+OnlineBackup+DeviceProtection+TechSupport+StreamingTV+StreamingMovies+Contract+PaperlessBilling+PaymentMethod,
                          data=train, 
                          method="class", 
                          control=rpart.control(cp=0))
rpart.plot(ct_train_full_tree)

printcp(ct_train_full_tree)
plotcp(ct_train_full_tree)

min_xerror<-ct_train_full_tree$cptable[which.min(ct_train_full_tree$cptable[,"xerror"]),]


min_xerror_tree<-prune(ct_train_full_tree, cp=min_xerror[1])

rpart.plot(min_xerror_tree)

train$min_xerror_prob<-predict(min_xerror_tree,train)[,2]
test$min_xerror_prob<-predict(min_xerror_tree,test)[,2]


x = seq(from = 0.3, to = 0.7, by = 0.01)
train_error = c()
test_error = c()

for (i in x)
{
  train$min_xerror_prob_class=ifelse(train$min_xerror_prob> i,1,0)
  test$min_xerror_prob_class=ifelse(test$min_xerror_prob> i,1,0)
  min_xerror_train_error = round(sum(train$min_xerror_prob_class!=train$Churn) / nrow(train), 4)
  min_xerror_test_error = round(sum(test$min_xerror_prob_class!=test$Churn) / nrow(test), 4)
  train_error = c(train_error,min_xerror_train_error)
  test_error = c(test_error,min_xerror_test_error)
}


train_error <- data.frame(x, train_error)
test_error <- data.frame(x, test_error)

ggplot() + 
  geom_line(data = train_error, aes(x = x, y = train_error), color = "pink", size = 1) +
  geom_line(data = test_error, aes(x = x, y = test_error), color = "purple", size = 1) +
  xlab('cut off point') +
  ylab('error rate')


train$min_xerror_prob_class=ifelse(train$min_xerror_prob>0.4,1,0)
test$min_xerror_prob_class=ifelse(test$min_xerror_prob>0.4,1,0)
test$min_xerror_prob_class = as.factor(test$min_xerror_prob_class)
test$Churn = as.factor(test$Churn)
confusionMatrix(test$min_xerror_prob_class, test$Churn) 

min_xerror_roc<-roc(test$Churn,test$min_xerror_prob,auc=TRUE)
plot(min_xerror_roc,print.auc=TRUE,col="red")

rpart.plot(min_xerror_tree)