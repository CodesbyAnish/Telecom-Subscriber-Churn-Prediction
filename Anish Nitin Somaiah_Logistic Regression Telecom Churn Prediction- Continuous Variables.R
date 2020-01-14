#Importing libraries
library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(corrplot)
library(caret)
library(rms)
library(MASS)
library(e1071)
library(ROCR)
library(gplots)
library(pROC)
library(rpart)
library(randomForest)
library(ggpubr)
library(sqldf)
library(caTools)
install.packages('PredPsych')
library(PredPsych)

#Importing Dataset
df_train <- read.csv("C:/Users/anish/Downloads/Data Mining/Final Project/telecom_customer_churn_bigml.csv")
df_train <- data.frame(telcodf)

#EDA for entire dataset
str(df_train)
summary(df_train)

#Datavalidation for cleaning
anyNA(df_train)

#correlation matrix for continuous variables
df_train1 <- df_train
df_train1$churn <- as.numeric(df_train1$churn)
df_train1$international.plan <- as.numeric(df_train1$international.plan)
df_train1$voice.mail.plan <- as.numeric(df_train1$voice.mail.plan)
str(df_train1)
corrplot(cor(df_train1[sapply(df_train1, is.numeric)]))

#Outlier Analysis
q1 <- ggplot(df_train, aes(x = 1, y =total.day.charge )) +
  geom_boxplot()+theme_minimal()+
  ylab("Total Day Charge")

q2 <- ggplot(df_train, aes(x = 1, y =total.eve.charge )) +
  geom_boxplot()+theme_minimal()+
  ylab("Total Evening Charge")

q3 <- ggplot(df_train, aes(x = 1, y =total.night.charge )) +
  geom_boxplot()+theme_minimal()+
  ylab("Total Night Charge")

q4 <- ggplot(df_train, aes(x = 1, y =total.intl.charge )) +
  geom_boxplot()+theme_minimal()+
  ylab("Total International Charges")

q5 <- ggplot(df_train, aes(x = 1, y =number.vmail.messages )) +
  geom_boxplot()+theme_minimal()+
  ylab("Total No.of Vmail messages")


ggarrange(q1,q2,q3,q4,q5, ncol = 3, nrow = 2)

#Exploratory Data Analysis

#Churn analysis based on continuous variables

#Account length plot
df_account <- df_train[c(21,2)]
df_account$cat = cut2(df_account$account.length,c(0,25,50,75,100,125,150,175,200,225,250,275,300,325,350))

df<-df_account%>%
  group_by(churn,cat)%>%
  summarise(count=n())


df<-df%>%
  filter(churn=='True')%>%
  mutate(perc = count/sum(count))

head(df)


ggplot(df,aes(fill=churn, y=count, x=cat))  + 
  xlab("Account length (in interval of 25) in weeks ") + 
  ylab("COUNT OF CHURNED CUSTOMERS")+
  geom_bar(position="dodge", stat="identity")+labs(title="Churned customers vs Account length ")+
  theme_minimal() +scale_fill_manual(values=c( "#800000")) +
  geom_text(aes(label=paste0(round(perc,2)*100,"%")), vjust = -0.5, color="red", size=3)+
  geom_text(aes(label=paste0(count)), vjust = 2, color="white", size=3)+
  theme(axis.text.x = element_text(angle = 90))+ theme(panel.border= element_blank()) +
  theme(axis.line = element_line(color="grey", size = 1))

#Customer call plot
df_account2 <- df_train[c(21,2,20)]
df_account2$cat = cut2(df_account2$account.length,c(1,25,50,75,100,125,150,175,200,225,250,275,300,325,350))

df_account_length_count_churn<-df_account2 %>%
  filter(churn=='True')%>%
  group_by(cat,customer.service.calls)%>%
  summarise(count_people_churn = n())
df_account_length_count_nonchurn<-df_account2 %>%
  filter(churn=='False')%>%
  group_by(cat,customer.service.calls)%>%
  summarise(count_people_nonchurn = n())
df_acc_cust_calls<-merge(df_account_length_count_churn, df_account_length_count_nonchurn,all=TRUE)
df_acc_cust_calls[c(4)] <-NULL
df_acc_cust_calls[is.na(df_acc_cust_calls)] <- 0

df_acc_cust_calls$cat_call = cut2(df_acc_cust_calls$customer.service.calls, c(0,2,4,6,8,12))
df_acc_cust_calls[c(2)]<-NULL

df3<-df_acc_cust_calls %>%
  group_by(cat,cat_call)%>%
  summarise(total_churn = sum(count_people_churn))
ggplot(df3,aes(fill=cat_call, y=total_churn, x=cat))  + 
  xlab("Account length (in interval of 25)") + 
  ylab("Number of customers churned")+
  geom_bar(position="dodge", stat="identity")+
  theme_minimal() +scale_fill_manual(values=c( "#800000", "#006666",
                                               "#E2E1C0", "#666666",
                                               "#D8CBCB" ))+
  labs(title  = "Relation between Customer Care Calls with Number of people churned ")+
  guides(fill=guide_legend(title="Cust_Care_Calls"))+ theme(legend.position="bottom")+
  theme(plot.title = element_text(hjust=0.5))


#Churn analysis with categorical variables

df1 <-df_train[c(21,2,5)]
df1$cat_acc_len = cut(df1$account.length,c(0,25,50,75,100,125,150,175,200,225,250,275,300,325,350))
df1<-df1[c(1,4,3)]
df1<-df1 %>%
  group_by(churn,international.plan,cat_acc_len)%>%
  summarise(total_count = n())
ggplot(df1,aes(x=cat_acc_len,y = total_count,fill = churn))  + facet_wrap(~international.plan,scale="free")+
  geom_bar(position="dodge", stat="identity") + theme_minimal()+
  theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_manual(values=c( "#006666","#800000"))+ 
  xlab("Account length (in interval of 25)  ") + 
  ylab("COUNT OF CUSTOMERS")+
  labs(title="International plans")+
  theme(plot.title = element_text(hjust=0.5))+ theme(panel.border= element_blank()) +
  theme(axis.line = element_line(color="grey", size = 1))+
  theme(axis.line = element_line(color="grey", size = 1))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


df2 <-df_train[c(21,2,6)]
df2$cat_acc_len = cut(df2$account.length,c(0,25,50,75,100,125,150,175,200,225,250,275,300,325,350))
df2<-df2[c(1,4,3)]
df2<-df2 %>%
  group_by(churn,voice.mail.plan,cat_acc_len)%>%
  summarise(total_count = n())
ggplot(df2,aes(x=cat_acc_len,y = total_count,fill = churn))  + facet_wrap(~voice.mail.plan,scale="free")+
  geom_bar(position="dodge", stat="identity") + theme_minimal()+
  theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_manual(values=c( "#006666","#800000"))+ 
  xlab("Account length (in interval of 25)  ") + 
  ylab("COUNT OF CUSTOMERS")+
  labs(title="Voicemail plans")+
  theme(plot.title = element_text(hjust=0.5))+ theme(panel.border= element_blank()) +
  theme(axis.line = element_line(color="grey", size = 1))+
  theme(axis.line = element_line(color="grey", size = 1))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#Logistic Regression
set.seed(101)
split<-sample.split(df_train,SplitRatio = 0.75)
training<-subset(df_train,split=="TRUE")
testing<-subset(df_train,split=="FALSE")
lrmodel <- glm(formula = churn ~ number.vmail.messages + total.day.charge + total.eve.charge +
                total.night.charge + total.intl.charge,
              family = "binomial", data = training)
summary(lrmodel)
vif(lrmodel)

#Prediction Model
res<-predict(lrmodel,testing,type = "response")
summary(res)

#Cutoff value estimation using ROC curve
res2<-predict(lrmodel,training,type = "response")
pred1=prediction(res2,training$churn)
perf1<-performance(pred1,"tpr","fpr")
plot(perf1,colorize=TRUE,print.cutoffs.at=seq(0.1,by=0.1))

#Confusion Matrix with threshold value
res3<-predict(lrmodel,testing,type = "response")
table(Actualvalue=testing$churn,Predictedvalue=res3>0.3)
Class <- table(Actualvalue=testing$churn,Predictedvalue=res3>0.3)
confusionMat <- list(table = matrix(c(791,90,14,58),ncol = 2,dimnames = list(Prediction = c(1,2),Reference = c(1,2))))
overallConfusionMetrics(confusionMat)

