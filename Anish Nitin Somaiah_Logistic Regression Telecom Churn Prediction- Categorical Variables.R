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
library(rpart.plot)
library(randomForest)
library(ggpubr)
library(sqldf)

#Importing Dataset
telco <- read.csv("C:/Users/anish/Downloads/Data Mining/Final Project/WA_Fn-UseC_-Telco-Customer-Churn.csv")
telco <- data.frame(telco)

#EDA for entire dataset
str(telco)
summary(telco)
telco %>%
  summarise(Total = n(), n_Churn = sum(Churn == "Yes"), p_Churn = n_Churn/Total)


#Data Cleaning
telco <- telco[complete.cases(telco),]

#EDA Plots for Continuous variables
ggplot(data = telco, aes(MonthlyCharges, color = Churn))+
  geom_freqpoly(binwidth = 5, size = 1)

ggplot(data = telco, aes(TotalCharges, color = Churn))+
  geom_freqpoly(binwidth = 200, size = 1)

ggplot(data = telco, aes(tenure, colour = Churn))+
  geom_freqpoly(binwidth = 5, size = 1)

#Outlier Analysis
ggplot(telco, aes(x = 1, y =tenure)) +
  geom_boxplot(color = "Blue")+theme_minimal()+
  ylab("Tenure")

ggplot(telco, aes(x = 1, y =MonthlyCharges)) +
  geom_boxplot(color = "Green")+theme_minimal()+
  ylab("Monthly Charges")

ggplot(telco, aes(x = 1, y =TotalCharges)) +
  geom_boxplot(color = "Red")+theme_minimal()+
  ylab("Total Charges")

#Correlation Plot for Continuous Variables
telco %>%
  dplyr::select (TotalCharges, MonthlyCharges, tenure) %>%
  cor() %>%
  corrplot.mixed(upper = "circle", tl.col = "black", number.cex = 0.7)

#Correlation Plot for Categorical Variables
telcom <- telco
telcom$customerID <- NULL
telcom$gender <- as.numeric(telcom$gender)
telcom$Partner <- as.numeric(telcom$Partner)
telcom$Dependents <- as.numeric(telcom$Dependents)
telcom$PhoneService <- as.numeric(telcom$PhoneService)
telcom$MultipleLines <- as.numeric(telcom$MultipleLines)
telcom$InternetService <- as.numeric(telcom$InternetService)
telcom$OnlineSecurity <- as.numeric(telcom$OnlineSecurity)
telcom$OnlineBackup <- as.numeric(telcom$OnlineBackup)
telcom$DeviceProtection <- as.numeric(telcom$DeviceProtection)
telcom$TechSupport <- as.numeric(telcom$TechSupport)
telcom$StreamingTV <- as.numeric(telcom$StreamingTV)
telcom$StreamingMovies <- as.numeric(telcom$StreamingMovies)
telcom$Contract <- as.numeric(telcom$Contract)
telcom$PaperlessBilling <- as.numeric(telcom$PaperlessBilling)
telcom$PaymentMethod <- as.numeric(telcom$PaymentMethod)
telcom$Churn <- as.numeric(telcom$Churn)
str(telcom)
telcom1<-sqldf("SELECT gender, SeniorCitizen, Partner, Dependents,
               PhoneService, MultipleLines, InternetService, OnlineSecurity, OnlineBackup, DeviceProtection,
               Techsupport, StreamingTV, StreamingMovies, COntract, PaperlessBilling, PaymentMethod,
               Churn FROM telcom")

corMatrix <- cor(telcom1)

corrplot(corMatrix)
par(mfrow=c(1,1))
corrplot(corMatrix, method="circle", type="lower", addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=90, #Text label color and rotation
         diag=TRUE, sig.level = 0.05, insig = "blank")




#PreProcessing data for Categorical EDA

#Categorizing Tenure
telco %>%
  mutate(tenure_year = case_when(tenure <= 12 ~ "0-1 year",
                                 tenure > 12 & tenure <= 24 ~ "1-2 years",
                                 tenure > 24 & tenure <= 36 ~ "2-3 years",
                                 tenure > 36 & tenure <= 48 ~ "3-4 years",
                                 tenure > 48 & tenure <= 60 ~ "4-5 years",
                                 tenure > 60 & tenure <= 72 ~ "5-6 years")) -> telco
telco$tenure <-NULL
table(telco$tenure_year)

#Removing "No services"
table(telco[, c("PhoneService","MultipleLines")])

table(telco[, c("InternetService", "OnlineSecurity")])
table(telco[, c("InternetService", "OnlineBackup")])
table(telco[, c("InternetService", "DeviceProtection")])
table(telco[, c("InternetService", "TechSupport")])
table(telco[, c("InternetService", "StreamingTV")])
table(telco[, c("InternetService", "StreamingMovies")])

#Categorizing Variables for model
telco %>%
  mutate(SeniorCitizen = ifelse(SeniorCitizen == 0, "No", "Yes")) -> categorical

categorical %>%
  dplyr::select(gender:Dependents, PhoneService:PaymentMethod, Churn) -> categorical 

categorical %>%
  dplyr::select(MultipleLines, OnlineSecurity:StreamingMovies, Churn) %>%
  filter(MultipleLines != "No phone service" &
           OnlineSecurity != "No internet service") -> c2

gather(c2, columns, value, -Churn) -> c3

categorical %>%
  dplyr::select(Contract:Churn) -> c4

categorical %>%
  dplyr::select(gender:Dependents, PhoneService, InternetService, Churn) %>%
  mutate(Gender_male = ifelse(gender =="Male", "Yes", "No")) -> c1 

c1$gender <- NULL


#EDA Plots for Categorical Variables
ggplot(c3)+
  geom_bar(aes(x = value, fill = Churn), position = "fill", stat = "count")+
  facet_wrap(~columns)+ 
  xlab("Attributes")


ggplot(c4) +
  geom_bar(aes(x = Contract, fill = Churn), position = "fill", stat = "count", 
           show.legend = F) -> p7

ggplot(c4) +
  geom_bar(aes(x = PaperlessBilling, fill = Churn), position = "fill", stat = "count", 
           show.legend = T) -> p8

ggplot(c4) +
  geom_bar(aes(x = PaymentMethod, fill = Churn), position = "fill", stat = "count", 
           show.legend = F) +
  scale_x_discrete(labels = c("Bank transfer", "Credit card", "Electronic check", "Mail check"))+
  theme(axis.text= element_text(size=7)) -> p9

ggarrange(p7,p8,p9, ncol = 2, nrow = 2)



ggplot(c1) +
  geom_bar(aes(x = Gender_male, fill = Churn), position = "fill", stat = "count", 
           show.legend = F) -> p1
ggplot(c1) +
  geom_bar(aes(x = SeniorCitizen, fill = Churn), position = "fill", stat = "count", 
           show.legend = F) -> p2
ggplot(c1) +
  geom_bar(aes(x = Partner, fill = Churn), position = "fill", stat = "count", 
           show.legend = F) -> p3    
ggplot(c1) +
  geom_bar(aes(x = Dependents, fill = Churn), position = "fill", stat = "count", 
           show.legend = F) -> p4  
ggplot(c1) +
  geom_bar(aes(x = PhoneService, fill = Churn), position = "fill", stat = "count", 
           show.legend = F) -> p5
ggplot(c1) +
  geom_bar(aes(x = InternetService, fill = Churn), position = "fill", stat = "count", 
           show.legend = F) -> p6

ggarrange(p1,p2,p3,p4,p5,p6, ncol = 3, nrow = 2)


#Logistic regression model

telco_lr <- telco

#DATA PREPARATION for MODEL

telco_lr %>%
  mutate(Churn = ifelse(Churn == "Yes", 1, 0)) -> telco_lr
telco_lr %>%
  mutate(gender = ifelse(gender == "Female", 1, 0)) -> telco_lr
telco_lr %>%
  mutate(Partner = ifelse(Partner == "Yes", 1, 0)) -> telco_lr
telco_lr %>%
  mutate(PhoneService = ifelse(PhoneService == "Yes", 1, 0)) -> telco_lr
telco_lr %>%
  mutate(Dependents = ifelse(Dependents == "Yes", 1, 0)) -> telco_lr
telco_lr %>%
  mutate(PaperlessBilling = ifelse(PaperlessBilling == "Yes", 1, 0)) -> telco_lr


telco_lr$customerID <- NULL
dmy <- dummyVars(" ~ .", data = telco_lr)
dmy <- data.frame(predict(dmy, newdata = telco_lr))
str(dmy)


dmy$MultipleLinesNo.phone.service <- NULL
dmy$OnlineSecurityNo.internet.service <- NULL
dmy$OnlineBackupNo.internet.service <- NULL
dmy$DeviceProtectionNo.internet.service <- NULL
dmy$TechSupportNo.internet.service <- NULL
dmy$StreamingTVNo.internet.service <- NULL
dmy$StreamingMoviesNo.internet.service <- NULL

dmy$ContractTwo.year <- NULL
dmy$InternetServiceNo <- NULL
dmy$PaymentMethodMailed.check <- NULL
dmy$tenure_year5.6.years <- NULL

str(dmy)

#SPlitting Training and Test Data Set
set.seed(818)
assignment <- sample(0:1, size= nrow(dmy), prob = c(0.75,0.25), replace = TRUE)
train <- dmy[assignment == 0, ]
test <- dmy[assignment == 1, ]

#LR model 1 with all variables
model1 <- glm(Churn ~., family = "binomial", data = train)
summary(model1)

#LR model 2 with significant variables
model2 <- stepAIC(model1, trace = 0)
summary(model2)
vif(model2)

#LR model 3 eliminating high Variance inflation factor
model3 <- glm(formula = Churn ~  SeniorCitizen + Dependents + PhoneService + MultipleLines.No + InternetService.DSL + OnlineBackup.No +
                DeviceProtection.No + StreamingTV.No + StreamingMovies.No + Contract.Month.to.month + Contract.One.year + 
                PaperlessBilling + PaymentMethod.Electronic.check + MonthlyCharges + tenure_year0.1.year + tenure_year1.2.years,
              family = "binomial", data = train)
vif(model3)


#LR final model eliminating high P values
model4 <- glm(formula = Churn ~  SeniorCitizen + Dependents + PhoneService + MultipleLines.No + InternetService.DSL + OnlineBackup.No +
                DeviceProtection.No + Contract.Month.to.month + Contract.One.year + PaperlessBilling + PaymentMethod.Electronic.check + 
                MonthlyCharges + tenure_year0.1.year + tenure_year1.2.years,
              family = "binomial", data = train)
summary(model4)
vif(model4)

#Prediction Model with 0.5 Threshold value
model_logit <- model4
predict(model_logit, data = train, type = "response") -> train_prob
predict(model_logit, newdata = test, type = "response") -> test_prob

train_pred <- factor(ifelse(train_prob >= 0.5, "Yes", "No"))
train_actual <- factor(ifelse(train$Churn == 1, "Yes", "No"))
test_pred <- factor(ifelse(test_prob >= 0.5, "Yes", "No"))
test_actual <- factor(ifelse(test$Churn == 1, "Yes", "No"))


#Sensitivity, Specificity, Accuracy and AUC measurement
confusionMatrix(data = train_pred, reference = train_actual)
roc <- roc(train$Churn, train_prob, plot= TRUE, print.auc=TRUE)

confusionMatrix(data = test_pred, reference = test_actual)
roc <- roc(test$Churn, test_prob, plot= TRUE, print.auc=TRUE)


#Absolute threshold value for model determination
pred <- prediction(train_prob, train_actual)
perf <- performance(pred, "spec", "sens")

cutoffs <- data.frame(cut=perf@alpha.values[[1]], specificity=perf@x.values[[1]], 
                      sensitivity= perf@y.values[[1]])
opt_cutoff <- cutoffs[which.min(abs(cutoffs$specificity-cutoffs$sensitivity)),]
opt_cutoff

ggplot(data = cutoffs) +
  geom_line(aes(x = cut, y = specificity, color ="red"), size = 1.5)+
  geom_line(aes(x = cut, y = sensitivity, color = "blue"), size = 1.5) +
  labs(x = "cutoff", y ="value") +
  scale_color_discrete(name = "", labels = c("Specificity", "Sensitivity"))+
  geom_vline(aes(xintercept = opt_cutoff$cut))+
  geom_text(aes(x= 0.55, y= 0.75),label="opt_cutoff = 0.3",hjust=1, size=4)


#Optimized and robust Prediction Model
train_pred_c <- factor(ifelse(train_prob >= 0.3, "Yes", "No"))
predict(model_logit, newdata = test, type = "response") -> test_prob
test_pred_c <- factor(ifelse(test_prob >= 0.3, "Yes", "No"))

#confusion matrix model analysis
confusionMatrix(data = train_pred_c, reference = train_actual)
confusionMatrix(data = test_pred_c, reference = test_actual)

#Decision Trees

#Data Preparation

telcotree <- telco
telcotree$customerID <- NULL
telcotree %>%
  mutate_if(is.character, as.factor) -> telcotree
str(telcotree)
set.seed(818)
tree <- sample(0:1, size= nrow(telcotree), prob = c(0.75,0.25), replace = TRUE)
traintree <- telcotree[tree == 0, ]
testtree <- telcotree[tree == 1, ]

#Tree Model

model_tree2 <- rpart(formula = Churn ~ gender + SeniorCitizen + Partner + Dependents + PhoneService + 
                       MultipleLines + InternetService + OnlineSecurity + TechSupport +
                       OnlineBackup + DeviceProtection + StreamingTV + StreamingMovies + 
                       Contract + PaperlessBilling + tenure_year +
                       PaymentMethod + MonthlyCharges, data = traintree, 
                     method = "class", parms = list(split = "gini"))

#Visualizing Tree Model

rpart.plot(model_tree2, box.palette="RdBu", shadow.col="gray", nn=TRUE)


#Predictions
predict(model_tree2, data = traintree, type = "class") -> traintree_pred2
predict(model_tree2, data = traintree, type = "prob") -> traintree_prob2
predict(model_tree2, newdata= testtree, type = "class") -> testtree_pred2
predict(model_tree2, newdata = testtree, type = "prob") -> testtree_prob2

#Confusion matrices for model analysis

confusionMatrix(data = traintree_pred2, reference = traintree$Churn)
traintree_actual <- ifelse(traintree$Churn == "Yes", 1,0)
roc <- roc(traintree_actual, traintree_prob2[,2], plot= TRUE, print.auc=TRUE)

testtree_actual <- ifelse(testtree$Churn == "Yes", 1,0)
confusionMatrix(data = testtree_pred2, reference = testtree$Churn)
roc <- roc(testtree_actual, testtree_prob2[,2], plot = TRUE, print.auc = TRUE)

#Random Forest

#Modeling

set.seed(802)
modelrf1 <- randomForest(formula = Churn ~., data = traintree)
print(modelrf1)


#Prediction using RF

predict(modelrf1, traintree, type = "class") -> trainrf_pred
predict(modelrf1, traintree, type = "prob") -> trainrf_prob
predict(modelrf1, newdata = testtree, type = "class") -> testrf_pred
predict(modelrf1, newdata = testtree, type = "prob") -> testrf_prob

#confusion Matrices Analysis

confusionMatrix(data = trainrf_pred, reference = traintree$Churn)
trainrf_actual <- ifelse(traintree$Churn == "Yes", 1,0)
roc <- roc(trainrf_actual, trainrf_prob[,2], plot= TRUE, print.auc=TRUE)

confusionMatrix(data = testrf_pred, reference = testtree$Churn)
testrf_actual <- ifelse(testtree$Churn == "Yes", 1,0)
roc <- roc(testrf_actual, testrf_prob[,2], plot = TRUE, print.auc = TRUE)

#Updated model
set.seed(818)
modelrf2 <- tuneRF(x = subset(traintree, select = -Churn), y = traintree$Churn, ntreeTry = 500, doBest = TRUE)
print(modelrf2)

#Tuning model

mtry <- seq(2, ncol(traintree) * 0.8, 2)
nodesize <- seq(3, 8, 2)
sampsize <- nrow(traintree) * c(0.7, 0.8)
hyper_grid <- expand.grid(mtry = mtry, nodesize = nodesize, sampsize = sampsize)

oob_err <- c()
for (i in 1:nrow(hyper_grid)) {
  model <- randomForest(formula = Churn ~ ., 
                        data = traintree,
                        mtry = hyper_grid$mtry[i],
                        nodesize = hyper_grid$nodesize[i],
                        sampsize = hyper_grid$sampsize[i])
  oob_err[i] <- model$err.rate[nrow(model$err.rate), "OOB"]
}

opt_i <- which.min(oob_err)
print(hyper_grid[opt_i,])

#Training model with parameters

set.seed(802)
modelrf3 <- randomForest(formula = Churn ~., data = traintree, mtry = 2, nodesize = 7, sampsize = 3658.2)
print(modelrf3)

#Predictions using updated random forest model

predict(modelrf2, traintree, type = "class") -> trainrf_pred2
predict(modelrf2, traintree, type = "prob") -> trainrf_prob2
predict(modelrf2, newdata = testtree, type = "class") -> testrf_pred2
predict(modelrf2, newdata = testtree, type = "prob") -> testrf_prob2

#confusion matrices for updated random forest

confusionMatrix(data = trainrf_pred2, reference = traintree$Churn)
trainrf_actual <- ifelse(traintree$Churn == "Yes", 1,0)
roc <- roc(trainrf_actual, trainrf_prob2[,2], plot= TRUE, print.auc=TRUE)

confusionMatrix(data = testrf_pred2, reference = testtree$Churn)
testrf_actual <- ifelse(testtree$Churn == "Yes", 1,0)
roc <- roc(testrf_actual, testrf_prob2[,2], plot = TRUE, print.auc = TRUE)

#RF visualization
varImpPlot(modelrf2,type=2)
  
#Model comparison

preds_list <- list(test_prob, testtree_prob2[,2], testrf_prob2[,2])
m <- length(preds_list)
actuals_list <- rep(list(testtree$Churn), m)

pred <- prediction(preds_list, actuals_list)
rocs <- performance(pred, "tpr", "fpr")
plot(rocs, col = as.list(1:m), main = "Test Set ROC Curves for 3 Models")
legend(x = "bottomright",
       legend = c("Logistic Regression", "Decision Tree", "Random Froest"),
       fill = 1:m)






















