#set working directory
setwd("C:/Users/Okwani/Desktop")

#install useful packages and libraries
install.packages("readxl")
library(readxl)
install.packages("psych")
library(psych)
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)
install.packages("tidyverse")
library(tidyverse)
install.packages("caret")
library(caret)
install.packages("corrplot")
library(corrplot)
install.packages("caret")
library(caret)
install.packages("lmtest")
library(lmtest)
install.packages("car")
library(car)
install.packages("pROC")
library(pROC)
install.packages("gmodels")
library(gmodels)
install.packages("tree")
library(tree)

#read in the bank data
data <- read_excel("C:/Users/Okwani/Desktop/banksv.xlsx")

#perform descriptive statistics to fully understand the data and identify mean,mode and
median as well as other unique characteristics of the 22 variables
summary(data)
describe(data)
describeBy(data)

#create a tab with the descriptive summary and download the csv file
tab<-describe(data)
write.csv(tab, "describe.csv")

#view the structure, head and tail of dataset
str(data)
head(data)
tail(data)

#view the sum of missing values in dataset
sum(is.na(data))
#using descriptive functions individually to assess some variables
mean(data$pdays, na.rm = TRUE)
min(data$euribor3m, na.rm = TRUE)
max(data$euribor3m, na.rm = TRUE)
mean(data$euribor3m)

#assessing all the levels within certain variables and counting how many observations are
in each level
data %>% group_by(education) %>% count()
data %>% group_by(day_of_week) %>% count()
data %>% group_by(marital) %>% count()
data %>% group_by(poutcome) %>% count()
data %>% group_by(loan) %>% count()
data %>% group_by(housing) %>% count() 
data %>% group_by(default) %>% count()
data %>% group_by(job) %>% count()
data %>% group_by(month) %>% count()
data %>% group_by(contact) %>% count()
data %>% group_by(subscribed) %>% count()

#viewing the percentage of people that subscribed in a variable
round(prop.table(table(data$subscribed))*100,1)
round(prop.table(table(data$education))*100,1)
round(prop.table(table(data$marital))*100,1)
round(prop.table(table(data$poutcome))*100,1)
round(prop.table(table(data$loan))*100,1)
round(prop.table(table(data$housing))*100,1)
round(prop.table(table(data$default))*100,1)

#more specifically viewing the percentage of people that subscribed within a level in the variable

data %>%
  select(marital, subscribed) %>%
  group_by(marital) %>%
  count(subscribed) %>%
  ungroup() %>%
  spread(key = subscribed, value = n) %>%
  mutate(total = no + yes,
         subscription_rate = round((yes/total)*100,2)) %>%
  select(marital,subscription_rate) %>%
  arrange(subscription_rate)

data %>%
  select(day_of_week, subscribed) %>%
  group_by(day_of_week) %>%
  count(subscribed) %>%
  ungroup() %>%
  spread(key = subscribed, value = n) %>%
  mutate(total = no + yes,
         subscription_rate = round((yes/total)*100,2)) %>%
  select(day_of_week,subscription_rate) %>%
  arrange(subscription_rate)

data %>%
  select(education, subscribed) %>%
  group_by(education) %>%
  count(subscribed) %>%
  ungroup() %>%
  spread(key = subscribed, value = n) %>%
  mutate(total = no + yes,
         subscription_rate = round((yes/total)*100,2)) %>%
  select(education,subscription_rate) %>%
  arrange(subscription_rate)

data %>%
  select(job, subscribed) %>%
  group_by(job) %>%
  count(subscribed) %>%
  ungroup() %>%
  spread(key = subscribed, value = n) %>%
  mutate(total = no + yes,
         subscription_rate = round((yes/total)*100,2)) %>%
  select(job,subscription_rate) %>%
  arrange(subscription_rate)

data %>%
  select(housing, subscribed) %>%
  group_by(housing) %>%
  count(subscribed) %>%
  ungroup() %>%
  spread(key = subscribed, value = n) %>%
  mutate(total = no + yes,
         subscription_rate = round((yes/total)*100,2)) %>%
  select(housing,subscription_rate) %>%
  arrange(subscription_rate)

data %>%
  select(default, subscribed) %>%
  group_by(default) %>%
  count(subscribed) %>%
  ungroup() %>%
  spread(key = subscribed, value = n) %>%
  mutate(total = no + yes,
         subscription_rate = round((yes/total)*100,2)) %>%
  select(default,subscription_rate) %>%
  arrange(subscription_rate)

data %>%
  select(poutcome, subscribed) %>%
  group_by(poutcome) %>%
  count(subscribed) %>%
  ungroup() %>%
  spread(key = subscribed, value = n) %>%
  mutate(total = no + yes,
         subscription_rate = round((yes/total)*100,2)) %>%
  select(poutcome,subscription_rate) %>%
  arrange(subscription_rate)

data %>%
  select(loan, subscribed) %>%
  group_by(loan) %>%
  count(subscribed) %>%
  ungroup() %>%
  spread(key = subscribed, value = n) %>%
  mutate(total = no + yes,
         subscription_rate = round((yes/total)*100,2)) %>%
  select(loan,subscription_rate) %>%
  arrange(subscription_rate)

data %>%
  select(contact, subscribed) %>%
  group_by(contact) %>%
  count(subscribed) %>%
  ungroup() %>%
  spread(key = subscribed, value = n) %>%
  mutate(total = no + yes,
         subscription_rate = round((yes/total)*100,2)) %>%
  select(contact,subscription_rate) %>%
  arrange(subscription_rate)

data %>%
  select(month, subscribed) %>%
  group_by(month) %>%
count(subscribed) %>%
  ungroup() %>%
  spread(key = subscribed, value = n) %>%
  mutate(total = no + yes,
         subscription_rate = round((yes/total)*100,2)) %>%
  select(month,total, subscription_rate) %>%
  arrange(subscription_rate)

data %>%
  select(education, subscribed) %>%
  group_by(education) %>%
  count(subscribed) %>%
  ungroup() %>%
  spread(key = subscribed, value = n) %>%
  mutate(total = no + yes,
         subscription_rate = round((yes/total)*100,2)) %>%
  select(education,total,subscription_rate) %>%
  arrange(subscription_rate)

#summarising the data visually
hist(data$age)
hist(data$euribor3m)
boxplot(data$age)

euribor_bar <- ggplot(data, aes(euribor3m))
euribor_bar + geom_bar(color = "blue") + theme(text = element_text(size=20))
subscription_bar <- ggplot(data, aes(subscribed))
subscription_bar + geom_bar(color = "black",fill = "blue") + theme(text =
                                                                     element_text(size=20))
education_bar <- ggplot(data, aes(education))
education_bar + geom_bar(color = "black",fill = "red")+theme(text = element_text(size=20),
                                                             axis.text.x=element_text(angle = 90, vjust = 0.5, hjust=1,size=10))
data$day_of_week <- factor(data$day_of_week,levels = c("mon", "tue","wed","thu","fri"))
day_bar <- ggplot(data, aes(day_of_week))
day_bar + geom_bar(color = "black",fill = "blue") + theme(text = element_text(size=20),
                                                          axis.text.x=element_text(angle = 90, vjust = 0.5, hjust=1,size=10))
camapaign_bar <- ggplot(data, aes(campaign))
camapaign_bar + geom_bar(color = "black",fill = "blue") + theme(text = element_text(size=20),
                                                                axis.text.x=element_text(angle = 90, vjust = 0.5, hjust=1,size=10))
loan_bar <- ggplot(data, aes(loan))
loan_bar + geom_bar(color = "black",fill = "purple") + theme(text = element_text(size=20),
                                                             axis.text.x=element_text(angle = 90, vjust = 0.5, hjust=1,size=10))
outcome_bar <- ggplot(data, aes(poutcome))
outcome_bar + geom_bar(color = "black",fill = "purple") + theme(text = element_text(size=20),
                                                                axis.text.x=element_text(angle = 90, vjust = 0.5, hjust=1,size=10))
mstatus_bar <- ggplot(data, aes(marital))
mstatus_bar + geom_bar(color = "black",fill = "red") + theme(text = element_text(size=20),
                                                             axis.text.x=element_text(angle = 90, vjust = 0.5, hjust=1,size=10))
housing_bar <- ggplot(data, aes(housing))
housing_bar + geom_bar(color = "black",fill = "blue") + theme(text = element_text(size=20),
                                                              axis.text.x=element_text(angle = 90, vjust = 0.5, hjust=1,size=10))
default_bar <- ggplot(data, aes(default))
default_bar + geom_bar(color = "black",fill = "purple") + theme(text = element_text(size=20),
                                                                axis.text.x=element_text(angle = 90, vjust = 0.5, hjust=1,size=10))
#rectifying data quality issues
#replacing the outliers evident in age with median
data$age[data$age > 99] <- 38
data$age[data$age < 18] <- 38

#Imputational errors
#recoding basic 4, 6 and 9 with basic
data$education[data$education == "basic.4y"] <-"basic"
data$education[data$education == "basic.6y"] <-"basic"
data$education[data$education == "basic.9y"] <-"basic"
data$default[data$default == "n"] <-"no"
data$month[data$month == "march"] <- "mar"

#replacing 999 with no contact as specified in data dictionary for pdays
data$pdays[data$pdays == "999"] <-"no contact"
#categorical rectification from factor to numerical
data$marital <- as.numeric(factor(data$marital,
                                  levels = c("unknown","single", "married", "divorced"), labels = c(0,1,2,3)
                                  ,ordered = TRUE))
data$loan <- as.numeric(factor(data$loan,
                               levels = c("unknown","no", "yes"), labels = c(0,1,2) ,ordered = TRUE))
data$housing <- as.numeric(factor(data$housing,
                                  levels = c("unknown","no", "yes"), labels = c(0,1,2) ,ordered = TRUE))
data$default <- as.numeric(factor(data$default,
                                  levels = c("unknown","no", "yes"), labels = c(0,1,2) ,ordered = TRUE))

#Missing variables
data$month <- replace(data$month, is.na(data$month), 0)
data$pdays<- replace(data$pdays, is.na(data$pdays), 0)

#remove near zero variables that might affect prediction and this drops pdays
nearzero.data <- nearZeroVar(data, saveMetrics = TRUE)
drop.cols <- rownames(nearzero.data)[nearzero.data$nzv == TRUE]
data <-data[,!names(data)%in% drop.cols]

#remove the duration (it is only known during call execution and is not a going determinant of term deposit subscription) and ID column as they are not relevant to this project
data<-select(data, -duration)
data<-select(data, -ID)

#visualize age to show rectification of outliers
boxplot(data$age)

#Visualisation of selected data using ggplot
#Independent variables - age, euribor3m, poutcome and number of employees
#control variables - education, campaign, marital, previous,days of the week

#visualising the relationship between age and term deposits
data %>%
  mutate(age_groups = cut(age, breaks = seq(18, 99, 10), include.lowest = T)) %>%
  ggplot(aes(age_groups, fill = subscribed))+
  geom_bar(stat = "count", position = "fill")+
  scale_y_continuous(labels = scales::percent_format())+
  labs(title = "Relationship between Age and Term Deposit Subscription",
       y = "Rate",
       x = "Age Group",
       fill = "Outcome")

#visualising term deposits subscription by previous outcome of campaign
data %>%
  ggplot(aes(poutcome, fill = subscribed))+
  geom_bar(stat = "count", position = "fill")+
  scale_y_continuous(labels = scales::percent_format())+
  labs(title = "Relationship between Previous campaign outcome and Term Deposit Subscription",
       y = "Rate",
       x = "Poutcome",
       fill = "Outcome")

#visualising the relationship between education level and term subscription rates
data %>%
  select(education, subscribed) %>%
  group_by(education) %>%
count(subscribed) %>%
  ungroup() %>%
  mutate(education = str_to_title(education)) %>%
  spread(key= subscribed, value = n) %>%
  mutate(positivity_ratio = round((yes/(yes+no)),3)) %>%
  mutate(education = reorder(education, positivity_ratio)) %>%
  arrange(desc(positivity_ratio)) %>%
  ggplot(aes(positivity_ratio,education, col = education))+
  geom_point()+
  geom_segment(aes(xend = 0, x = positivity_ratio, y = education, yend = education))+
  labs(
    title = "Education and Term Subscription Rates",
    y = NULL,
    x = NULL
  )+
  scale_x_continuous(labels = scales::percent_format())

#correlation between age, campaign and subscription
data %>%
  select(age, campaign, subscribed) %>%
  ggplot(aes(y = campaign, x = age, col = subscribed)) +
  geom_point(alpha = 0.3)+
  geom_hline(yintercept = 8)+
  scale_y_log10()+
  scale_x_log10()+
  labs(
    y = "Contact during Campaign",
    x = "Age",
    col = "Outcome of campaign"
  )

#visualising the relationship between Number of Employees and term subscription rates
data %>%
  select(nr.employed,subscribed) %>%
  group_by(nr.employed)%>%
  count(subscribed) %>%
  ungroup() %>%
  mutate(nr.employed= str_to_title(nr.employed)) %>%
  spread(key= subscribed, value = n) %>%
  mutate(positivity_ratio = round((yes/(yes+no)),3)) %>%
mutate(nr.employed = reorder(nr.employed, positivity_ratio)) %>%
  arrange(desc(positivity_ratio)) %>%
  ggplot(aes(positivity_ratio,nr.employed, col = nr.employed))+
  geom_point()+
  geom_segment(aes(xend = 0, x = positivity_ratio, y = nr.employed, yend = nr.employed))+
  labs(
    title = "Number of Employees and Term Deposit subscription rates",
    y = NULL,
    x = NULL
  )+
  scale_x_continuous(labels = scales::percent_format())

#Visualising the relationship between day of the week and term deposit subscription
data %>%
  ggplot(aes(day_of_week, fill = subscribed))+
  geom_bar(stat = "count", position = "fill")+
  scale_y_continuous(labels = scales::percent_format())+
  labs(title = "Relationship between Day of the week and Term Deposit Subscription",
       y = "Rate",
       x = "Day of the week",
       fill = "Outcome")

#data preparation for t-test.
#replace no with 0 and yes with 1 in subscribed
data$subscribed[data$subscribed == "no"] <-0
data$subscribed[data$subscribed == "yes"] <-1
#change subscribed to numeric for t test
data$subscribed <- as.numeric(data$subscribed)
data$poutcome <- as.numeric(factor(data$poutcome,
                                   levels = c("nonexistent","failure", "success"), labels = c(0,1,2) ,ordered =
                                     TRUE))
data$poutcome <- as.numeric(data$poutcome)

#categorise other variables as factor for the t-test
sapply(data, class)
data[sapply(data, is.character)] <- lapply(data[sapply(data, is.character)], as.factor)
sapply(data, class)
#correlation test
t.test(data$subscribed, data$euribor3m)
t.test(data$subscribed,data$age)
t.test(data$subscribed, data$poutcome)
t.test(data$subscribed, data$nr.employed)

#visualise the correlation using a plot
subdata <- data[c("age","euribor3m", "poutcome", "nr.employed", "subscribed")]
cor <- cor(subdata)
cor_sort <- as.matrix(sort(cor[,'subscribed'], decreasing = TRUE))
corrplot.mixed(cor, tl.col = "blue", tl.pos = "lt")

#change subscribed and poutcome back to factor
data$subscribed <- as.factor(data$subscribed)
data$poutcome <-as.factor(data$poutcome)

#Split data into test and train
set.seed(40386014)
index <- createDataPartition(data$subscribed, p= 0.8, list=FALSE)
train <- data[index,]
test <- data[-index,]

#begin model creation
#model 1
formula1 <- subscribed ~ age + euribor3m + poutcome + nr.employed
model1 <- glm(formula1, data = train, family = "binomial")
summary(model1)
formula2 <- subscribed ~ age + euribor3m + poutcome + nr.employed+ education
model2 <- glm(formula2, data = train, family = "binomial")
summary(model2)
formula3 <- subscribed ~ age + euribor3m + poutcome + nr.employed+ education + campaign
model3 <- glm(formula3, data = train, family = "binomial")
summary(model3)
formula4 <- subscribed ~ age + euribor3m + poutcome + nr.employed+ education + campaign +
  marital
model4 <- glm(formula4, data = train, family = "binomial")
summary(model4)
formula<- subscribed ~ age + euribor3m + poutcome + nr.employed+ education + campaign +
  marital + day_of_week
model <- glm(formula, data = train, family = "binomial")
summary(model)

#assessing the R square of the model
logisticPseudoR2s <- function(LogModel) {
  dev <- LogModel$deviance
  nullDev <- LogModel$null.deviance
  modelN <- length(LogModel$fitted.values)
  R.l <- 1 - dev / nullDev
  R.cs <- 1- exp ( -(nullDev - dev) / modelN)
  R.n <- R.cs / ( 1 - ( exp (-(nullDev / modelN))))
  cat("Pseudo R^2 for logistic regression\n")
  cat("Hosmer and Lemeshow R^2 ", round(R.l, 3), "\n")
  cat("Cox and Snell R^2 ", round(R.cs, 3), "\n")
  cat("Nagelkerke R^2 ", round(R.n, 3), "\n")
}
logisticPseudoR2s(model)
exp(model$coefficients)
exp(confint(model))


#train residual
train_residuals <- resid(model)
train_predictions <- fitted(model)
hist(train_residuals, breaks = 40)
train$standardisedResiduals <- rstandard(model)
sum(train$standardisedResiduals > 1.96)

#Assessing Influential Cases
train$cook <- cooks.distance(model)
sum(train$cook > 1) #returned 0, meaning that there are no influential cases
table(train$cook)

#accessing multicollinearity
vif(model)

#vif showed no output above 10 meaning that there is no need for concern and the
assumption is fulfilled.

#Prediction
data$subscribed = factor(data$subscribed,levels=c("0","1"))
predictions <- predict(model, test, type = "response")
class_pred<- ifelse(predictions > .5, "1", "0")
class_pred = factor(class_pred,levels=c("0","1"))
confusionMatrix(table(class_pred, test$subscribed))
postResample(class_pred, test$subscribed)

#Check AUC-ROC for predictive accuracy
# create roc curve
roc_object <- roc( test$subscribed, predictions)

# calculate area under curve
auc( roc_object )

#Area under the curve: 0.7607

#checking independence of errors
dwtest(model)

#tests the linearity of the logit
ageLogInt <- log(data$age)*data$age
euriborLogInt <- log(data$euribor3m)*data$euribor3m
nr.employedLogInt <- log(data$nr.employed)*data$nr.employed
campaignLogInt <- log(data$campaign)*data$campaign
formula5 <- subscribed ~ ageLogInt + euriborLogInt +nr.employedLogInt+campaignLogInt +
  education + marital + day_of_week + age +euribor3m + nr.employed + campaign
model5 <- glm(formula5, data = data, family = "binomial")
summary(model5)
