#Install and load libraries
if(!require(e1071)) install.packages("e1071", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(rmarkdown)) install.packages("rmarkdown", repos = "http://cran.us.r-project.org")
if(!require(class)) install.packages("class", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(ggridges)) install.packages("ggridges", repos = "http://cran.us.r-project.org")

library(stringr)
library(tidyverse)
library(caret)
library(data.table)
library(lubridate)
library(readxl)
library(knitr)
library(rmarkdown)
library(e1071)
library(class)
library(randomForest)
library(ggridges)

#Importing data
bank <- read_xls("C:/Users/felip/Documentos/Estudo/R/Curso DataScience/Final project - choose you own/Bank/default.xls", sheet = 1, range = "B2:Y30002", col_names = TRUE)

#Cleaning data. 
bank$SEX <- factor(bank$SEX, levels = c(1,2), labels = c("Male", "Female"))
bank$EDUCATION <- factor(bank$EDUCATION, levels = c(1,2,3,4), labels = c("graduate","university","high_school", "others"))
bank$MARRIAGE <- factor(bank$MARRIAGE, levels = c(1,2,3), labels = c("Married", "Single", "Others"))
bank$`default payment next month` <- factor(bank$`default payment next month`, levels = c(0,1), labels = c("Not default","default"))
bank <- rename(bank, default_payment_next_month = `default payment next month`, credit_amount = LIMIT_BAL)
bank <- select(bank, c(-(PAY_4:PAY_6), -(BILL_AMT4:BILL_AMT6), -(PAY_AMT4:PAY_AMT6)))
bank <- filter(bank, !is.na(credit_amount) & !is.na(SEX) & !is.na(EDUCATION) & !is.na(MARRIAGE) & !is.na(AGE) & !is.na(PAY_0) & !is.na(PAY_2) & !is.na(PAY_3) & !is.na(BILL_AMT1) & !is.na(BILL_AMT2) & !is.na(BILL_AMT3) & !is.na(default_payment_next_month))
correlation <- cor(select(bank, c(-(SEX:MARRIAGE), -default_payment_next_month)))

#Data analysis and visualization
summary(bank$AGE)
summary(bank$credit_amount)
table(bank$MARRIAGE)
table(bank$SEX)

bank %>% group_by(SEX) %>% summarise(age = mean(AGE), limit_bal = mean(credit_amount), default = mean(default_payment_next_month == "default"))
bank %>% group_by(EDUCATION) %>% summarise(age = mean(AGE), limit_bal = mean(credit_amount), default = mean(default_payment_next_month == "default"))
bank %>% group_by(MARRIAGE) %>% summarise(age = mean(AGE), limit_bal = mean(credit_amount), default = mean(default_payment_next_month == "default"))
bank %>% group_by(AGE) %>% summarise(limit_bal = mean(credit_amount), default = mean(default_payment_next_month == "default"))

ggplot(data = bank, mapping = aes(x = SEX, fill = default_payment_next_month)) + geom_bar(position = "fill") + labs(title = "Graphic 1.1: Default by SEX")
ggplot(data = bank, mapping = aes(x = MARRIAGE, fill = default_payment_next_month)) + geom_bar(position = "fill") + labs(title = "Graphic 1.2: Default by MARRIAGE")
ggplot(data = bank, mapping = aes(x = EDUCATION, fill = default_payment_next_month)) + geom_bar(position = "fill") + labs(title = "Graphic 1.3: Default by EDUCATION")
ggplot(data = bank, mapping = aes(x = AGE, fill = default_payment_next_month)) + geom_bar(position = "fill") + labs(title = "Graphic 1.4: Default by AGE")

ggplot(data = bank, mapping = aes(AGE, fill = SEX)) + geom_density(col = "black", alpha = 0.2) + labs(title = "Graphic 2.1: Age (density plot) by sex") + facet_grid(bank$default_payment_next_month)
ggplot(data = bank, mapping = aes(AGE, fill = MARRIAGE)) + geom_density(col = "black", alpha = 0.2) + facet_grid(bank$default_payment_next_month) + labs(title = "Graphic 2.2: Age (density plot) by marriage")
ggplot(data = bank, mapping = aes(AGE, EDUCATION)) + geom_density_ridges() + labs(title = "Graphic 2.3: Age (density plot) by education") + facet_grid(bank$default_payment_next_month)

ggplot(data = bank, mapping = aes(credit_amount, fill = default_payment_next_month)) + geom_density(col = "black", alpha = 0.2) + labs(title = "Graphic 3.1: Amount of the given credit (density plot)")
ggplot(data = bank, mapping = aes(credit_amount, fill = default_payment_next_month)) + geom_density(col = "black", alpha = 0.2) + facet_grid(bank$SEX) + labs(title = "Graphic 3.2: Amount of the given credit (density plot)")
ggplot(data = bank, mapping = aes(credit_amount, fill = default_payment_next_month)) + geom_density(col = "black", alpha = 0.2) + facet_grid(bank$MARRIAGE) + labs(title = "Graphic 3.3: Amount of the given credit (density plot)")
ggplot(data = bank, mapping = aes(credit_amount, fill = default_payment_next_month)) + geom_density(col = "black", alpha = 0.2) + facet_grid(bank$EDUCATION) + labs(title = "Graphic 3.4: Amount of the given credit (density plot)")


#Models
##Create partition
index <- createDataPartition(bank$default_payment_next_month, p = 0.6, list = FALSE)
test_set <- bank[-index,]
train_set <- bank[index,]
##Model 1: logistic
fit1 <- glm(default_payment_next_month ~ credit_amount + PAY_0 + PAY_2 + PAY_3 + EDUCATION + MARRIAGE, data = train_set, family = "binomial")
pred1 <- predict(fit1, test_set, type = "response")

min(pred1)
max(pred1)
###Best cutoff
cutoff1 <- seq(min(pred1),max(pred1), 0.01)
acc <- map_dbl(cutoff1, function(x){
  pred1 <- ifelse(pred1 > x, 1, 0)
  pred1 <- factor(pred1, levels = c(0,1), labels = c("Not default","default"))
  F_meas(data = pred1, reference = test_set$default_payment_next_month, relevant = "default")
})
acc
best <- cutoff[which.max(acc)]
best
###Logistc: final result with best cutoff
pred1 <- ifelse(pred1 > best, 1, 0)
pred1 <- factor(pred1, levels = c(0,1), labels = c("Not default","default"))
confusionMatrix(pred1, test_set$default_payment_next_month)

##model 2: KNN
ks <- seq(1,100,by = 1)
###best K
overall_acc_k <- map_df(ks, function(k){
  fit2 <- knn3(default_payment_next_month ~ credit_amount + PAY_0 + PAY_2 + PAY_3 + EDUCATION + MARRIAGE, k = k, data = train_set)
  pred2 <- predict(fit2, test_set, type = "class")
  f <- F_meas(data = pred2, reference = test_set$default_payment_next_month, relevant = "default")
  tibble(f, k)
})
max_acc <- which.max(overall_acc_k$f)
max_acc
###KNN: final result with best k
fit_max<- knn3(default_payment_next_month ~ credit_amount + PAY_0 + PAY_2 + PAY_3 + EDUCATION + MARRIAGE, k = max_acc, data = train_set)
predmax <- predict(fit_max, test_set, type = "class")
confusionMatrix(predmax, test_set$default_payment_next_month)