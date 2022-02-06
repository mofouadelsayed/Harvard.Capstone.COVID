options(warn=-1)

###################################################################
# WARNING: Due to the size of this Dataset & the use of randomForest 
# model, running this code might take some time to finish processing
###################################################################

# Installing packages if required
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")
if(!require(ggpubr)) install.packages("ggpubr", repos = "http://cran.us.r-project.org")
if(!require(ggrepel)) install.packages("ggrepel", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(Amelia)) install.packages("Amelia", repos = "http://cran.us.r-project.org")
if(!require(gcookbook)) install.packages("gcookbook", repos = "http://cran.us.r-project.org")
if(!require(CatEncoders)) install.packages("CatEncoders", repos = "http://cran.us.r-project.org")
if(!require(cleandata)) install.packages("cleandata", repos = "http://cran.us.r-project.org")
if(!require(caTools)) install.packages("caTools", repos = "http://cran.us.r-project.org")
if(!require(ROSE)) install.packages("ROSE", repos = "http://cran.us.r-project.org")
if(!require(party)) install.packages("party", repos = "http://cran.us.r-project.org")
if(!require(ranger)) install.packages("ranger", repos = "http://cran.us.r-project.org")

# Load Libraries
library(tidyverse)
library(ggplot2)
library(scales)
library(ggpubr)
library(ggrepel)
library(caret)
library(Amelia)
library(gcookbook)
library(CatEncoders)
library(cleandata)
library(caTools)
library(ROSE)
library(party)
library(ranger)

# Download from GitHub
covid_alberta <- read_csv("https://raw.githubusercontent.com/mofouadelsayed/Harvard.Capstone/main/covid-19-alberta-statistics-data.csv")

# Rename Columns
colnames(covid_alberta)<- c("Number", "Date", "Zone", "Gender", "Age_group", "Status",
                            "Type")

# Exclude Unknown entries
covid_alberta<- covid_alberta %>% filter(!Age_group== "Unknown") %>% 
  filter(!Gender== "Unknown") %>% filter(!Status== "Active")

# Glimpse Dataset
glimpse(covid_alberta)

###################################
# Data Exploration & Visualization
##################################

# Daily cases clearly showing the pandemic waves
daily_cases<-covid_alberta %>% filter(Date< "2021-09-30")  %>% group_by(Date) %>% summarize(Cases= n())
daily_cases %>% ggplot(aes(Date, Cases)) + geom_line(color= "#264653", size= 0.75) + theme_bw() +  
  ggtitle("Daily COVID Cases in Alberta") + 
  theme(plot.title = element_text(size = 14, face = "bold", colour = "#264653", margin = margin(0,0,15,0)), 
        axis.title = element_text(size = 12, face = "bold", colour = "#264653"), axis.text = element_text(face= "bold", color= "#f9844a"), 
        panel.border = element_blank(), panel.background = element_blank()) + scale_y_continuous(labels = comma)

# Cases by Age group & Gender
within(covid_alberta, Age_group <- factor(Age_group, levels=names(sort(table(Age_group), decreasing=TRUE)))) %>% 
  ggplot(aes(Age_group,fill=Gender)) + geom_bar(position = position_dodge()) + 
  theme(axis.text.x = element_text(angle=90, hjust=1), 
        plot.title = element_text(size = 14, face = "bold", colour = "#264653", margin = margin(0,0,15,0)), 
        axis.title = element_text(size = 12, face = "bold", colour = "#264653"), 
        axis.text = element_text(face= "bold", color= "#264653"), 
        panel.border = element_blank(), panel.background = element_blank()) + 
  geom_text(stat = 'count' , aes(label= ..count..), 
            position = position_dodge(0.95), color= "white", vjust= 1.2, size=1.5, fontface= "bold") + 
  scale_fill_manual(values = c("#264653", "#f4a261")) + ggtitle("Case Counts by Age Group & Gender") + 
  scale_y_continuous(labels = comma) + ylab("Case Count") + xlab("Age Group")

# Case status by Age Group
within(covid_alberta, Age_group <- factor(Age_group, levels=names(sort(table(Age_group), decreasing=TRUE)))) %>% 
  ggplot(aes(Age_group,fill=Status)) + geom_bar(position = position_dodge()) + 
  theme(axis.text.x = element_text(angle=90, hjust=1), 
        plot.title = element_text(size = 14, face = "bold", colour = "#264653", margin = margin(0,0,15,0)), 
        axis.title = element_text(size = 12, face = "bold", colour = "#264653"), 
        axis.text = element_text(face= "bold", color= "#264653"), 
        panel.border = element_blank(), panel.background = element_blank()) + 
  geom_text(stat = 'count' , aes(label= ..count..), 
            position = position_dodge(0.95), color= "#264653", vjust= -1, size=2, fontface= "bold") + 
  scale_fill_manual(values = c("#264653", "#f4a261")) + ggtitle("Recovery Status by Age Group") + 
  scale_y_continuous(labels = comma) + ylab("Count") + xlab("Age Group")

# Status by Gender
covid_alberta %>% ggplot(aes(Gender,fill=Status)) + geom_bar(position = position_dodge()) +  
  scale_y_continuous(labels = comma) + 
  theme(plot.title = element_text(size = 14, face = "bold", colour = "#264653", margin = margin(0,0,15,0)), 
        axis.title = element_text(size = 12, face = "bold", colour = "#264653"), 
        axis.text = element_text(face= "bold", color= "#264653"), panel.border = element_blank(), panel.background = element_blank()) + 
  geom_text(stat = 'count' , aes(label= ..count..), 
            position = position_dodge(0.95), color= "#264653", vjust= -0.5, size=3.5, fontface= "bold") + 
  scale_fill_manual(values = c("#264653", "#f4a261")) + ggtitle("Recovery Status by Gender") + ylab("Count")

# Death by Gender & Age group
covid_alberta %>% filter(Status== "Died") %>% ggplot(aes(Age_group,fill=Gender)) + 
  geom_bar(position = position_dodge()) + 
  theme(axis.text.x = element_text(angle=90, hjust=1), 
        plot.title = element_text(size = 14, face = "bold", colour = "#264653", margin = margin(0,0,15,0)), 
        axis.title = element_text(size = 12, face = "bold", colour = "#264653"), 
        axis.text = element_text(face= "bold", color= "#264653"), panel.border = element_blank(), panel.background = element_blank()) + 
  geom_text(stat = 'count' , aes(label= ..count..), 
            position = position_dodge(0.95), color= "#264653", vjust= -0.5, size=2.5, fontface= "bold") + 
  scale_fill_manual(values = c("#264653", "#f4a261")) + ggtitle("COVID Deaths by Age Group & Gender") + 
  scale_y_continuous(labels = comma) + ylab("Deaths") + xlab("Age Group")

# Cases & Deaths by Zones
covid_alberta %>% ggplot(aes(Zone,fill=Status)) + geom_bar(position = position_dodge()) + 
  theme(axis.text.x = element_text(angle=90, hjust=1), 
        plot.title = element_text(size = 14, face = "bold", colour = "#264653", margin = margin(0,0,15,0)), 
        axis.title = element_text(size = 12, face = "bold", colour = "#264653"), 
        axis.text = element_text(face= "bold", color= "#264653"), panel.border = element_blank(), panel.background = element_blank()) + 
  geom_text(stat = 'count' , aes(label= ..count..), 
            position = position_dodge(0.95), color= "#264653", vjust= -0.5, size=2.5, fontface= "bold") + 
  scale_fill_manual(values = c("#264653", "#f4a261")) + ggtitle("Zone Cases Vs. Deaths") + 
  scale_y_continuous(labels = comma) + ylab("Count") + xlab("Zone")

# Percentage of total deaths from COVID
mean(covid_alberta$Status== "Died")*100

###################
# Data Preparation
##################
# Converting the Type, Status & Gender variables to factors to be able to 
# feed them into the models.
covid_alberta$Type<-  factor(covid_alberta$Type,
                             levels = c('Confirmed', 'Probable'),
                             labels = c(1, 0))

covid_alberta$Status<-  factor(covid_alberta$Status,
                               levels = c('Recovered','Died'),
                               labels = c(0, 1))

covid_alberta$Gender<- factor(covid_alberta$Gender,
                              levels = c('Male', 'Female'),
                              labels = c(1, 0))

covid_alberta$Age_group<- factor(covid_alberta$Age_group, 
                                 levels = c('Under 1 year','1-4 years','5-9 years',
                                            '10-19 years','20-29 years',
                                            '30-39 years','40-49 years','50-59 years','60-69 years',
                                            '70-79 years','80+ years'),
                                 labels = c(0,1,2,3,4,5,6,7,8,9,10))

# Converting Zones by dummyVars
covid_alberta$Zone<- factor(covid_alberta$Zone, exclude = NULL)
covid_alberta$Zone<- addNA(covid_alberta$Zone)
dummy<- caret::dummyVars("~Zone", data= covid_alberta)
df<- data.frame(predict(dummy, newdata = covid_alberta))
covid_alberta<- cbind(covid_alberta, df)

# Removing the Zone Column as replaced by the dummyVars outpput & Remove Date & Number
covid_alberta<- covid_alberta %>% select(c(-Zone, -Date, -Number))
as_tibble(head(covid_alberta))

# Creating the train & test sets
set.seed(123, sample.kind = "Rounding")
ind<- createDataPartition(covid_alberta$Status, times= 1, p=0.7, list= FALSE)
train<- covid_alberta[ind,]
test<- covid_alberta[-ind,]

# Creating the Over-Sampling & Under-Sampling balanced data
set.seed(123, sample.kind = "Rounding")
over<- ovun.sample(Status ~ ., data=train, method = "over", N= 412762)$data
table(over$Status)

set.seed(123, sample.kind = "Rounding")
under<- ovun.sample(Status~ ., data= train, method = "under", N= 4060)$data
table(under$Status)

###################
# Building Models
##################

# 1: Unbalanced glm model (Baseline model)
unbal_glm_fit<- train %>% glm(Status ~ ., data=., family = "binomial")
unbal_glm_pred<- ifelse(predict(unbal_glm_fit, newdata = test, type = "response")
                        >0.5, 1, 0)

Accuracy_results <- tibble(Method = "Unbalanced glm", Balanced_Accuracy = confusionMatrix(as.factor(unbal_glm_pred), 
                          test$Status, positive = "1")$byClass["Balanced Accuracy"]) 

confusionMatrix(as.factor(unbal_glm_pred), test$Status, positive = "1")

# Unbalanced data showing status "1" (Died) is Less than 1% of the total data
table(train$Status)


# 2: Balanced glm model using Over-Sampling
bal_glm_fit_over<- over %>% glm(Status ~ ., data=., family = "binomial")
bal_glm_pred_over<- ifelse(predict(bal_glm_fit_over, newdata = test, type = "response") >0.5, 1, 0)

Accuracy_results <- bind_rows(Accuracy_results, tibble(Method = "Balanced Over-sampling glm", 
                                     Balanced_Accuracy = confusionMatrix(as.factor(bal_glm_pred_over),
                                     test$Status, positive = "1")$byClass["Balanced Accuracy"])) 

confusionMatrix(as.factor(bal_glm_pred_over), test$Status, positive = "1")


# 3: Balanced glm model using Under-Sampling
bal_glm_fit_under<- under %>% glm(Status ~ ., data=., family = "binomial")
bal_glm_pred_under<- ifelse(predict(bal_glm_fit_under, newdata = test, type = "response") >0.5, 1, 0)

Accuracy_results <- bind_rows(Accuracy_results, tibble(Method = "Balanced Under-Sampling glm",
                                     Balanced_Accuracy = confusionMatrix(as.factor(bal_glm_pred_under), 
                                     test$Status, positive = "1")$byClass["Balanced Accuracy"])) 

confusionMatrix(as.factor(bal_glm_pred_under), test$Status, positive = "1")

# 4: Decision Tree Balanced using Over-Sampling
bal_ctree_over<- over %>% ctree(Status ~ ., data=.)
bal_ctree_over_pred<- predict(bal_ctree_over, test)

Accuracy_results <- bind_rows(Accuracy_results, tibble(Method = "Balanced Over-Sampling Ctree", 
                                     Balanced_Accuracy = confusionMatrix(bal_ctree_over_pred, 
                                     test$Status, positive = "1")$byClass["Balanced Accuracy"])) 

confusionMatrix(bal_ctree_over_pred, test$Status, positive = "1")

# 5: Random Forest using Over-Sampling
rf<- over %>% ranger(Status ~ ., data=., mtry = 8, num.trees = 200)
rf_pred<- predict(rf, test)

Accuracy_results <- bind_rows(Accuracy_results, tibble(Method = "Balanced Over-Sampling Random Forest",
                                     Balanced_Accuracy = confusionMatrix(rf_pred$predictions, 
                                     test$Status, positive = "1")$byClass["Balanced Accuracy"]))

confusionMatrix(rf_pred$predictions, test$Status, positive = "1")

################
# Final Results
###############

# Table of Balanced Accuracy of all models arranged from highest to lowest
Accuracy_results %>% arrange(desc(Balanced_Accuracy)) %>% knitr::kable()
