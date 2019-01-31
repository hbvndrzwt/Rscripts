#This R script should be an indication on how to procede with analytics after experiments
#This script is specific for the Journey_4 experiment of the WmGuidance
# ctrl + shift + c for multiple line commenting :)

# Metrics:
# GuidanceFinished
# LectureCompleted
# WmActivated
# Activated
# Retained

# Groupings:
# CompanyType
# UserType
# ExperimentGroup
# GuidanceFinished
# BrokenEvents

## MISSING : 


#Install packages if necessary
# install.packages("compute.es"); 
# install.packages("car"); 
# install.packages ("ggplot2");
# install.packages("multcomp");
# install.packages("pastecs");
# install.packages("WRS");
# install.packages("reshape");
# install.packages("lazyeval");
# install.packages("ggpubr");
# install.packages("mlogit");

library(compute.es); 
library(car); 
library(ggplot2); 
library(multcomp); 
library(pastecs); 
library(WRS);
library(reshape);
library(lazyeval);
library(ggplot2);
library(ggpubr);
library(mlogit);
library(readxl)

#Set working directory for output files
setwd("C:\\Users\\hzw\\Dropbox (Mendix)\\Community\\Onboarding\\_Analytics\\R Scripts\\Experiment Analysis\\Output")

#Load Dataset
Journey_4_Output <- read_excel("C:/Users/hzw/Dropbox (Mendix)/Community/Onboarding/_Analytics/Experiment Output/Journey_4_Output.xlsx")
View(Journey_4_Output)
Output <- Journey_4_Output

#Disregard null values in dataframe
Output$UserType[Output$UserType == "NULL"] <- NA
Output$BrokenEvents[Output$BrokenEvents == "NULL"] <- 0
Output$UserType[Output$UserType == "Project management"] <- NA #This group is too small to take into consideration in this experiment

#Set the independent variables as factors
Output$CompanyType <- factor(Output$CompanyType, levels = c("Other", "Customer", "Partner", "University"))
Output$UserType <- factor(Output$UserType, levels = c("Low-code", "No-code", "Project management"))
Output$ExperimentGroup <- as.factor(Output$ExperimentGroup)

#Setting contrasts for the independent variables - IF NECESSARY
#contrasts(Output$Grouping1) <- cbind(c(-3, 1, 1, 1), c(0, -2, 1, 1), c(0, 0, -1, 1))
#contrasts(Output$Grouping2) <- cbind(c(-2, 1, 1), c(0, -1, 1))
#contrasts(Output$ExperimentGroup) <- c(-1, 1)


#Creating Subsets
Other <- subset(Output, CompanyType == "Other")
Customer <- subset(Output, CompanyType == "Customer")
Partner <- subset(Output, CompanyType == "Partner")
University <- subset(Output, CompanyType == "University")

NoCode <- subset(Output, UserType == "No-code")
LowCode <- subset(Output, UserType == "Low-code")
ProjectManagement <- subset(Output, UserType == "Project management")

A <- subset(Output, ExperimentGroup == "A")
B <- subset(Output, ExperimentGroup == "B")


#Set Metric & Grouping for below script
# colnames(Output)[which(names(Output) == "LectureCompleted")] <- "Metric"
# colnames(Output)[which(names(Output) == "Grouping")] <- "UserType"


#First get some Descriptive statistics per metric
by(Output$Metric, Output$Grouping, stat.desc)



#Creating graphs for Report and Identify metrics/groups to focus on
ggline(Journey_4_Output, x = "BrokenEvents", y = "LectureCompleted", color = "ExperimentGroup", plot_type = "l", add = c("mean_ci"),  palette = c("#00AFBB", "#E7B800"), 
       xlab = "", ylab = "LectureCompleted")#+facet_grid(.~BrokenEvents)



## ONE-FACTOR ANALYSIS

#Create a Linear Model: Outcome = Group + error
LinearModel1 <- glm(WmActivated ~ CompanyType, data = Output, family = binomial())

#Create an ANOVA Model 
AnovaModel <- aov(WmActivated ~ ExperimentGroup, data = Customer)
AnovaModel1 <- aov(WmActivated ~ ExperimentGroup, data = Other)
AnovaModel2 <- aov(WmActivated ~ ExperimentGroup, data = Partner)
AnovaModel3 <- aov(WmActivated ~ ExperimentGroup, data = University)


Output$GuidanceFinished <- as.factor(Output$GuidanceFinished)
Journey_4_Output$BrokenEvents <- as.factor(Journey_4_Output$BrokenEvents)
AnovaGroup <- aov(GuidanceFinished ~ BrokenEvents, data = Journey_4_Output)

#Summary of Models
summary(AnovaModel)
summary(AnovaModel1)
summary(AnovaModel2)
summary(AnovaModel3)
summary(AnovaGroup)

summary(LinearModel1)


out <- capture.output(summary(AnovaModel1))
cat("ANOVA Model LectureCompleted ~ ExperimentGroup (Other)", out, file = "testfile.txt", sep = "\t", append = TRUE)








#TWO-FACTOR ANOVA (Not sure if/how to set this up) 

#Get descriptive statistics for multiple independent variables on the dependent variable
by(Output$Metric3, list(Output$Grouping2, Output$ExperimentGroup), stat.desc)


#Define the Factorial Anova models: Set up the models we want to look at (can be more than 1 model)
FactAnovaModel1 <- aov(Metric3 ~ ExperimentGroup*Grouping1, data = Output, family = binomial())
FactAnovaModel2 <- aov(Metric1 ~ ExperimentGroup*Grouping1*Grouping2, data = Output)

#Define Linear Models: Set up the models we want to look at (can be more than 1 model)
GLMModel <- glm(Metric3 ~ ExperimentGroup*Grouping2, data = Output, family = binomial())

#Summary of the Models
summary(GLMModel)
Anova(FactAnovaModel1, type = "III")





## Additional - Archived
#post hoc analysis
pairwise.t.test(Output$Metric1, Output$Grouping1, p.adjust.method = "bonferroni")
postHocs <- glht(FactAnovaModel1, linfct = mcp(Grouping1 = "Tukey"))
summary(postHocs)
confint(postHocs)


#NOT WORKING
interaction.plot(x.factor = Output$Grouping1, trace.factor = Output$ExperimentGroup, response = Output$Metric1)

