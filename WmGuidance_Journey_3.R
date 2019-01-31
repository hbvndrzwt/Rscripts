#This R script should be an indication on how to procede with analytics after experiments
#This script is specific for the Journey_3 experiment of the WmGuidance
# ctrl + shift + c for multiple line commenting :)

## MISSING : 
# 1. Output Charts/graph (of descriptives as well as of the models)
# 2. Two-factor (Factorial Anova)
# 3. Figure out why experimentgroup is not significant in an Factorial ANOVA, but it is in a one-way ANOVA

#Install packages if necessary
install.packages("compute.es"); 
install.packages("car"); 
install.packages ("ggplot2");
install.packages("multcomp");
install.packages("pastecs");
install.packages("WRS");
install.packages("reshape");
install.packages("lazyeval");
install.packages("ggpubr");
install.packages("mlogit");

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

#Set working directory for output files
setwd("C:\\Users\\hzw\\Dropbox (Mendix)\\Community\\Onboarding\\_Analytics\\R Scripts\\Experiment Analysis\\Output")

#Define DataFrame and Metrics
Output <- journey_3_Output
colnames(Output)[which(names(Output) == "LectureCompleted")] <- "Metric1"
colnames(Output)[which(names(Output) == "WmActivated")] <- "Metric2"
colnames(Output)[which(names(Output) == "GuidanceFinished")] <- "Metric3"
colnames(Output)[which(names(Output) == "Activated")] <- "Metric4"
colnames(Output)[which(names(Output) == "Retained")] <- "Metric5"
colnames(Output)[which(names(Output) == "CompanyType")] <- "Grouping1"
colnames(Output)[which(names(Output) == "CustomerType")] <- "Grouping2"

#Disregard null values in dataframe
Output[Output == "NULL"] <- NA

#Set the independent variables as factors
Output$Grouping1 <- factor(Output$Grouping1, levels = c("Other", "Customer", "Partner", "University"))
Output$Grouping2 <- factor(Output$Grouping2, levels = c("Low-code", "No-code", "Project management"))
Output$ExperimentGroup <- as.factor(Output$ExperimentGroup)

#Setting contrasts for the independent variables - IF NECESSARY
#contrasts(Output$Grouping1) <- cbind(c(-3, 1, 1, 1), c(0, -2, 1, 1), c(0, 0, -1, 1))
#contrasts(Output$Grouping2) <- cbind(c(-2, 1, 1), c(0, -1, 1))
#contrasts(Output$ExperimentGroup) <- c(-1, 1)

#First get some Descriptive statistics per metric
by(Output$Metric1, Output$Grouping1, stat.desc)
by(Output$Metric2, Output$Grouping1, stat.desc)
by(Output$Metric3, Output$Grouping1, stat.desc)
by(Output$Metric1, Output$Grouping2, stat.desc)
by(Output$Metric2, Output$Grouping2, stat.desc)
by(Output$Metric3, Output$Grouping2, stat.desc)
by(Output$Metric3, Output$ExperimentGroup, stat.desc)


#Creating Subsets
Other <- subset(Output, Grouping1 == "Other")
Customer <- subset(Output, Grouping1 == "Customer")
Partner <- subset(Output, Grouping1 == "Partner")
University <- subset(Output, Grouping1 == "University")

NoCode <- subset(Output, Grouping2 == "No-code")
LowCode <- subset(Output, Grouping2 == "Low-code")
ProjectManagement <- subset(Output, Grouping2 == "Project management")


#Creating graphs for Report and Identify metrics/groups to focus on
ggline(Output, x = "ExperimentGroup", y = "Metric5", color = "ExperimentGroup", plot_type = "l", add = c("mean_ci"),  palette = c("#00AFBB", "#E7B800"), 
       xlab = "", ylab = "WmActivated")+facet_grid(.~Grouping2)


## ONE-FACTOR ANALYSIS

#LeveneTest to test whether the variances in the Outcome varies across groups
leveneTest(Output$Metric1, Output$Grouping1, center = mean)
leveneTest(Output$Metric2, Output$Grouping1, center = mean)
leveneTest(Output$Metric3, Output$Grouping1, center = mean)
leveneTest(Output$Metric1, Output$Grouping2, center = mean)
leveneTest(Output$Metric2, Output$Grouping2, center = mean)
leveneTest(Output$Metric3, Output$Grouping2, center = mean)

#Create a Linear Model: Outcome = Group + error
LinearModel1 <- glm(Metric1 ~ Grouping1, data = Output, family = binomial())
LinearModel2 <- glm(Metric1 ~ Grouping2, data = Output, family = binomial())

#Create an ANOVA Model 
AnovaModel1 <- aov(Metric2 ~ ExperimentGroup, data = Customer)


#Summary of Models
summary(AnovaModel1)
summary.glm(LinearModel2)

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



