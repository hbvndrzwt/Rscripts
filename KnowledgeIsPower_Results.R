

install.packages ("ggplot2");
install.packages("ggpubr");
install.packages("pastecs");
install.packages("plotly");

library(ggplot2)
library(ggpubr)
library(pastecs)
library(readxl)
library(plotly)

Challenge_KnowledgePower_Output_3_1_2019 <- read_excel("C:/Users/hzw/Dropbox (Mendix)/Community/Onboarding/_Analytics/Experiments/Challenges/Challenge_KnowledgePower_Output__3-1-2019.xlsx", 
                                                       col_types = c("text", "skip", "skip", "date", "text", "text", "numeric", "numeric", "numeric", "numeric"))

dt <- Challenge_KnowledgePower_Output_3_1_2019
var <- Challenge_KnowledgePower_Output_3_1_2019$DiffLearnActivity


#Get a table where the users with 0 activity before AND 0 activity after are removed
KnowledgePower_WithActivity <- dt[dt$DiffLearnActivity !=0,]
KnowledgePower_WithActivity_CreditClaimed <- KnowledgePower_WithActivity[KnowledgePower_WithActivity$CountCreditClaimed > 0, ]

boxplot(KnowledgePower_WithActivity$DiffLearnActivity, main = "With Outliers, Diff != 0", xlab = NA)
hist(KnowledgePower_WithActivity$DiffLearnActivity, main = "With Outliers, Diff != 0", xlab = "Diff Learn Activity")
boxplot(KnowledgePower_WithActivity_CreditClaimed$DiffLearnActivity, main = "With Outliers, Diff != 0, Credit Claimed", xlab = NA)
hist(KnowledgePower_WithActivity_CreditClaimed$DiffLearnActivity, main = "With Outliers, Diff != 0, Credit Claimed", xlab = "Diff Learn Activity")

mean_Total <- mean(KnowledgePower_WithActivity$DiffLearnActivity)
mean_CreditClaimed <- mean(KnowledgePower_WithActivity_CreditClaimed$DiffLearnActivity)

summary(KnowledgePower_WithActivity_CreditClaimed$DiffLearnActivity)
summary(dt_Total$var)




#Set DataFrame and Variable for Outlier Checking
dt <- KnowledgePower_WithActivity
var<- KnowledgePower_WithActivity$DiffLearnActivity

#Checking (and removing) of outliers
var_name <- eval(substitute(var),eval(dt))
na1 <- sum(is.na(var_name))
m1 <- mean(var_name, na.rm = T) #Mean of variable (with outliers)
par(mfrow=c(2, 2), oma=c(0,0,3,0)) 
boxplot(var_name, main="With outliers")
hist(var_name, main="With outliers", xlab=NA, ylab=NA)
outlier <- boxplot.stats(var_name)$out
mo <- mean(outlier) #Get the mean of all the outliers
var_name <- ifelse(var_name %in% outlier, NA, var_name)
boxplot(var_name, main="Without outliers")
hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
title("Outlier Check", outer=TRUE)
na2 <- sum(is.na(var_name))
cat("Outliers identified:", na2 - na1, "n")
cat("Propotion (%) of outliers:", round((na2 - na1) / sum(!is.na(var_name))*100, 1), "n")
cat("Mean of the outliers:", round(mo, 2), "n")
m2 <- mean(var_name, na.rm = T) #Mean without the outliers
cat("Mean without removing outliers:", round(m1, 2), "n")
cat("Mean if we remove outliers:", round(m2, 2), "n")
response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
if(response == "y" | response == "yes"){
  dt[as.character(substitute(var))] <- invisible(var_name)
  assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
  cat("Outliers successfully removed", "n")
  return(invisible(dt))
} else{
  cat("Nothing changed", "n")
  return(invisible(var_name))
}


boxplot(dt_Total$var, main = "Without Outliers", xlab = NA)
hist(dt_Total$var, main = "Without Outliers", xlab = "Diff Learn Activity")
boxplot(dt_CreditClaimed$var, main = "Without Outliers, CreditClaimed", xlab = NA)
hist(dt_CreditClaimed$var, main = "Without Outliers, CreditClaimed", xlab = "Diff Learn Activity")

#Creating graphs for Report and Identify metrics/groups to focus on
ggline(dt_Total, x = "CountCreditClaimed", y = "var", plot_type = "p", palette = c("#00AFBB", "#E7B800"), 
       xlab = "CountCreditsClaimed", ylab = "DiffLearn")
