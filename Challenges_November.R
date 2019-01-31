# install the required package
#install.packages("odbc")
#install.packages("data.table")
#install.packages("DBI")
#install.packages("rstudioapi")
#install.packages("plyr")

# Other required libraries
library(odbc)
library(data.table)
library(DBI)
library(rstudioapi)
library(dplyr, warn.conflicts = FALSE)
library(plyr)

#Load the functions that are needed in this script
source("C:\\Users\\hzw\\Dropbox (Mendix)\\Community\\Onboarding\\_Analytics\\R Scripts\\Datalake_Connect_Schema.R")
source("C:\\Users\\hzw\\Dropbox (Mendix)\\Community\\Onboarding\\_Analytics\\R Scripts\\Functions.R")

# Make sure to load the right dataset from Datalake_Connect_Schema!
Dataset <- Datalake_connect(schema = "community", table = "Challenges_Experiment_Data", password = "RuGMXHZHPAaLUR5N")

#Set datetime variables to use in filtering out datapoints
nov1 <- strptime("01/11/2018 00:00:00", "%d/%m/%Y %H:%M:%S")
dec1 <- strptime("01/12/2018 00:00:00", "%d/%m/%Y %H:%M:%S")

#Filtering out the Onboarding Challenge 
OnboardingID <- unique(Dataset$OpenId[grep("NjqEv5zp4mO3NhV0dpIzMpmDIVro8WTY", Dataset$ChallengeId)])
d_without_onb <- Dataset[!Dataset$OpenId %in% OnboardingID,]

#Create an vector with unique OpenIDs that had a Challenge Assigned before 1st Nov and filter those out of the dataset, also filter out all Challenges assigned after 1dec
dumpID <- Dataset$OpenId[Dataset$TimeChallengeAssigned < nov1]
dumpID <- unique(dumpID)
d_firstChallenge <- d_without_onb[!d_without_onb$OpenId %in% dumpID,]
d_firstChallenge <- d_firstChallenge[d_firstChallenge$TimeChallengeAssigned < dec1,]

#Filter out user-challenges that are still active
d_NotActive <- d_firstChallenge[d_firstChallenge$Status != "Currently Active",]
d <- d_NotActive

#create a list of all challenges and metrics
challenges <- na.omit(unique(d$ChallengeId))
metrics <- names(d)[10:14]

#Generate table with results for all metrics for all challenges
x <- Challenges_SignificanceTest(challenges)


#############################################################################################################################


#Generate dataset for two-group testing for that Challenge
Challenge_Name <- Challenge_Test("Fill with ChallengeID")

#Get the Pvalue for Two-group comparison (Two-sided) from the dataset that is generated in the step before
pvalue_Learn <- TwoGroup_LevelTest(Challenge_Name$Metric[Challenge_Name$Group == 'A'], Challenge_Name$Metric[Challenge_Name$Group == 'B'])

mean(na.omit(Challenge_Name$Metric[Challenge_Name$Group == 'A']))
mean(na.omit(Challenge_Name$Metric[Challenge_Name$Group == 'B']))



