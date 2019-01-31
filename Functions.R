#Script with main functions

Challenge_Test <- function(ChallengeId) {
  #Create 2 unique lists of OpenIDs, based on if they received the Challenge or not  
  OpenId_Challenge <- unique(d$OpenId[d$ChallengeId == ChallengeId])
  OpenId_Rest <- unique(d$OpenId[!d$OpenId %in% OpenId_Challenge])
  
  Group_A <- d[d$OpenId %in% OpenId_Challenge,]
  Group_A_Unique <- Group_A[!duplicated(Group_A$OpenId),]
  Group_A_Unique$Group <- "A"
  Group_B <- d[d$OpenId %in% OpenId_Rest,]
  Group_B_Unique <- Group_B[!duplicated(Group_B$OpenId),]
  Group_B_Unique$Group <- "B"
  
  Total <- rbind(Group_A_Unique, Group_B_Unique)
  Total <- subset(Total, select = -c(ChallengeId, Status, LastChallengeEventChangedId))
  
  return(Total)
}

##################################################################################################################


TwoGroup_LevelTest <- function(GroupA, GroupB){
  nNAA=sum(is.na(GroupA))
  nNAB=sum(is.na(GroupB))
  if(nNAA>0 | nNAB >0){
    warning("There are ",nNAA," NAs in GroupA")
    warning("There are ",nNAB," NAs in GroupB")
    GroupA <- na.omit(GroupA)
    GroupB <- na.omit(GroupB)
  }

  #Here would come the correct significance test
  meanA <- mean(GroupA)
  meanB <- mean(GroupB)
  
  s <- sqrt((sum((GroupA - meanA)^2) + sum((GroupB - meanB)^2))/
    (length(GroupA) + length(GroupB) - 2) *
    ((1/length(GroupA)) + (1/length(GroupB))))
  
  zscore <- abs(meanA - meanB)/s
  
  pvalue <- (1- pnorm(zscore))*2
  
  return(pvalue)
  
}


###############################################################################################################



Challenges_SignificanceTest <- function(Challenges){
  
  p_values <- data.frame()
  
  for(i in Challenges){
    
    TestDataset <- Challenge_Test(i)
    
    #Fix the loop, so that we don't have to change the column numbers manually.
    for(n in 7:11){
      
      GroupA <- TestDataset[TestDataset$Group == 'A', n]
      GroupB <- TestDataset[TestDataset$Group == 'B', n]
      
      nNAA=sum(is.na(GroupA))
      nNAB=sum(is.na(GroupB))
      if(nNAA>0 | nNAB >0){
        warning("There are ",nNAA," NAs in GroupA")
        warning("There are ",nNAB," NAs in GroupB")
        GroupA <- na.omit(GroupA)
        GroupB <- na.omit(GroupB)
      }

      meanA <- mean(GroupA)
      meanB <- mean(GroupB)
      
      s <- sqrt((sum((GroupA - meanA)^2) + sum((GroupB - meanB)^2))/
                  (length(GroupA) + length(GroupB) - 2) *
                  ((1/length(GroupA)) + (1/length(GroupB))))
      
      zscore <- abs(meanA - meanB)/s
      pvalue <- (1- pnorm(zscore))*2
      
      New_row <- data.frame(Challenge = i, Metric = colnames(TestDataset)[n], meanA = meanA, meanB = meanB, pvalue = pvalue, z_score = zscore)
      p_values <- rbind(p_values, New_row)
      
    }
  }
  return(p_values)
}  


