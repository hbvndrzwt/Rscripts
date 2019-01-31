MemberSignupRank <- function(events){
  
  require(dplyr)
  require(data.table)
  
  #only select signup completed events
  events <- events[which(events$EventType.Name == "MemberSignupCompleted")]
  #drop NAs in the signup completed date
  events <- events[complete.cases(events[,"Member.Company.CompanyId"])]
  
  CompanyIds <- unique(events$Member.Company.CompanyId) #unique company IDs
  result.list <- list() # list to store results in
  result.vector <- 1 #vector to store results in
  
  #start loop per company (parent loop)
  for(i in 1:length(CompanyIds)){
    
    #grab the events per company
    comp_events <- events[which(events$Member.Company.CompanyId == CompanyIds[i]),]
    
    #grab the unique member Ids per company
    OpenIds <- unique(comp_events$Member.OpenId)
    
    #start the nested loop per user of the company selected in the parent loop
    for(x in 1: length(OpenIds)){
      
      result.vector[x] <- as.numeric(difftime(comp_events[which(comp_events$Member.OpenId == OpenIds[x]),]$Member.SignupDate, 
                                              min(comp_events$Member.SignupDate),
                                              unit = "days"))
      
    }
    # for each company, create a data table to store MemberOpenId,Member.Signupdate, CompanyId, and result.vector
    result.data.table <- data.table(OpenIds,comp_events$Member.SignupDate,comp_events$Member.Company.CompanyId, result.vector)
    #store the data table in a list (in order for the loop to function)
    result.list[[i]] <- result.data.table
    
    #reset the value of result.vector to 1 for the next iteration of the loop
    result.vector <- 1
    
  }
  #turn the result.list into a data table
  result <- rbindlist(result.list)
  
  #rename the columns of the data table
  names(result) <- c("Member.OpenId", "Member.SignupDate","CompanyId", "Days.AfterFirstCompanySignup")
  
  #add the signup rank of a user (= the order in which they signed up)
  result <- result %>% group_by(CompanyId) %>%
    mutate(SignupRank = rank(Member.SignupDate)) %>%
    arrange(Member.SignupDate)
  
  #round the Days.AfterFirstCompanySignup 
  result$Days.AfterFirstCompanySignup <- round(result$Days.AfterFirstCompanySignup, digits = 0)
  
  #return the resulting data table
  return(result)
}


