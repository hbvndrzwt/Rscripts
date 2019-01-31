
install.packages ("ggplot2");
install.packages("ggpubr");
install.packages("pastecs");

library(readxl)
Datalake_TimeDiffConfirmationEmail <- read_excel("C:/Users/hzw/Dropbox (Mendix)/Community/Onboarding/_Analytics/SQL Queries/Output/Datalake_TimeDiffConfirmationEmail.xlsx")
View(Datalake_TimeDiffConfirmationEmail)

dt <- Datalake_TimeDiffConfirmationEmail
var <- Datalake_TimeDiffConfirmationEmail$TimeDiffSecond


which.outlier <- function(l,pvalue=0.001,underlying=c("normal","poisson","exponential","gamma"),recursive=FALSE,method=c("simple","del_res")){
  underlying <- match.arg(underlying)
  method <- match.arg(method)
  
  if(method=="del_res"){
    p <- numeric(1)
    out <- integer()
    if(all(is.na(l))) return(out)
    ## deleted residuals loop
    for(i in 1:length(l)){
      if(is.na(l[i])) next
      p <- switch(underlying,
                  normal = pnorm(l[i],mean(l[-i],na.rm = TRUE),sd(l[-i],na.rm = TRUE)),
                  poisson = ppois(l[i],mean(l[-i],na.rm = TRUE)),
                  exponential = pexp(l[i],1/mean(l[-i],na.rm = TRUE)),
                  gamma = {
                    m=mean(l[-i],na.rm = TRUE)
                    scale=var(l[-i],na.rm = TRUE)/m
                    shape=m/scale
                    pgamma(l[i],shape = shape,scale = scale)
                  }
      )
      if(underlying == "normal"){
        if(p<pvalue | 1-p < pvalue) out <- c(out,i)
      }else if(underlying %in% c("poisson","exponential","gamma")){
        if(1-p < pvalue) out <- c(out,i)
      }
    }
  }
  else if(method=="simple"){
    p <- switch(underlying,
                normal = pnorm(l,mean(l,na.rm = TRUE),sd(l,na.rm = TRUE)),
                poisson = ppois(l,mean(l,na.rm = TRUE)),
                exponential = pexp(l,1/mean(l,na.rm = TRUE)),
                gamma = {
                  m=mean(l,na.rm = TRUE)
                  scale=var(l,na.rm = TRUE)/m
                  shape=m/scale
                  pgamma(l,shape = shape,scale = scale)
                }
    )
    if(underlying == "normal"){
      out <- which(p<pvalue | 1-p < pvalue)
    }else if(underlying %in% c("poisson","exponential","gamma")){
      out <- which(1-p < pvalue)
    }
  }
  
  if(length(out)==0) recursive=FALSE
  if(recursive){
    l[out] <- NA
    return(c(out,which.outlier(l,pvalue,underlying,recursive,method)))
  }
  return(out)
}

which.outlier(dt$TimeDiffSecond, underlying = "gamma", pvalue = 0.01, method = "simple")
dt$TimeDiffSecond[which.outlier(dt$TimeDiffSecond, underlying = "gamma", pvalue = 0.01, method = "simple")]
x <- dt$TimeDiffSecond[-which.outlier(dt$TimeDiffSecond, underlying = "gamma", pvalue = 0.005, method = "simple")]


library(ggplot2)
library(ggpubr)
library(pastecs)

  
#Get descriptives of the data (with & without outliers)
summary(dt$TimeDiffSecond)
stat.desc(dt$TimeDiffSecond)

summary(x)
stat.desc(x)

q975 <- quantile(x, 0.975)
q975/3600

#Gamma density function without Outliers
scaleWithoutOutliers <- var(dt$var, na.rm = T)/meanWithoutOutliers
shapeWithoutOutliers <- meanWithoutOutliers/scaleWithoutOutliers


for( OptimalTime in c(1:700)){
  
  GammaDensity <- pgamma(q = OptimalTime, shape = shapeWithoutOutliers, scale = scaleWithoutOutliers)
  
  if(GammaDensity >= 0.99){
    break()
  }
  
}
print(GammaDensity)
print(OptimalTime)
  





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





