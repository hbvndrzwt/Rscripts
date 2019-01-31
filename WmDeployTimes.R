
success <- subset(FullDeployWM, FullDeployWM$status == "success")
failed <- subset(FullDeployWM, FullDeployWM$status == "failed")

par(mfrow=c(2, 2), oma=c(0,0,3,0)) 

boxplot(FullDeployWM$DeployDuration, main = "Boxplot All Deploys")
hist(FullDeployWM$DeployDuration, main = "Histogram All Deploys")
mean(FullDeployWM$DeployDuration)/60

boxplot(success$DeployDuration, main = "Boxplot Success Deploys")
hist(success$DeployDuration, main = "Histogram Success Deploys")
mean(success$DeployDuration)/60

boxplot(failed$DeployDuration, main = "Boxplot Failed Deploys")
hist(failed$DeployDuration, main = "Histogram Failed Deploys")
mean(failed$DeployDuration)/60