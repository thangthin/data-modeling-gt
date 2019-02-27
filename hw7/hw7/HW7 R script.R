rm(list = ls())
set.seed(1)
dat <- read.table('uscrime.txt', stringsAsFactors=FALSE, header=TRUE)
library(rpart)

head(dat)

formula <- Crime~M + So + Ed + Po1 + Po2 + LF + M.F + Pop + NW + U1 + U2 + Wealth + Ineq + Prob + Time
fit <- rpart(formula, method="anova", data=dat)


printcp(fit) # display the results
plotcp(fit) # visualize cross-validation results
summary(fit) # detailed summary of splits

fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"]
# # create additional plots
par(mfrow=c(1,2)) # two plots on one page
rsq.rpart(fit) # visualize cross-validation results

# # plot tree
plot(fit, uniform=TRUE,
    main="Regression Tree for Mileage ")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

