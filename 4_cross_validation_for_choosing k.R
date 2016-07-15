# cross validation for choosing k

rm(list=ls())
source("Proj2Funs.R")

# install and load useful libraries
lapply(c("RTextTools","topicmodels","tm","SnowballC","topicmodels","wordcloud"),checkpackages)
load("SSCfull.Rdata")

# cross validation
K  = c(3,5,8,10,12,15,18,20,25,30,35,40,50,60)
PP = rep(NA,length(K))

for (j in 1:length(K)){
  PP[j] = CrossValid(dtm2,K[j])
  print(K[j])
}

save(K,PP, file ="CrossValidation.Rdata")

# make some plots
par(mfrow = c(1,1))
plot(k,p,,main="10-Fold cross validation results",xlab="Number of topic groups k",
     ylab="Total perplexity",type = "b",col=1,lwd=3)