# trying four different modelling methods

rm(list=ls())
source("Proj2Funs.R")

# install and load useful libraries
lapply(c("RTextTools","topicmodels","tm","SnowballC","topicmodels","wordcloud"),checkpackages)

load("SSCfull.Rdata")

# topic modelling
k0 = 10
# Full LDA model estimating the group allocation
# using Variational Expectation Maximization (VEM)
myVEM = LDA(dtm2, k = k0)
# Holding the Dirichlet parameter fixed estimating
# the group allocation using Variational
# Expectation Maximization (VEM)
myVEM_fixed = LDA(dtm2, k = k0, control = list(estimate.alpha = FALSE))
# Use Gibbs Sampling to estimate all parameters
# rather than using the Variational EM:
myGibbs = LDA(dtm2, k = k0, method = "Gibbs", control = list(burnin = 1000,iter = 10000))
# correlated topics model extends LDA to allow
# correlations between topics and not just
# correlations between terms.
myCTM = CTM(dtm2, k = k0, control = list(var = list(tol = 10^-4),em = list(tol = 10^-3)))

# some inspection
# top 5 terms for each topic in LDA according to VEM
# (Terms = terms(myVEM, 5))
# (my_topics = topics(myVEM))[1:10]
# attributes(attributes(myVEM))
# attributes(myVEM)$alpha
# attributes(myVEM)$terms[694]
# dim(attributes(myVEM)$beta)
# rowSums(exp(attributes(myVEM)$beta))
# my_topics[28]
# sort((Terms = terms(myVEM, 30))[, my_topics[28]])
# topics(myGibbs)[28]
# topics(myVEM_fixed)[28]
# topics(myVEM)[28]
# topics(myCTM)[28]
# sort((Terms = terms(myGibbs, 30))[, topics(myGibbs)[28]])

# probability matrix
tempVEM       = attributes(myVEM)$gamma
tempVEM_fixed = attributes(myVEM_fixed)$gamma
tempGibbs     = attributes(myGibbs)$gamma
tempCTM       = attributes(myCTM)$gamma

save(tempVEM,tempVEM_fixed,tempGibbs,tempCTM,file ="VariousModels.Rdata")

# make some plots
par(mfrow = c(2,2))
hist(tempVEM,main = "Histogram of probabilities from VEM",xlim=c(0,1),xlab="entries in gamma")
hist(tempVEM_fixed,main = "Histogram of probabilities from VEM_fixed",xlim=c(0,1),xlab="entries in gamma")
hist(tempGibbs,main = "Histogram of probabilities from Gibbs",xlim=c(0,1),xlab="entries in gamma")
hist(tempCTM,main = "Histogram of probabilities from CTM",xlim=c(0,1),xlab="entries in gamma")