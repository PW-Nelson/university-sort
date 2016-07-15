# find the change of research interests at the SSC

rm(list=ls())
source("Proj2Funs.R")

# install and load useful libraries
lapply(c("RTextTools","topicmodels","tm","SnowballC","topicmodels","wordcloud"),checkpackages)

# Ylist = rep(NA,5)
# load("SSC2015.Rdata")
# Ylist[1] = length(Alist)
# load("SSC2014.Rdata")
# Ylist[2] = length(Alist)
# load("SSC2013.Rdata")
# Ylist[3] = length(Alist)
# load("SSC2012.Rdata")
# Ylist[4] = length(Alist)
# load("SSC2011.Rdata")
# Ylist[5] = length(Alist)

load("SSCfull.Rdata")
k0    = 30
myCTM = CTM(dtm2, k = k0, control = list(var = list(tol = 10^-4),
                                         em = list(tol = 10^-3)))
CTMp  = attributes(myCTM)$gamma

save(Ylist,UList,AList,myCTM,CTMp,file ="myCTM.Rdata")


############# print all the research topics
for (i in 1:k0){
  print(paste("Topic",i,":"))
  print(terms(myCTM, 10)[,i])
}
########### gather the research topic info of each year (all)
YearData     = matrix(NA,nrow=k0,ncol=5)
YearData[,1] = colSums(CTMp[1:Ylist[1],])
for (i in 2:5){
  YearData[,i] = colSums(CTMp[(sum(Ylist[1:(i-1)])+1):(sum(Ylist[1:(i-1)])+Ylist[i]),])
}
# name the topics
namelist = c("Bayesian Methods","Medical Statistics","High-Dimensional Statistics","Demographics",
             "Statistical Physics","Spatial Analysis","Risk Management","Experimental Design",
             "Genomics","Sampling Theory", "Machine Learning","Statistical Computing",
             "Epidemiology","Survey Methodology","Applied Longitudinal Data Analysis", "Longevity Risk",
             "Actuarial Science","Environmental Statistics","Functional Data Analysis","Econometrics",
             "Genetics","Statistical Learning","Survival and Event History Analysis","Simulation",
             "Population Size Estimation","Insurance Solvency Assessment", "Goodness-of-Fit Testing","Pension Plans",
             "Stochastic Modelling","Model Diagnostics/Selection")
save(namelist,file ="TopicNames.Rdata")

## plot the research topics of each year
df.bar = barplot(YearData[,1], ylim=c(0,max(YearData)*1.05),las = 2, names.arg = "",
        col = "lightgrey", main = "Research topics at SSC from 2011-2015 (all)", 
        ylab = "Number of abstracts",xlab = "topics")
for (i in 1:5)
lines(x = df.bar,y=YearData[,i],col=i,type = "b",lwd=3)
axis(1, at=seq(1, 10, by=1), labels = FALSE)
text(x = df.bar, par("usr")[3] - 0.2, labels = namelist, srt = 45,pos = 2, xpd = TRUE)

legend("topright", legend=c("SSC2015", "SSC2014","SSC2013","SSC2012","SSC2011"),col=1:5, lty=1,cex=0.8)


########### gather the research topic info of each year: top 10 topics
YData = apply(YearData,1,sum)
ind   = sort(YData, decreasing = T,index.return = T)$ix
ind   = ind[1:10]
YearData = matrix(NA,nrow=10,ncol=5)
YearData[,1] = colSums(CTMp[1:Ylist[1],ind])
for (i in 2:5){
  YearData[,i] = colSums(CTMp[(sum(Ylist[1:(i-1)])+1):(sum(Ylist[1:(i-1)])+Ylist[i]),ind])
}
df.bar = barplot(YearData[,1], ylim=c(0,max(YearData)*1.05),las = 2, names.arg = "",
                 col = "lightgrey", main = "Research topics at SSC from 2011-2015 (selected)", 
                 ylab = "Number of abstracts",xlab = "topics")
for (i in 1:5)
  lines(x = df.bar,y=YearData[,i],col=i,type = "b",lwd=3)
axis(1, at=seq(1, 10, by=1), labels = FALSE)
text(x = df.bar, par("usr")[3] - 0.2, labels = namelist[ind], srt = 45, pos = 2, xpd = TRUE)

legend("topright", legend=c("SSC2015", "SSC2014","SSC2013","SSC2012","SSC2011"),col=1:5, lty=1,cex=0.8)