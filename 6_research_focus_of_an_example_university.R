# find the research topic of any university

rm(list=ls())
source("Proj2Funs.R")

# install and load useful libraries
lapply(c("RTextTools","topicmodels","tm","SnowballC","topicmodels","wordcloud"),checkpackages)

load("myCTM.Rdata")
load("TopicNames.Rdata")

# find 10 most active universities
load("SSCfull.Rdata")
universities = unlist(UList)
a = table(universities)
b = as.matrix(a)
UniversityNames = rownames(b)
UniversitySort  = sort.int(b, decreasing = TRUE,index.return=TRUE)

# make  plots
for (i in 1:10){
  par(mfrow = c(1,1))
  Uname = UniversityNames[UniversitySort$ix[i]]
  print(Uname)
  # get the probability list of "Uname" being in all groups
  Problist1 = Problist(CTMp,UList,Uname)
  # arrange according to year
  UData = matrix(NA,nrow=30,ncol=5)
  k = which(Problist1$Index < Ylist[1]+1 & Problist1$Index>0)
  UData[,1] = colSums(Problist1$Prob[k,])
  for (i in 2:5){
    k = which(Problist1$Index < sum(Ylist[1:i])+1 & Problist1$Index>sum(Ylist[1:(i-1)]))
    UData[,i] = colSums(Problist1$Prob[k,])
  }
  # select top 6 topics
  udata = apply(UData,1,sum)
  ind   = sort(udata, decreasing = T,index.return = T)$ix
  ind   = ind[1:6]
  # make plots
  counts = matrix(NA,ncol=5,nrow=6)
  for (i in 1:5) counts[,i] = UData[ind,(6-i)]
  counts = t(counts)
  rownames(counts) = c("SSC2011", "SSC2012","SSC2013","SSC2014","SSC2015")
  counts = as.table(counts)

  df.bar = barplot(counts, main=paste("Top 6 research topics at",Uname),
        xlab="topics", ylab="Number of abstracts",col=1+1:5) 
  text(x = df.bar, par("usr")[3] - 0.2, labels = namelist[ind], srt = 45, 
     pos = 2, xpd = TRUE)
  legend("topright", legend=c("SSC2011", "SSC2012","SSC2013","SSC2014","SSC2015"),
       col=1+1:5, lty=1,cex=0.8)
}