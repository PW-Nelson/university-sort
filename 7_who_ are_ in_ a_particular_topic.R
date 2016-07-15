# find the universities within one research topic

rm(list=ls())
source("Proj2Funs.R")

# install and load useful libraries
lapply(c("RTextTools","topicmodels","tm","SnowballC","topicmodels","wordcloud"),checkpackages)


load("SSCfull.Rdata")
load("myCTM.Rdata")
load("TopicNames.Rdata")

# the complete affiliation list
universities = unlist(UList)
Universities = unique(universities)

# find the 6 hottest topics
hotopic = apply(CTMp,2,sum)
ind     = sort(hotopic, decreasing = T,index.return = T)$ix
MyTopic = ind[1:6]

par(mfrow = c(1,2))
for (topicn in MyTopic[5:6]){
  # find number of abstracts of each university
  Probs = rep(0,length(Universities))
  for (i in 1:length(UList)){
    for (j in length(UList[[i]])){
      k = which(Universities==UList[[i]][[j]])
      if (length(k)==1){
        Probs[k] = Probs[k]+CTMp[i,topicn]
        }else{
          print("error")
        }    
    } 
    }
  # find 10 most active universities
  ind = sort(Probs, decreasing = T,index.return = T)$ix
  ind = ind[1:10]
  # make the plots
  df.bar = barplot(Probs[ind], col = "lightblue", main = paste("10 active universities doing",namelist[topicn]),
        ylab = "Number of abstracts")
  text(x = df.bar, par("usr")[3] - 0.2, labels = Universities[ind], srt = 45, 
     pos = 2, xpd = TRUE)
}