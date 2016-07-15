# model setup

rm(list=ls())
source("Proj2Funs.R")

# install and load useful libraries
lapply(c("RTextTools","topicmodels","tm","SnowballC","topicmodels","wordcloud"),checkpackages)

# load the data
AList = NULL
UList = NULL
Ylist = rep(NA,5)
load("SSC2015.Rdata")
AList = append(AList,Alist)
UList = append(UList,Ulist)
Ylist[1] = length(Alist)
load("SSC2014.Rdata")
AList = append(AList,Alist)
UList = append(UList,Ulist)
Ylist[2] = length(Alist)
load("SSC2013.Rdata")
AList = append(AList,Alist)
UList = append(UList,Ulist)
Ylist[3] = length(Alist)
load("SSC2012.Rdata")
AList = append(AList,Alist)
UList = append(UList,Ulist)
Ylist[4] = length(Alist)
load("SSC2011.Rdata")
AList = append(AList,Alist)
UList = append(UList,Ulist)
Ylist[5] = length(Alist)

# create a text database
(MyCorpus = Corpus(VectorSource(AList)))

# make the text semi-readable
MyCorpus = tm_map(MyCorpus, content_transformer(tolower))
# deal with equivalent words
MyCorpus = ContentTrans(MyCorpus)
MyCorpus = tm_map(MyCorpus, removeWords, stopwords("english"))
MyCorpus = tm_map(MyCorpus, removeWords, stopwords("french"))
MyCorpus = tm_map(MyCorpus, removePunctuation, preserve_intra_word_dashes = TRUE)
MyCorpus = tm_map(MyCorpus, removeNumbers)
MyCorpus = tm_map(MyCorpus, stripWhitespace)
# stemming
myCorpusStemmed = tm_map(MyCorpus,stemDocument,language=c("french","english"))

# word counting
(dtm = DocumentTermMatrix(myCorpusStemmed))
# # some inspection
# inspect(dtm[1:5, 5160:5165])
# findFreqTerms(dtm, 100)
# findAssocs(dtm, "regress", 0.4)

# trim out all elements with more than 98% sparsity
(dtm2 = removeSparseTerms(dtm, 0.98))
# # some inspection
# colnames(dtm2)[1:10]

# Wordcloud time
par(mfrow = c(1,1))
v = sort(colSums(as.matrix(dtm2)), decreasing = TRUE)
SSCWords = names(v)
d = data.frame(word = SSCWords, freq = v)
wordcloud(d$word, colors = rainbow(4), random.color = T,d$freq, min.freq = 50)

# barchart visual
summed = matrix(colSums(as.matrix(dtm2)), ncol = 1)
rownames(summed) = colnames(dtm2)
summedsorted = summed[sort(summed, decreasing = T,index.return = T)$ix, ]
d = data.frame(word = names(summedsorted), freq = summedsorted)
barplot(d$freq[1:50], las = 2, names.arg = d$word[1:50],
        col = "lightblue", main = "Top 50 most frequent words",
        ylab = "Word frequencies")

save(UList,AList,Ylist,dtm2, file ="SSCfull.Rdata")