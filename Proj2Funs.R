# 1) check availability of packages and load them
# 2) data pre-processing: delete irrelavant information
# 3) define an additional content transformer to deal with equivalent words
# 4) find affiliation list of one line
# 5) get a list of abstract and a list of university names from pdftxt
# 6) get the indices of the abstracts involving some university "Uname"
# 7) get the probabilities for an institute belonging to all groups
# 8) cross validation for choosing number of groups k


#############################################################
#1 Checking the availability of packages using a nice function 
#############################################################
checkpackages=function(package){
  # Checking the Availability of packages using a nice function 
  # Installs them.  
  # example usage: checkpackages("gtools")
  if (!package %in% installed.packages()){
    install.packages(package)
  }
  library(package,character.only =T)
}

#############################################################
#2 Data pre-processing: delete irrelavant information
#############################################################
preprocess1 = function(Corpus){  
  myCorpus = Corpus
  
  # delete blank elements and page numbers
  lp = 1
  while (lp <= length(myCorpus)) {
    if (nchar(as.character(myCorpus[[lp]])) > 5) {
      lp = lp + 1
    }else{
      myCorpus = myCorpus[-lp]
    }
  }
  
  # delete session names
  lp = 1
  while (lp <= length(myCorpus)) {
    temp = unlist(strsplit(as.character(myCorpus[[lp]]),split=""))
    if (temp[1]=="\f" && temp[4]=="-") {
      myCorpus = myCorpus[-lp]
    }else{
      lp = lp + 1
    }
  }
  
  # delete Organizer and Chair
  lp = 1
  while (lp <= length(myCorpus)) {
    temp = unlist(strsplit(as.character(myCorpus[[lp]]),split=" "))
    if (temp[1]=="Organizer" && temp[2]=="and" && temp[3]=="Chair") {
      myCorpus = myCorpus[-lp]
    }else{
      lp = lp + 1
    }
  }
  
  # delete Abstract(s), Room, Chair, Organizer
  lp = 1
  while (lp <= length(myCorpus)) {
    temp = unlist(strsplit(as.character(myCorpus[[lp]]),split="/"))
    if (temp[1]=="Abstract" || temp[1]=="Abstracts" || temp[1]=="Room" || temp[1]=="Chair" || temp[1]=="Organizer") {
      myCorpus = myCorpus[-lp]
    }else{
      lp = lp + 1
    }
  }
  
  # delete session sponsorship
  lp = 1
  while (lp <= length(myCorpus)) {
    temp = unlist(strsplit(as.character(myCorpus[[lp]]),split=" "))
    if (temp[1]=="Session" && temp[2]=="sponsored" && temp[3]=="by") {
      myCorpus = myCorpus[-lp]
    }else{
      lp = lp + 1
    }
  }
  
  
  # delte single line with dates
  lp = 1
  while (lp <= length(myCorpus)) {
    temp = unlist(strsplit(as.character(myCorpus[[lp]]),split=""))
    if (temp[1]=='[') {
      myCorpus = myCorpus[-lp]
    }else{
      lp = lp + 1
    }
  }
  
  return(myCorpus)
}


#############################################################
#3 Define an additional content transformer to deal with equivalent words
#############################################################
ContentTrans = function(myCorpus){
  # deal with the abundance of nearly equivalent words
  
  # load the term transformation relations
  termtrans = read.csv("data_term_transforms.csv")
  
  # delete repeated rows using data frame df
  df = data.frame(termtrans$actuarial,termtrans$Actuarial,termtrans$Actuarial.1)
  df = df[!duplicated(df),]
  
  # obtain all the relations: "a"->"b"
  Termtrans = as.matrix(df)
  # the last two rows are null
  Termtrans = Termtrans[1:761,]
  # delete dupicated ones
  df1 = data.frame(c(Termtrans[,1],Termtrans[,2]),c(Termtrans[,2],Termtrans[,3]))
  df1 = df1[!duplicated(df1),]
  
  # trim the too-long inefficient list
  Termtrans = as.matrix(df1)
  DpSet = NULL
  for(i in 1:dim(Termtrans)[1]){
    # delete useless relation "a"->"a"
    if (Termtrans[i,1]==Termtrans[i,2]){
      DpSet = c(DpSet,i) 
    }else{ # append space to both start and end of a word
      Termtrans[i,1] = paste("",tolower(Termtrans[i,1]),"")
      Termtrans[i,2] = paste("",tolower(Termtrans[i,2]),"")
    }
    # change to word "delate" into ""
    if (Termtrans[i,2]==" delete "){
      Termtrans[i,2] = ""
    }
  }
  WordList = Termtrans[-DpSet,]
  WordList = rbind(WordList,c(" can ",""),c(" will ",""))
  
  # define a transformer
  f = content_transformer(function(x, y) gsub(pattern = y[1], replacement=y[2], x))
  
  # conduct transformation of equivalent words
  for( WordIndex in 1:dim(WordList)[1]){
    myCorpus = tm_map(myCorpus, f, WordList[WordIndex,])
  }
  
  return(myCorpus)
}


#############################################################
#4 Find university names of one line
#############################################################
subsR = function(y,textex){
  substr(x = textex, start = y[1] + 1, stop = y[2] - 1)
}

getUniversities = function(TextEx){
  
  # if there is no proper affiliation, return NULL
  Ulist = NULL
  
  # change the set of capital letters to admit some French author names
  LetterSet = c(LETTERS,"É")
  
  # obtain all the indices of brackets
  BracketStart = gregexpr("\\(", TextEx)
  BracketEnds  = gregexpr("\\)", TextEx)
  
  # if there exists any brackets
  if (BracketStart[[1]]!=-1 && BracketEnds[[1]]!=-1 && BracketStart[[1]][1]!=1){
    # characterize the string
    StrTemp = unlist(strsplit(TextEx,split=""))
    
    k = 1
    while (k <= length(BracketStart[[1]])){
      # there should be at least three letters infront of an affiliation
      if (BracketStart[[1]][k]<4){ 
        flag1 = FALSE
      }else{
        # proper start of an affiliation
        strtemp1 = StrTemp[(BracketStart[[1]][k]-3):(BracketStart[[1]][k]+1)]
        flag1    = all(strtemp1[1] %in% LetterSet,strtemp1[2] %in% LetterSet,strtemp1[3]==" ",strtemp1[4]=="(",strtemp1[5] %in% LetterSet)
      }
      
      flag2 = FALSE
      if (is.na(BracketEnds[[1]][k]) == FALSE){    
        if (BracketEnds[[1]][k] == length(StrTemp)){
          if (StrTemp[BracketEnds[[1]][k]-1] %in% c(letters,LetterSet)){
            # this should definitely avoid some reference in brackets
            flag2 = TRUE
          }
          # proper end of an affiliation
          }else{
            strtemp2 = StrTemp[BracketEnds[[1]][k]:(BracketEnds[[1]][k]+2)]
            flag2a   = all(strtemp2[3]==" ", strtemp2[2]==",",strtemp2[1]==")")
            flag2b   = all(strtemp2[3]=="[",strtemp2[2]==" ",strtemp2[1]==")")
            flag2    = (flag2a || flag2b)
          }
      }
      
      if (flag1 == FALSE || flag2 == FALSE ){
        BracketStart[[1]] = BracketStart[[1]][-k]
        BracketEnds[[1]]  = BracketEnds[[1]][-k]
      }else{
        k = k+1
      }     
    }
    
    # obtain the affiliation list
    if (length(BracketStart[[1]])>0){
      BracketIndex = cbind(BracketStart[[1]], BracketEnds[[1]])
      Ulist        = apply(BracketIndex, 1, subsR, textex = TextEx)
    }
  }
  
  return(Ulist) 
}


#############################################################
#5 Get a list of abstract and a list of university names
#############################################################
getUnA = function(MyCorpus){
  UList = list()
  AList = list()
  
  Index = 0
  for (lp in 1:length(MyCorpus)){
    # transverse all lines
    Prov =  getUniversities(as.character(MyCorpus[[lp]]))
     
    if (is.null(Prov)){
      # part of an abstract
      AList[[Index]] = paste0(AList[[Index]],MyCorpus[[lp]],collapse = NULL)
      }else{ # an author-type line
        Index = Index+1
        UList[[Index]] = list()
        UList[[Index]] = as.list(Prov)
        AList[[Index]] = ""
      }  
    }
  
  return(list(U=UList,A=AList))
}


#############################################################
#5 Get the indices of the abstracts involving some university "Uname"
#############################################################
GetAbId = function(Ulist,Uname){
  # if there is no such intitue, return NULL
  UnameAbstractID = NULL
  
  # the abbrevation
  UnameTemp = unlist(strsplit(as.character(Uname),split=""))
  UnameAbb0 = UnameTemp[which(UnameTemp %in% LETTERS)]
  UnameAbb  = paste(UnameAbb0,collapse='')
  
  for(i in 1:length(Ulist)){ 
    Ulisti = unlist(Ulist[i])
    l = length(which(Ulisti==Uname))+length(which(Ulisti==UnameAbb))
    if (l>0){
      UnameAbstractID = c(UnameAbstractID,i)
    }    
  }
  
  return(UnameAbstractID)
}


#############################################################
#7 Get the probabilities for an institute belonging to all groups
#############################################################
Problist = function(Prob,Ulist,Uname){
  # the list of probablities (p1,..,pk)
  # pk is P(university "UName"  is in groups k)
  myIndex = GetAbId(Ulist,Uname)
  
  if(length(myIndex)<=0){
    output = NULL
  }else{
      myProb = Prob[myIndex,]
  }
  
  return(list(Index=myIndex,Prob=myProb))
}


#############################################################
#8 Cross validation for choosing number of groups k
#############################################################
CrossValid = function(dtm2,k0,fold=10){
  # cross validation for a model with k groups
  # the default fold for cross validation is 5
  # output the summed perplexity on test data
  
  #split the data into 5 folds
  subn   = floor(dim(dtm2)[1]/fold)
  TestID = vector('list',fold)
  for (i in 1:(fold-1)){
    TestID[[i]] = ((i-1)*subn+1):(i*subn)
  }
  TestID[[fold]] = (i*subn+1):dim(dtm2)[1]
  
  # gather traing data and test/validation data
  # sum up the likelohood on all test data
  output = 0
  for (i in 1:fold){
    TestDtm  = dtm2[TestID[[i]],]
    TrainDtm = dtm2[-TestID[[i]],]
    myCTM    = CTM(TrainDtm, k = k0, control = list(var = list(tol = 10^-4),
                                                    em = list(tol = 10^-3)))
    output = output+perplexity(myCTM,TestDtm)  
  }
  
  return(output)  
}