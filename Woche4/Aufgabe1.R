
Salim Gnaoui and Gharbi Marwen
install.packages('plyr')
library(plyr)


###2.2 
### get the csv file to open, file path is given as argument
myread.cvsdata <- function (path) {
  ### transform the csv into data frame, 
  ###after some searches, I figured out that I should use '/t' as seperator as my local-configuration is in German, otherwise it will not work , 
  data=read.table((path),header = T,sep = "\t")
  ###  import the csv file, and prevent r fron transforming the columns (names are given in the vector c) to "FACTOR"
  data=read.table((path),header = T,sep = "\t",as.is=c("developer","tstamp","file"))
  ### YOU can run View(data) to visualize the dataframe (if you are using R studio)
  ### extract the column named file (first position) from data , 
  
  file=data[,1]
  ### extract the column named developer (3 rd position) from data , 
  developer=data[,3]
  ### transform the extracted sets  to vectors then to factor
  file=factor(c(file))
  developer=factor (c(developer))
  data <- cbind(data,data.frame(file))
  data <- cbind(data,data.frame(developer))
  ### rename the factors (position 7 and 8)
  names(data)[[(8)]] <- "filef"
  names(data)[[(9)]] <- "developerf"
  data <- subset(data, select = -c(7))
  
  return (data) ;
  
}

changedfiles.count <- function (data){
  files = factor(data$file);
 nlevels((files)) 
}
changedfiles.top <- function (data){
  result=sort(table(data$file),decreasing =T)
  print(result[1:5])
  return (result)
}
### Im using the plyr library to count frequency of elements 
changedfiles.ntimes <- function(data)
{
  ### count frequency of column file and save into into variable files
  files=count(result,"file")
  ### tranform the colum freq of dataframe files into a factor
  files$freq=factor(files$freq)
  ### add levels 1,2,3,5,10 to show empty elements 
  levels(files$freq)=c(levels(files$freq), c(1,2,3,5,10))
  ### recompute the frequency of file's frequency
  result= (table(files$freq))
  ### print only 1,2,3,5 and10 , as mentionned in the exercice
   print (result[c("1","2","3","5","10")])
}
### it wokrs as expected, but this is not the best solution , I tried using sapply and tapply and I didnt get it, 
changedfiles.touchedbyn <- function(data) {
  h=data.frame(table(result$developer,result$file))
  k=subset( h, h$Freq > 0)
  finalresult=table(k$Var2)
  finalresult=data.frame(finalresult)
  finalresult$Freq=factor(finalresult$Freq)
  levels(finalresult$Freq)=c(levels(finalresult$Freq), c(1:10))
  k=table(finalresult$Freq)
  print(k)
}
### reutrn either filetype or "nothing" IF it doesnt exist  
getFileextension <- function(filename) {
  if (sub("^.*\\.([a-zA-Z0-9]*)$","", filename)=="")
  return (tolower(strsplit(filename,split='.', fixed=TRUE)[[1]][[2]]))
  else return ("nothing")
} 
filetypes.changes.total<- function(data)
{
  result =table(result$filetypef)
  print (result)
  return (result)
}

filetypes.changes.mean <- function(data) {
  result =sapply(data,mean)
  
  return (result)
}
getnumberfile <-function (data) {
  p=data.frame(result$filef,result$filetypef)
  u=unique(p)
  print(table(u$result.filetypef))
  return (table(u$result.filetypef))
}
getmaxfile <-function (data) {

  result= (table(data$file))
  result=data.frame(result)
  result$Var1=as.character(result$Var1)
  result$filetypef <- lapply(result$Var1, function(x) sapply(x, getFileextension))
  return (result)
}


result=myread.cvsdata(file.choose())
result$filetypef <- lapply(result$file, function(x) sapply(x, getFileextension))
result$filetypef=as.character(result$filetypef)
r=changedfiles.count(result)                  
r=changedfiles.top(result)
changedfiles.ntimes(result)
changedfiles.touchedbyn(result)
filetypeschangetot=filetypes.changes.total(data)
filetypesmean=filetypes.changes.mean(filetypeschangetot)
filewithnumberfile=getnumberfile(result)
g=getmaxfile(result)
g$filetypef=as.character(g$filetypef)
min=as.table(tapply(g$Freq,g$filetypef,min))
max=as.table(tapply(g$Freq,g$filetypef,max))
finaltable=""

finaltable=rbind(finaltable,mean=filetypesmean,number=filewithnumberfile,min=min,max=max)
