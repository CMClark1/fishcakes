
#Functions to pull and clean data from the .csv file that is standard output from the Food Habits Program

#For predation files (where a given species is the predator)
predpull<-function(file=NULL,lgroup=5){
  pred.dat<-read.csv(file)
  pred.dat2 <-pred.dat%>%
    filter(!is.na(PWT))%>%
    mutate(LGROUP=as.numeric(gsub("\\(","",gsub("\\,.*","",cut(FLEN,seq(from=1,to=151,by=lgroup))))))
  return(pred.dat2)
}

#For prey files (where a given species is the prey)
preypull<-function(file=NULL,lgroup=5){
  prey.dat<-read.csv(file)
  prey.dat2 <-prey.dat%>%
    filter(!is.na(PWT))%>%
    mutate(LGROUP=as.numeric(gsub("\\(","",gsub("\\,.*","",cut(FLEN,seq(from=1,to=151,by=lgroup))))))
  return(prey.dat2)
}
