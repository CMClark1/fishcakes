#' Pull and clean diet data from .csv file
#' @description
#' Pull and clean diet data from a .csv file provided by the Food Habits Program.
#' 
#' @param file A file name address.
#' @param lgroup Length groups used for selecting individual fish to record stomach contents. Default is 5 because, for the most species, the requirement for stomach sampling is 1 per 5cm.
#'
#' @return A dataframe of data without missing prey weights and with length groups specified.
#' @export
#'
#' @examples
#' x <- csvpull(file="C:/USER/filename.csv")
#' 
csvpull<-function(file=NULL,lgroup=5){
  pred.dat<-read.csv(file)
  pred.dat2 <-pred.dat%>%
    filter(!is.na(PWT))%>%
    mutate(LGROUP=as.numeric(gsub("\\(","",gsub("\\,.*","",cut(FLEN,seq(from=1,to=151,by=lgroup))))))
  return(pred.dat2)
}