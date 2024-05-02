#' Pull and clean diet data from .csv file
#' @description
#' Pull and clean diet data from a .csv file provided by the Food Habits Program.
#' 
#' @param file A file name address.
#' @param lgroup Length groups used for selecting individual fish to record stomach contents. Default is 5 because, for the most species, the requirement for stomach sampling is 1 per 5cm.
#' @param lifestage Default is to group all lifestages together. To compare juveniles to adults, use code 1 and specify juvmaxlen (below).
#' @param juvmaxlen Juvenile maximum length. Specify the maximum length for a juvenile of the predator species. Default is 40 (cm).
#' 
#'
#' @return A dataframe of data without missing prey weights and with length groups specified.
#' @export
#'
#' @examples
#' x <- csvpull(file="C:/USER/filename.csv")
#' 
csvpull<-function(file=NULL,lgroup=5,lifestage="GROUP",juvmaxlen=40){
  pred.dat<-utils::read.csv(file)
  pred.dat2 <-pred.dat%>%
    dplyr::filter(!is.na(PWT))%>%
    dplyr::mutate(LGROUP=as.numeric(gsub("\\(","",gsub("\\,.*","",cut(FLEN,seq(from=1,to=151,by=lgroup))))),LIFESTAGE=lifestage,LIFESTAGE=dplyr::case_when(LIFESTAGE==1 & FLEN<=juvmaxlen ~ "JUVENILE",
                                                        LIFESTAGE==1 & FLEN>juvmaxlen ~ "ADULT",
                                                        LIFESTAGE!=1 ~ "GROUP"))
  return(pred.dat2)
}