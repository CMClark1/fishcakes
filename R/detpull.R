#' Pull and clean length data
#' @description
#' Pull and clean detailed length data for a given species from the survey database.
#' 
#' @param spec A species code.
#' @param minyear A minimum year. Default is 1995, when diet data collection began
#' @param maxyear A maximum year. Default is the present year.
#' @param lgroup Length groups used for selecting individual fish to record stomach contents. Default is 5 because, for the most species, the requirement for stomach sampling is 1 per 5cm.
#' @param username Oracle username. Default is oracle.username for those with an .Rprofile.
#' @param password Oracle password. Default is oracle.password for those with an .Rprofile.
#' @param dsn Oracle dsn. Defauls is oracle.dsn for those with an .Rprofile.
#'
#' @return A dataframe of length observations for a given species with length groups defined.
#' @export
#'
#' @examples
#' x <- detpull(spec=10)
#' 
detpull<- function(spec=NULL, minyear=1995, maxyear=substr(Sys.Date(),1,4),lgroup=5,username=oracle.username, password=oracle.password, dsn=oracle.dsn){
  channel <- ROracle::dbConnect(DBI::dbDriver("Oracle"), username=username, password=password, dsn)
  set.det<-ROracle::dbGetQuery(channel, paste("select a.mission, a.setno, a.flen
                              from groundfish.gsdet a, groundfish.gsmissions b, groundfish.gsinf c
                              where a.spec=",spec,
                              "and b.year between ",minyear," and ",maxyear,
                              "and a.mission=b.mission
                              and a.mission=c.mission
                              and a.setno=c.setno"))
  set.det2<-set.det%>%
    dplyr::mutate(LGROUP=as.numeric(gsub("\\(","",gsub("\\,.*","",cut(FLEN,seq(from=1,to=151,by=lgroup))))))
  return(set.det2)
}
 
