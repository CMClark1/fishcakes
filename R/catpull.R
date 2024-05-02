#' Pull and clean set-level catch data
#' @description
#' Pull and clean set-level catch data for a given species from the survey database.
#' 
#' @param spec A species code.
#' @param minyear A minimum year. Default is 1995, when diet data collection began
#' @param maxyear A maximum year. Default is the present year.
#' @param username Oracle username. Default is oracle.username for those with an .Rprofile.
#' @param password Oracle password. Default is oracle.password for those with an .Rprofile.
#' @param dsn Oracle dsn. Defauls is oracle.dsn for those with an .Rprofile.
#'
#' @return A dataframe of length observations for a given species with length groups defined.
#' @export
#'
#' @examples
#' x <- catpull(spec=10)
#' 
catpull<- function(spec=NULL, minyear=1995, maxyear=substr(Sys.Date(),1,4),lgroup=5,username=oracle.username, password=oracle.password, dsn=oracle.dsn){
  channel <- ROracle::dbConnect(DBI::dbDriver("Oracle"), username=username, password=password, dsn)
  set.cat<-ROracle::dbGetQuery(channel, paste("select a.mission, a.setno, a.dist, b.sampwgt, b.totwgt
                              from groundfish.gsinf a, groundfish.gscat b, groundfish.gsmissions c
                              where b.spec=",spec,
                                              "and c.year between ",minyear," and ",maxyear,
                                              "and a.mission=b.mission
                              and a.mission=c.mission
                              and a.setno=b.setno"))
  allsets<-ROracle::dbGetQuery(channel, paste("select a.mission,a.setno, a.dist
                                              from groundfish.gsinf a, groundfish.gsmissions c
                                              where c.year between",minyear," and ",maxyear,
                                              "and a.mission=c.mission"))
  
  allsets2 <- allsets%>%dplyr::left_join(set.cat)%>%base::replace(is.na(.), 0)
  return(allsets2)
}