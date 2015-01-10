#' @title calculateBayesProbabilities
#' @description Calculates bayes race classification based upon MUG likelihood and Prior Race Distribution
#' @details Takes in a MUG typing and prior race distribution to return a race classification
#'
#'  @param mug as returned from HaplotypeServiceClient package function "imputePairForMug"
#'  @param race are the race groups to consider
#'  @param prior is the distribution to apply to the likelihood in forming the posterior
#'  @param host is the location of the imputePairForMug client service that retreives the HLA information
#'
#'  @return A posterior distribution of [race|mug,prior]
#'
#'  @import HaplotypeServiceClient
#'
#' @examples
#' \dontrun{
#'mug<-list(structure(list(locus = "A", type1 = "33:03", type2 = "68:01"), .Names = c("locus",
#'                                                                                    "type1", "type2")), structure(list(locus = "B", type1 = "14:01",
#'                                                                                                                       type2 = "55:01"), .Names = c("locus", "type1", "type2")),
#'          structure(list(locus = "C", type1 = "03:03", type2 = "07:05"), .Names = c("locus",
#'                                                                                    "type1", "type2")), structure(list(locus = "DQB1", type1 = "06:03",
#'                                                                                                                       type2 = "06:09"), .Names = c("locus", "type1", "type2"
#'                                                                                                                       )), structure(list(locus = "DRB1", type1 = "13:01", type2 = ""), .Names = c("locus",
#'                                                                                                                                                                                                   "type1", "type2")))
#'race<-c("CAU","AFA","HIS","API")
#'prior<-c(75,25,100,20)
#'host<-"http://p1haplostats-s1:28080"
#'out<-calculateBayesProbabilities(mug,race,prior,host)
#' }
#'
#' @export
calculateBayesProbabilities <- function(mug,race,prior,host){
  imputation<-lapply(race,imputePairForMug,mug=mug,host=host)
  imputation<-lapply(1:length(race),function(i){
    b<-imputation[[i]]
    if(is.null(b)){return(data.frame("cumulative_GF"=0,"race"=race[i]))}
    b$Frequency1<-as.numeric(b$Frequency1)
    b$Frequency2<-as.numeric(b$Frequency2)
    idx<-b$Haplotype1==b$Haplotype2
    GF<-b$Frequency1*b$Frequency2*(2-idx)
    data.frame("likelihood"=sum(GF),"race"=race[i])
  })
  imputation<-matrix(unlist(imputation),ncol=2,byrow=T)
  colnames(imputation)<-c("likelihood","race")
  imputation<-data.frame(prior=prior/sum(prior),imputation)
  class(imputation$likelihood)<-"numeric"
  imputation$un_normalized_post<-imputation$prior*imputation$likelihood
  imputation$posterior<-imputation$un_normalized_post/sum(imputation$un_normalized_post)
  return(data.frame(race=imputation$race,prior=imputation$prior,likelihood=imputation$likelihood,posterior=imputation$posterior))
}
