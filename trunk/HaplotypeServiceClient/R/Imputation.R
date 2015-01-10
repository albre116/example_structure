
httpHeaders <- c(Accept = "application/json",
                 'Content-Type' = 'application/json;charset=UTF-8')

#' @title imputeForMug
#' @description Returns a Imputation List
#' @details Takes in a population and MUG and returns a list of probable
#' haplotypes and their frequencies
#'
#'  @param populations Population to search in
#'  @param mug List of alleles
#'  @param host Hostname and Port Number of the Server
#'
#'  @return List of probable haplotypes and their frequencies in the given population
#'
#' @importFrom RCurl getURL basicTextGatherer curlPerform
#' @importFrom rjson fromJSON toJSON
#'
#' @examples
#' \dontrun{
#' mug <- getMugForRID(1419426,'http://p1haplostats-s1:28080')
#'
#' imputeForMug("CAU",mug,'http://p1haplostats-s1:28080')
#' }
#'
#' @export
#'

imputeForMug <- function(population, mug, host){
  if(!isValidPopulation(population)) {
    return(NULL)
  }
  if(!isValidMUG(mug)) {
    return(NULL)
  }

  mug <- fixBPR(mug)

  url <- paste(host, '/impute/haplotype', sep='')

  # 1. Populations
  if (population == "UNK" || population == "NAM"){
    population  <-  "CAU"
  }
  populations <- list(population)
  # Assemble populations and HLA data
  hla <- list(populations=populations, hlaMUG=mug)

  request <- rjson::toJSON(hla)


  # Send it to the server
  reader <- RCurl::basicTextGatherer()
  status <- RCurl::curlPerform(url = url,
                               httpheader = httpHeaders,
                               postfields = request,
                               writefunction = reader$update)
  if(status == 0) {
    # Success
    response <- reader$value()
    #print("Response:")
    #print(response)
    # haplotype
    imputedHaplotypes <- rjson::fromJSON(response)[[1]]
    size <- length(imputedHaplotypes)
    options(stringsAsFactors=F)
    haplotypes = data.frame()
    #print(imputedHaplotypes)
    for(i in 1:size) {
      imputedHaplotype <- imputedHaplotypes[[i]]
      haplotypes <- rbind(haplotypes, c(imputedHaplotype$haplotype, imputedHaplotype$population, imputedHaplotype$frequency))
    }
    names(haplotypes) <- c("Haplotype", "Population", "Frequency")
    return(haplotypes)
  } else {
    print("Failed with status:")
    print(status)
  }
  NULL

}

#' @title imputePairForMug
#' @description Returns a Imputation pairs List
#' @details Takes in a population and MUG and returns a list of probable
#'  haplotype pairs and their frequencies
#'
#'  @param populations Population to search in
#'  @param mug List of alleles
#'  @param host Hostname and Port Number of the Server
#'
#'  @return List of probable haplotype pairs and their frequencies in the given population
#'
#' @importFrom RCurl getURL basicTextGatherer curlPerform
#' @importFrom rjson fromJSON toJSON
#'
#' @examples
#' \dontrun{
#' mug <- getMugForRID(1419426,'http://p1haplostats-s1:28080')
#'
#' imputePairForMug("CAU",mug,'http://p1haplostats-s1:28080')
#' }
#'
#' @export
#'

imputePairForMug <- function(population, mug, host){
  if(!isValidPopulation(population)) {
    return(NULL)
  }
  if(!isValidMUG(mug)) {
    return(NULL)
  }

  mug <- fixBPR(mug)

  url <- paste(host, '/impute/haplotype-pairs', sep='')

  # 1. Populations
  if (population == "UNK"){
    population  <-  "CAU"
  }
  populations <- list(population)
  # Assemble populations and HLA data
  hla <- list(populations=populations, hlaMUG=mug)

  request <- rjson::toJSON(hla)


  # Send it to the server
  reader <- RCurl::basicTextGatherer()
  status <- RCurl::curlPerform(url = url,
                               httpheader = httpHeaders,
                               postfields = request,
                               writefunction = reader$update)
  if(status == 0) {
    # Success
    response <- reader$value()
    #print("Response:")
    #print(response)
    # haplotype
    responseJSON <- rjson::fromJSON(response);
    if(length(responseJSON) < 1) {
      message("No pairs found that explain the HLA")
    } else {
      imputedHaplotypePairs <- rjson::fromJSON(response)[[1]]
      size = length(imputedHaplotypePairs)
      options(stringsAsFactors=FALSE)
      haplotype.pairs = data.frame()
      for(i in 1:size) {
        pairs <- imputedHaplotypePairs[i][[1]]
        haplotype1 <- pairs$haplotype1
        haplotype2 <- pairs$haplotype2
        haplo_pair <- c(as.character(haplotype1$haplotype), as.character(haplotype1$population), as.numeric(haplotype1$frequency),
                        as.character(haplotype2$haplotype), as.character(haplotype2$population), as.numeric(haplotype2$frequency),
                        as.numeric(pairs$frequency))
        #print(haplo_pair)
        haplotype.pairs <- rbind(haplotype.pairs, haplo_pair)

        s1 <- paste(haplotype1$haplotype, haplotype1$population, haplotype1$frequency)
        s2 <- paste(haplotype2$haplotype, haplotype2$population, haplotype2$frequency)
        s <- paste(s1, s2, pairs$frequency)
        #print(s)
      }
      names(haplotype.pairs) <- c("Haplotype1", "Race1", "Frequency1", "Haplotype2", "Race2", "Frequency2", "Pair Frequency")
      return(haplotype.pairs)
    }
  } else {
    message(paste("Failed with status:",status))
  }
  NULL


}



isValidPopulation <- function(population){

  !is.null(population) && (length(population) == 1) && (nchar(population) > 0)

}

isValidMUG <- function(mug){
  !is.null(mug)
}

fixBPR <- function(mug) {
  for(i in 1:length(mug)) {
    if(mug[[i]]$locus == 'B') {
      mug[[i]]$locus <- 'BPR'
    }
  }
  mug
}


