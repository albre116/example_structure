
httpHeaders <- c(Accept = "application/json",
                 'Content-Type' = 'application/json;charset=UTF-8')

#' @title getMugForRID
#' @description Returns a MUG for a given RID
#' @details Takes in a RID and a host:port and accesses the server to get the corresponding
#'  mug and returns the results in a list of loci and alleles
#'
#'  @param rid Patient ID
#'  @param host Hostname and Port Number of the Server
#'
#'  @return mug List of the Loci and Alleles on the server for that \code{rid},
#'    returns \code{NULL} if \code{rid} is not found.
#'
#' @importFrom RCurl getURL
#' @importFrom rjson fromJSON
#'
#' @examples
#' \dontrun{
#' mug <- getMugForRID(1419426,'http://p1haplostats-s1:28080')
#' }
#'
#' @export
getMugForRID <- function(rid, host) {
  url <- paste(host, '/hla/rid/', rid, sep='')

  resultJSON <- NULL
  tryCatch(
    resultJSON <- RCurl::getURL(url, httpheader= httpHeaders),
    error = function(e) {
      message(paste("Failed calling the service at URL", url))
    })

  if(!is.null(resultJSON) && nchar(resultJSON) > 0) {
    result <- rjson::fromJSON(resultJSON)
    searchTypings <- result$searchTypings
    mug <- list()
    for(i in 1:length(searchTypings)) {
      locus <- searchTypings[[i]]$hlaLocus
      # Haplogic uses BPR, turn it into B
      if(locus == 'BPR') locus <- 'B'
      type1 <- searchTypings[[i]]$antigen1
      if(type1 == "null") {
        type1 <- ""
      }
      type2 <- searchTypings[[i]]$antigen2
      if(type2 == "null") {
        type2 <- ""
      }
      s <- list(locus=locus, type1=type1, type2=type2)
      mug[[i]] <- s
    }
    return(mug)
  }
  NULL
}
