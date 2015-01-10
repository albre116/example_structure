
httpHeaders <- c(Accept = "application/json",
                 'Content-Type' = 'application/json;charset=UTF-8')

#' @title getSireForRID
#' @description Returns a Sire for a given RID
#' @details Takes in a RID and a host:port and accesses the server to get the corresponding
#'  mug and returns the sire
#'
#'  @param rid Patient ID
#'  @param host Hostname and Port Number of the Server
#'
#'  @return sire Returns a vector of all self identified Broad Race groups,
#'    returns \code{NULL} if \code{rid} is not found.
#'
#' @importFrom RCurl getURL
#' @importFrom rjson fromJSON
#'
#' @examples
#' \dontrun{
#' mug <- getSireForRID(1419426,'http://p1haplostats-s1:28080')
#' }
#'
#' @export
getSireForRID <- function(rid, host){

  # build the complete URL
  url = paste(host, '/sire/rid/', rid, sep='')

  resultJSON <- NULL
  tryCatch(
  # Get SIRE from the server
  resultJSON <- RCurl::getURL(url, httpheader= httpHeaders),
  error = function(e) {
    message(paste("Failed calling the service at URL", url))
  }

  )

  if(!is.null(resultJSON) && nchar(resultJSON) > 0) {
    result <- rjson::fromJSON(resultJSON)
    return(result$races)
  }
  NULL
}

