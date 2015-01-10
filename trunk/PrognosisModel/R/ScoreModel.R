#' @title proportionalOddsPrognosis
#' @description predictions from a polr model fit
#' @details prediction probabilities [good,fair,poor] for a given individual
#'
#'  @param race for prediction
#'  @param log_geno_freq is the offset scaled genotype frequency f(h1,h2)
#'  @param fit is the polr model object
#'
#'  @return Probability triplet
#'
#' @importFrom MASS polr
#'
#' @examples
#' \dontrun{
#'
#' proportionalOddsPrognosis("AFA", -11.56, fit)
#' }
#'
#'
proportionalOddsPrognosis <- function(race, log_geno_freq, fit){
  predict(fit,newdata=data.frame("GF"=log_geno_freq,"Race"=race),type="probs")
}



