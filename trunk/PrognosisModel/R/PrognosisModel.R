#' @title calculatePrognosis
#' @description Calculates progonsis for give haplotypes and race
#' @details Takes in two haplotype frequencies and a race and returns set of prognosis probabilities
#'
#'  @param h1 First haplotype frequency
#'  @param h2 Second haplotype frequency
#'  @param race Population of haplotypes
#'  @param raw_data Raw data to use for build model
#'
#'  @return prognosis data frame with probabilites of a Good, Fair, or Poor search
#'
#'
#' @examples
#' \dontrun{
#'  h1 <- 0.00339
#'  h2 <- 0.00005
#'  race <- "CAU"
#'  prognosis <- calculatePrognosis(h1, h2,race)
#' }
#'
#' @export
calculatePrognosis <- function(h1, h2, race){

  gf <- haplotypeToGenotypeFrequency(h1,h2,offset_fit)
  log_gf <- log(gf)

  # model does not fit for NAM, default to CAU
  if (race=="NAM"){
    race="UNK"
  }

  # Training data(training_data) is available as part of the package.
  #fit <- buildModel(training_data)
  ###fit is a raw-data item because you don't want to calculate it a bunch

  prog <- proportionalOddsPrognosis(race, log_gf, fit_polr)

  prognosis <- data.frame(Good = prog['A'], Fair = prog['B'], Poor = prog['C'], row.names = NULL)

  prognosis
}

#' Compute Genotype Frequency
#' Currently just multiplies 2 haplotype frequencies. Can be changed to use a
#' different strategy.
haplotypeToGenotypeFrequency <- function(h1, h2,offset_fit) {
  h1<-max(h1,MIN_THRESH_H1)
  h2<-max(h2,MIN_THRESH_H2)

  h1<-log(h1)
  h2<-log(h2)
  GF<-exp((h1+h2)*offset_fit$coefficients[2]+offset_fit$coefficients[1])
  return(GF)
}


