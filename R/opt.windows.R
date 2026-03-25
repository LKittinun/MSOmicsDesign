#' @title Optimized windows placement
#' @description
#' This function adjusts the edges of isolation windows to position them within forbidden zones, where +2H and +3H peptides are unlikely to be presented.
#'
#' @param mass A numeric vector containing edges of isolation windows
#' @param phospho If `TRUE` will also adjust for phosphopeptides.
#' @return Adjusted masses
#' @export
#'
#' @examples
#' windows <- seq(400,1000,20)
#' opt.windows(windows)
#' @references
#' * Egertson JD, Kuehn A, Merrihew GE, Bateman NW, MacLean BX, Ting YS, et al. Multiplexed MS/MS for improved data-independent acquisition. Nat Methods 2013;10:744–6.
#' * Pino LK, Just SC, MacCoss MJ, Searle BC. Acquiring and Analyzing Data Independent Acquisition Proteomics Experiments without Spectrum Libraries. Molecular & Cellular Proteomics 2020;19:1088–103.


opt.windows <- function(mass, phospho = FALSE) {
  if(phospho){
    constant <- 0.25 + (97.976896/1.00045475 - ceiling(97.976896/1.00045475))
  }
  else{
    constant <- 0.25
  }
    mass <- constant + ceiling(mass/1.00045475)*1.00045475

  return(mass)
}




