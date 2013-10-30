ELP.functions$Haigis <- function(pACD) {
  result <- pACD
  attr(result, 'parameters') <- list(pACD = pACD)
  return(result)
}