ELP.functions$Hoffer <- function(L, ACD, A) {
  args <- list(L = L)
  if (missing(ACD) || ! is.finite(ACD)) {
    ACD <- ELP.functions$Holladay(A)
    args$A <- A
    warning("Using Holladay approximation for Hoffer's ACD from A constant: ", ACD, "mm")
  } else {
    args$ACD <- ACD
  }
  ELP <- 0.292 * L - 2.93 + (ACD - 3.94)
  result <- ELP
  attr(result, 'parameters') <- args
  return(result)
}


Power.functions$Hoffer <- function(L, K, ELP) {
  args <- list(L = L, K = K, ELP = ELP)
  P <- 1336 / (L - ELP - 0.05) - 
    1336 / (1336 / K - ELP - 0.05)
  attr(P, 'parameters') <- args
  return(P)
  #https://encrypted.google.com/books?id=NhWJsGFK6qgC&pg=PA8&lpg=PA8&dq=colenbrander+formula&source=bl&ots=j__sC0XHHg&sig=DEaK9Qg0LOvedzq4itcfe7pBZ4Q&hl=en&sa=X&ei=scdlUuC1JcHP2wW90IDAAg&ved=0CD4Q6AEwAg#v=onepage&q=colenbrander%20formula&f=false
  # equivalent formula
  return(1336 / (L - ELP - 0.05) - 
           1.336 / (1.336 / K - (ELP + 0.05) / 1000))
}