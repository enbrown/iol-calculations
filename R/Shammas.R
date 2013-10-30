Power.functions$Shammas <- function(L, K, ELP) {
  args <- list(L = L, K = K, ELP = ELP)
  #https://encrypted.google.com/books?id=NhWJsGFK6qgC&pg=PA8&lpg=PA8&dq=colenbrander+formula&source=bl&ots=j__sC0XHHg&sig=DEaK9Qg0LOvedzq4itcfe7pBZ4Q&hl=en&sa=X&ei=scdlUuC1JcHP2wW90IDAAg&ved=0CD4Q6AEwAg#v=onepage&q=colenbrander%20formula&f=false
  # Assumes axial length is from immersion ultrasound (add 0.24mm for contact)
  P <- 1336 / (L - 0.1 * (L - 23) - ELP - 0.05) -
    1 / (1.0125 / K - (ELP + 0.05) / 1336)
  attr(P, 'parameters') <- args
  return(P)
}