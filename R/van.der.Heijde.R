Power.functions$van.der.Heijde <- function(L, K, ELP) {
  #https://encrypted.google.com/books?id=NhWJsGFK6qgC&pg=PA8&lpg=PA8&dq=colenbrander+formula&source=bl&ots=j__sC0XHHg&sig=DEaK9Qg0LOvedzq4itcfe7pBZ4Q&hl=en&sa=X&ei=scdlUuC1JcHP2wW90IDAAg&ved=0CD4Q6AEwAg#v=onepage&q=colenbrander%20formula&f=false
  P <- 1336 / (L - ELP) -
    1 / (1 / K - ELP / 1336)
  attr(P, 'parameters') <- list(L = L, K = K, ELP = ELP)
  return(P)
}