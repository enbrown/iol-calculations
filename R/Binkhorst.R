Power.functions$Binkhorst <- function(L, R, K, cornea_n, ELP) {
  args <- list(L = L, K = K)
  if (missing(R) || ! is.finite(R)) {
    if (missing(cornea_n) || is.finite(cornea_n)) {
      cornea_n <- Constants$Biometry$corneal_index
      warning("Using standard corneal index of refraction to convert K to R for Binkhorst equation: ", cornea_n)
    }
    R <- 1000 * (cornea_n - 1) / K
    args$K <- R
    args$cornea_n <- cornea_n
    warning("Convert K to R for Binkhorst equation: ", R, "mm")
  } else {
    args$R <- R
  }
  #https://encrypted.google.com/books?id=NhWJsGFK6qgC&pg=PA8&lpg=PA8&dq=colenbrander+formula&source=bl&ots=j__sC0XHHg&sig=DEaK9Qg0LOvedzq4itcfe7pBZ4Q&hl=en&sa=X&ei=scdlUuC1JcHP2wW90IDAAg&ved=0CD4Q6AEwAg#v=onepage&q=colenbrander%20formula&f=false
  P <- 1336 * (4 * R - L) / 
    ((L - ELP) * (4 * R - ELP))
  attr(P, 'parameters') <- args
  return(P)
}

Power.functions$Binkhorst.adjusted <- function(L, R, K, cornea_n, ELP) {
  args <- list(L = L, ELP = ELP)
  if (missing(R) || ! is.finite(R)) {
    if (missing(cornea_n) || ! is.finite(cornea_n)) {
      cornea_n <- Constants$Biometry$corneal_index
      warning("Using standard corneal index of refraction to convert K to R for Binkhorst's adjusted equation: ", cornea_n)
    }
    R <- 1000 * (cornea_n - 1) / K
    args$K <- K
    args$cornea_n <- cornea_n
    warning("Convert K to R for Binkhorst's adjusted equation: ", R, "mm")
  } else {
    args$R <- R
  }
  #https://encrypted.google.com/books?id=NhWJsGFK6qgC&pg=PA8&lpg=PA8&dq=colenbrander+formula&source=bl&ots=j__sC0XHHg&sig=DEaK9Qg0LOvedzq4itcfe7pBZ4Q&hl=en&sa=X&ei=scdlUuC1JcHP2wW90IDAAg&ved=0CD4Q6AEwAg#v=onepage&q=colenbrander%20formula&f=false
  aELP <- ELP + 0.17 * (L - 23.45)
  P <- 1336 * (4 * R - L) / 
    ((L - aELP) * (4 * R - aELP))
  attr(P, 'parameters') <- args
  return(P)
}