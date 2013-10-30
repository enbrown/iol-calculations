ELP.functions$Holladay <- function(A) {
  # http://books.google.com/books?id=SHjgbQ9auqYC&pg=PT93&dq=iol+calculation+1336&hl=en&sa=X&ei=kXsXUubKNeiW2gX1woDgDQ&ved=0CEMQ6AEwAg#v=onepage&q=iol%20calculation%201336&f=false
  ELP <- Constants$IOL$A_to_pACD_a0 + Constants$IOL$A_to_pACD_a1 * A
  result <- ELP
  attr(result, 'parameters') <- list(A = A)
  return(result)
}

Power.functions$Holladay <- function(L, K, ELP, Rx = 0, V = 13) {
  args <- list(L = L, K = K, ELP = ELP, Rx = Rx, V = V)
  P <- 1336 / (L - ELP) -
    1336 / (
      1336 / ((1000 / (1000 / Rx - V)) + K) 
      - ELP)
  attr(P, 'parameters') <- args
  return(P)
}