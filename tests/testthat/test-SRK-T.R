context("SRK/T")

test_that("SRK/T Equation (internal functions)", {
  tests <- read.csv('tests.csv')
  tests <- subset(tests, Method == 'SRK.T')
  f.elp <- IOL:::SRK.T.ELP
  f.iol <- IOL:::SRK.T.Power
  
  arglist <- function(x){ 
    z <- deparse(substitute(x))
    nams <- names(x)
    paste(sapply(seq_along(x), function(i) paste(names(x)[i], '=', x[[i]])),
          collapse = ', ')
  }
  
  for (i in 1:nrow(tests)) {
    args <- tests[i,]
    args <- args[,!is.na(args)]
    elp <- do.call(f.elp, args[names(args) %in% names(formals(f.elp))])
    if ('ELP' %in% names(args)) {
      expect_equivalent(round(elp, digits = 2),
                        round(args$ELP, digits = 2),
                        info=arglist(c(line = i, args, got = elp)))
    } else {
      args$ELP <- elp[[1]]
    }
    args$ELP <- elp[[1]]
    iol <- do.call(f.iol, args[names(args) %in% names(formals(f.iol))])
    if ('Power' %in% names(args)) {
      expect_equivalent(round(iol, digits = 2),
                        round(args$Power, digits = 2),
                        info=arglist(c(line = i, args, got = iol)))
    }
  }
})