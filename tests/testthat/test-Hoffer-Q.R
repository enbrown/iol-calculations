context("Hoffer-Q")

test_that("Hoffer-Q Equation (internal functions)", {
  # Normal eye
  L <- 23.5; K <- 43.5; pACD <- 4.50
  elp <- IOL:::Hoffer.Q.ELP(L = L, K = K, pACD = pACD)
  expect_equivalent(round(elp, digits = 5), 4.40887)
  iol <- IOL:::Hoffer.Q.Power(L = L, K = K, ELP = elp)
  expect_equivalent(round(iol, digits=5), 19.27599)
  
  # Hyperopic eye
  L <- 22.0; K <- 46.0; pACD <- 3.50
  elp <- IOL:::Hoffer.Q.ELP(L = L, K = K, pACD = pACD)
  expect_equivalent(round(elp, digits = 5), 3.14482)
  iol <- IOL:::Hoffer.Q.Power(L = L, K = K, ELP = elp)
  expect_equivalent(round(iol, digits=5), 19.35878)
  
  # Myopic eye
  L <- 26.0; K <- 42.0; pACD <- 5.50
  elp <- IOL:::Hoffer.Q.ELP(L = L, K = K, pACD = pACD)
  expect_equivalent(round(elp, digits = 5), 6.06225)
  iol <- IOL:::Hoffer.Q.Power(L = L, K = K, ELP = elp)
  expect_equivalent(round(iol, digits=5), 15.18708)
  
  # Normal eye
  L <- 23.5; R <- 7.7; pACD <- 4.96
  elp <- IOL:::Hoffer.Q.ELP(L = L, K = 337.5 / R, pACD = pACD)
  iol <- IOL:::Hoffer.Q.Power(L = L, K = 337.5 / R, ELP = elp)
  expect_equivalent(round(iol, digits=2), 19.67)

  L <- 23.5; R <- 7.7; pACD <- 5.59
  elp <- IOL:::Hoffer.Q.ELP(L = L, K = 337.5 / R, pACD = pACD)
  iol <- IOL:::Hoffer.Q.Power(L = L, K = 337.5 / R, ELP = elp)
  expect_equivalent(round(iol, digits=2), 20.88)

  # Very steep cornea
  L <- 23.5; R <- 5; pACD <- 4.96
  elp <- IOL:::Hoffer.Q.ELP(L = L, K = 337.5 / R, pACD = pACD)
  iol <- IOL:::Hoffer.Q.Power(L = L, K = 337.5 / R, ELP = elp)
  expect_equivalent(round(iol, digits=2), -36.48)
  
  L <- 23.5; R <- 5; pACD <- 5.59
  elp <- IOL:::Hoffer.Q.ELP(L = L, K = 337.5 / R, pACD = pACD)
  iol <- IOL:::Hoffer.Q.Power(L = L, K = 337.5 / R, ELP = elp)
  expect_equivalent(round(iol, digits=2), -40.83)

  # Very short eye
  L <- 18.5; R <- 8; pACD <- 4.96
  elp <- IOL:::Hoffer.Q.ELP(L = L, K = 337.5 / R, pACD = pACD)
  iol <- IOL:::Hoffer.Q.Power(L = L, K = 337.5 / R, ELP = elp)
  expect_equivalent(round(iol, digits=2), 42.66)
  
  L <- 18.5; R <- 8; pACD <- 5.59
  elp <- IOL:::Hoffer.Q.ELP(L = L, K = 337.5 / R, pACD = pACD)
  iol <- IOL:::Hoffer.Q.Power(L = L, K = 337.5 / R, ELP = elp)
  expect_equivalent(round(iol, digits=2), 45.59)

  # Long eye
  L <- 28.0; R <- 7; pACD <- 4.96
  elp <- IOL:::Hoffer.Q.ELP(L = L, K = 337.5 / R, pACD = pACD)
  iol <- IOL:::Hoffer.Q.Power(L = L, K = 337.5 / R, ELP = elp)
  expect_equivalent(floor(iol*100)/100, -0.86) 
  # ULIB site seems to round differently
  
  L <- 28.0; R <- 7; pACD <- 5.59
  elp <- IOL:::Hoffer.Q.ELP(L = L, K = 337.5 / R, pACD = pACD)
  iol <- IOL:::Hoffer.Q.Power(L = L, K = 337.5 / R, ELP = elp)
  expect_equivalent(round(iol, digits=2), -0.91)
  
  # Very long eye (verified using Hoffer Programs Ver 1.5a)
  L <- 32; K <- 48.21; pACD <- 4.96
  elp <- IOL:::Hoffer.Q.ELP(L = L, K = K, pACD = pACD)
  iol <- IOL:::Hoffer.Q.Power(L = L, K = K, ELP = elp)
  expect_equivalent(round(iol, digits=2), -11.03)
  
  L <- 32; K <- 48.21; pACD <- 5.59
  elp <- IOL:::Hoffer.Q.ELP(L = L, K = K, pACD = pACD)
  iol <- IOL:::Hoffer.Q.Power(L = L, K = K, ELP = elp)
  expect_equivalent(round(iol, digits=2), -11.67)

  L <- 12; K <- 40; pACD <- 4.97
  elp <- IOL:::Hoffer.Q.ELP(L = L, K = K, pACD = 4.97)
  iol <- IOL:::Hoffer.Q.Power(L = L, K = K, ELP = elp)
  expect_equivalent(round(iol, digits=2), 114.73)
  
  })

test_that("Hoffer-Q Equation (internal functions)", {
  tests <- read.csv('tests.csv')
  tests <- subset(tests, Method == 'Hoffer.Q')
  f.elp <- IOL:::Hoffer.Q.ELP
  f.iol <- IOL:::Hoffer.Q.Power
  
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