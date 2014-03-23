context("Haigis ELP functions")

#
# Check internal function
#
test_that("Haigis equation (internal functions)", {
  expect_warning(elp <- IOL:::Haigis.ELP(L = 23.5, ACD = 3.3, A = 118))
  expect_equivalent(elp, 4.94706)
  expect_warning(elp <- IOL:::Haigis.ELP(L = 23.5, ACD = 3.3, pACD = 4.97))
  expect_equivalent(elp, 4.953)
  elp <- IOL:::Haigis.ELP(L = 23.5, ACD = 3.3, a0 = 1.277)
  expect_equivalent(elp, 4.947)

  # Normal eyes
  L <- 23.5; ACD <- 3.3; R <- 7.7; A <- 118.0; P <- 20.79
  expect_warning(elp <- IOL:::Haigis.ELP(L = L, ACD = ACD, A = A))
  iol <- IOL:::Haigis.Power(L = L, R = R, ELP = elp)
  expect_equivalent(round(iol, digits=2), P)
  L <- 23.5; ACD <- 3.3; R <- 7.7; A <- 119.0; P <- 22.05
  expect_warning(elp <- IOL:::Haigis.ELP(L = L, ACD = ACD, A = A))
  iol <- IOL:::Haigis.Power(L = L, R = R, ELP = elp)
  expect_equivalent(round(iol, digits=2), P)
  
  # Short eyes
  L <- 19.0; ACD <- 3.3; R <- 7.7; A <- 118.0; P <- 41.77
  expect_warning(elp <- IOL:::Haigis.ELP(L = L, ACD = ACD, A = A))
  iol <- IOL:::Haigis.Power(L = L, R = R, ELP = elp)
  expect_equivalent(round(iol, digits=2), P)
  L <- 19.0; ACD <- 3.3; R <- 7.7; A <- 119.0; P <- 44.70
  expect_warning(elp <- IOL:::Haigis.ELP(L = L, ACD = ACD, A = A))
  iol <- IOL:::Haigis.Power(L = L, R = R, ELP = elp)
  expect_equivalent(round(iol, digits=2), P)

  # Long eyes
  L <- 32.0; ACD <- 3.3; R <- 7.7; A <- 118.0; P <- -1.96
  expect_warning(elp <- IOL:::Haigis.ELP(L = L, ACD = ACD, A = A))
  iol <- IOL:::Haigis.Power(L = L, R = R, ELP = elp)
  expect_equivalent(round(iol, digits=2), P)
  L <- 32.0; ACD <- 3.3; R <- 7.7; A <- 119.0; P <- -2.05
  expect_warning(elp <- IOL:::Haigis.ELP(L = L, ACD = ACD, A = A))
  iol <- IOL:::Haigis.Power(L = L, R = R, ELP = elp)
  expect_equivalent(round(iol, digits=2), P)

  # Steep eyes
  L <- 23.5; ACD <- 3.3; R <- 6; A <- 118.0; P <- 2.55
  expect_warning(elp <- IOL:::Haigis.ELP(L = L, ACD = ACD, A = A))
  iol <- IOL:::Haigis.Power(L = L, R = R, ELP = elp)
  expect_equivalent(round(iol, digits=2), P)
  L <- 23.5; ACD <- 3.3; R <- 6; A <- 119.0; P <- 2.73
  expect_warning(elp <- IOL:::Haigis.ELP(L = L, ACD = ACD, A = A))
  iol <- IOL:::Haigis.Power(L = L, R = R, ELP = elp)
  expect_equivalent(round(iol, digits=2), P)

  # Flat eyes
  L <- 23.5; ACD <- 3.3; R <- 11; A <- 118.0; P <- 38.09
  expect_warning(elp <- IOL:::Haigis.ELP(L = L, ACD = ACD, A = A))
  iol <- IOL:::Haigis.Power(L = L, R = R, ELP = elp)
  expect_equivalent(round(iol, digits=2), P)
  L <- 23.5; ACD <- 3.3; R <- 11; A <- 119.0; P <- 40.05
  expect_warning(elp <- IOL:::Haigis.ELP(L = L, ACD = ACD, A = A))
  iol <- IOL:::Haigis.Power(L = L, R = R, ELP = elp)
  expect_equivalent(round(iol, digits=2), P)

  # Unrealistic eyes
  L <- 12; ACD <- 1; R <- 5; A <- 118.0; P <- 69.10
  expect_warning(elp <- IOL:::Haigis.ELP(L = L, ACD = ACD, A = A))
  iol <- IOL:::Haigis.Power(L = L, R = R, ELP = elp)
  expect_equivalent(round(iol, digits=2), P)
  L <- 12; ACD <- 3; R <- 5; A <- 118.0; P <- 79.42
  expect_warning(elp <- IOL:::Haigis.ELP(L = L, ACD = ACD, A = A))
  iol <- IOL:::Haigis.Power(L = L, R = R, ELP = elp)
  expect_equivalent(round(iol, digits=2), P)
  L <- 12; ACD <- 6; R <- 5; A <- 118.0; P <- 100.09
  expect_warning(elp <- IOL:::Haigis.ELP(L = L, ACD = ACD, A = A))
  iol <- IOL:::Haigis.Power(L = L, R = R, ELP = elp)
  expect_equivalent(round(iol, digits=2), P)
  L <- 12; ACD <- 1; R <- 11; A <- 118.0; P <- 114.22
  expect_warning(elp <- IOL:::Haigis.ELP(L = L, ACD = ACD, A = A))
  iol <- IOL:::Haigis.Power(L = L, R = R, ELP = elp)
  expect_equivalent(round(iol, digits=2), P)
  L <- 12; ACD <- 3; R <- 11; A <- 118.0; P <- 127.66
  expect_warning(elp <- IOL:::Haigis.ELP(L = L, ACD = ACD, A = A))
  iol <- IOL:::Haigis.Power(L = L, R = R, ELP = elp)
  expect_equivalent(round(iol, digits=2), P)
  L <- 12; ACD <- 6; R <- 11; A <- 118.0; P <- 153.70
  expect_warning(elp <- IOL:::Haigis.ELP(L = L, ACD = ACD, A = A))
  iol <- IOL:::Haigis.Power(L = L, R = R, ELP = elp)
  expect_equivalent(round(iol, digits=2), P)

  L <- 32; ACD <- 1; R <- 5; A <- 118.0; P <- -38.21
  expect_warning(elp <- IOL:::Haigis.ELP(L = L, ACD = ACD, A = A))
  iol <- IOL:::Haigis.Power(L = L, R = R, ELP = elp)
  expect_equivalent(round(iol, digits=2), P)
  L <- 32; ACD <- 3; R <- 5; A <- 118.0; P <- -41.55
  expect_warning(elp <- IOL:::Haigis.ELP(L = L, ACD = ACD, A = A))
  iol <- IOL:::Haigis.Power(L = L, R = R, ELP = elp)
  expect_equivalent(round(iol, digits=2), P)
  L <- 32; ACD <- 6; R <- 5; A <- 118.0; P <- -47.47
  expect_warning(elp <- IOL:::Haigis.ELP(L = L, ACD = ACD, A = A))
  iol <- IOL:::Haigis.Power(L = L, R = R, ELP = elp)
  expect_equivalent(round(iol, digits=2), P)
  L <- 32; ACD <- 1; R <- 11; A <- 118.0; P <- 15.40
  expect_warning(elp <- IOL:::Haigis.ELP(L = L, ACD = ACD, A = A))
  iol <- IOL:::Haigis.Power(L = L, R = R, ELP = elp)
  expect_equivalent(round(iol, digits=2), P)
  L <- 32; ACD <- 3; R <- 11; A <- 118.0; P <- 16.19
  expect_warning(elp <- IOL:::Haigis.ELP(L = L, ACD = ACD, A = A))
  iol <- IOL:::Haigis.Power(L = L, R = R, ELP = elp)
  expect_equivalent(round(iol, digits=2), P)
  L <- 32; ACD <- 6; R <- 11; A <- 118.0; P <- 17.51
  expect_warning(elp <- IOL:::Haigis.ELP(L = L, ACD = ACD, A = A))
  iol <- IOL:::Haigis.Power(L = L, R = R, ELP = elp)
  expect_equivalent(round(iol, digits=2), P)
})

test_that("Haigis equation (internal functions)", {
  expect_error(IOL:::Haigis.ELP())
  expect_error(IOL:::Haigis.ELP(L = 23))
  expect_error(IOL:::Haigis.ELP(ACD = 3.3))
  expect_error(IOL:::Haigis.ELP(a0 = 1.0))
  expect_error(IOL:::Haigis.ELP(L = 23, ACD = 3.3))
  expect_error(IOL:::Haigis.ELP(ACD = 3.3, a0 = 1.0))
  #expect_error(IOL:::Haigis.ELP(L = 23, a0 = 1.0))
  expect_warning(IOL:::Haigis.ELP(L = 23, ACD = 3.3, A = 118))
  expect_warning(IOL:::Haigis.ELP(L = 23, ACD = 3.3, pACD = 4.5))
})  

