context("SRK-II")

test_that("SRK-II Equation (internal functions)", {
  # Normal eye
  L <- 23.5; R <- 7.75; A <- 118.0
  iol <- IOL:::SRK.II.Power(L = L, K = 337.5/R, A = A)
  expect_equivalent(round(iol, digits=2), 20.06)
  L <- 23.5; R <- 7.75; A <- 119.0
  iol <- IOL:::SRK.II.Power(L = L, K = 337.5/R, A = A)
  expect_equivalent(round(iol, digits=2), 21.06)
  
  # Hyperopic eye
  L <- 22.0; R <- 7.34; A <- 118.0
  iol <- IOL:::SRK.II.Power(L = L, K = 337.5/R, A = A)
  expect_equivalent(round(iol, digits=2), 21.62)
  L <- 22.0; R <- 7.34; A <- 119.0
  iol <- IOL:::SRK.II.Power(L = L, K = 337.5/R, A = A)
  expect_equivalent(round(iol, digits=2), 22.62)
  
  # Myopic eye
  L <- 26.0; R <- 8.04; A <- 118.0
  iol <- IOL:::SRK.II.Power(L = L, K = 337.5/R, A = A)
  expect_equivalent(round(iol, digits=2), 14.72)
  L <- 26.0; R <- 8.04; A <- 119.0
  iol <- IOL:::SRK.II.Power(L = L, K = 337.5/R, A = A)
  expect_equivalent(round(iol, digits=2), 15.72)
  
  # Span of eye lengths
  L <- 19.0; R <- 7.0; A <- 118.0; P <- 30.11
  iol <- IOL:::SRK.II.Power(L = L, K = 337.5/R, A = A)
  expect_equivalent(round(iol, digits=2), P)
  
  L <- 20.0; R <- 7.0; A <- 118.0; P <- 26.61
  iol <- IOL:::SRK.II.Power(L = L, K = 337.5/R, A = A)
  expect_equivalent(round(iol, digits=2), P)
  
  L <- 20.5; R <- 7.0; A <- 118.0; P <- 25.36
  iol <- IOL:::SRK.II.Power(L = L, K = 337.5/R, A = A)
  expect_equivalent(round(iol, digits=2), P)
  
  L <- 21.0; R <- 7.0; A <- 118.0; P <- 23.11
  iol <- IOL:::SRK.II.Power(L = L, K = 337.5/R, A = A)
  expect_equivalent(round(iol, digits=2), P)
  
  L <- 21.5; R <- 7.0; A <- 118.0; P <- 21.86
  iol <- IOL:::SRK.II.Power(L = L, K = 337.5/R, A = A)
  expect_equivalent(round(iol, digits=2), P)
  
  L <- 22.0; R <- 7.0; A <- 118.0; P <- 19.61
  iol <- IOL:::SRK.II.Power(L = L, K = 337.5/R, A = A)
  expect_equivalent(round(iol, digits=2), P)
  
  L <- 24.0; R <- 7.0; A <- 118.0; P <- 14.61
  iol <- IOL:::SRK.II.Power(L = L, K = 337.5/R, A = A)
  expect_equivalent(round(iol, digits=2), P)
  
  L <- 24.5; R <- 7.0; A <- 118.0; P <- 12.86
  iol <- IOL:::SRK.II.Power(L = L, K = 337.5/R, A = A)
  expect_equivalent(round(iol, digits=2), P)
  
  L <- 32.0; R <- 7.0; A <- 118.0; P <- -5.89
  iol <- IOL:::SRK.II.Power(L = L, K = 337.5/R, A = A)
  expect_equivalent(round(iol, digits=2), P)
})
