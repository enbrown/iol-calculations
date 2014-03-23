context("Binkhorst functions")

#
# Test a number of powers
#
test_that("Binkhorst power calculations for a range of values", {
  expect_equivalent(IOL:::Binkhorst.Power(L = 22, R = 6.0, ELP = 3), 1336*2/(19*21))
  expect_equivalent(IOL:::Binkhorst.Power(L = 24, R = 6.5, ELP = 2), 1336*2/(22*24))
  expect_equivalent(IOL:::Binkhorst.Power(L = 24, R = 6.5, ELP = 4), 1336*2/(20*22))
  expect_equivalent(IOL:::Binkhorst.Power(L = 24, R = 6.5, ELP = 6), 1336*2/(18*20))
  expect_equivalent(IOL:::Binkhorst.Power(L = 24, R = 7.5, ELP = 2), 1336*6/(22*28))
  expect_equivalent(IOL:::Binkhorst.Power(L = 24, R = 7.5, ELP = 4), 1336*6/(20*26))
  expect_equivalent(IOL:::Binkhorst.Power(L = 24, R = 7.5, ELP = 6), 1336*6/(18*24))
  expect_equivalent(IOL:::Binkhorst.Power(L = 30, R = 6.0, ELP = 6), -1336*6/(24*18))
})
test_that("Adjusted-Binkhorst power calculations for a range of values", {
  expect_equivalent(IOL:::Binkhorst.adjusted.Power(L = 23.45, R = 7.1125, ELP = 3.45), 1336*5/(20*25))
  expect_equivalent(IOL:::Binkhorst.adjusted.Power(L = 22, R = 6, ELP = 2), 5.932323175)
  expect_equivalent(IOL:::Binkhorst.adjusted.Power(L = 22, R = 7.7, ELP = 2), 19.99149918)
  expect_equivalent(IOL:::Binkhorst.adjusted.Power(L = 22, R = 9, ELP = 2), 26.97542793)
  expect_equivalent(IOL:::Binkhorst.adjusted.Power(L = 22, R = 6, ELP = 4), 7.232807799)
  expect_equivalent(IOL:::Binkhorst.adjusted.Power(L = 22, R = 7.7, ELP = 4), 23.82311167)
  expect_equivalent(IOL:::Binkhorst.adjusted.Power(L = 22, R = 9, ELP = 4), 31.78866859)
  expect_equivalent(IOL:::Binkhorst.adjusted.Power(L = 22, R = 6, ELP = 6), 9.013574807)
  expect_equivalent(IOL:::Binkhorst.adjusted.Power(L = 22, R = 7.7, ELP = 6), 28.89231022)
  expect_equivalent(IOL:::Binkhorst.adjusted.Power(L = 22, R = 9, ELP = 6), 38.06269648)
})

#
# Test the Binkhorst function directly
#
test_that("Binkhorst power function (internal function)", {
  iol <- IOL:::Binkhorst.Power(L = 24, R = 9, ELP = 4)
  
  # Check the output class
  expect_is(iol, 'numeric')
  
  # Check the output value
  expect_equivalent(iol, 25.05)
  
  # Check the recorded input parameters
  expect_equivalent(attr(iol, 'parameters'),
                    list(L = 24, ELP = 4, R = 9))
})

#
# Check normal function
#
test_that("Binkhorst power function (public function)", {
  # Test the Binkhorst function
  expect_warning(iol <- Power(L = 24, K = 45, ELP = 3, which = 'Binkhorst'))         

  # Check the output class
  expect_is(iol, 'Power')
  
  # Check the output value
  expect_equivalent(unclass(iol), 13.44936751)
  expect_match(names(iol), 'Binkhorst')
  
  # Check the recorded input parameters
  expect_equivalent(attr(iol, 'parameters'),
               list(Binkhorst = list(L = 24,
                                     ELP = 3,
                                     K = 45,
                                     cornea_n = 4/3)))
  
  # Check the recorded function call
  expect_equal(attr(iol, 'function'),
               'Binkhorst')
  expect_match(attr(iol, 'function.arguments'),
               'L, ELP, K, cornea_n')
})

#
# Test the Adjusted Binkhorst function directly
#
test_that("Adjusted Binkhorst power function (internal function)", {
  iol <- IOL:::Binkhorst.adjusted.Power(L = 24, R = 7, ELP = 4)
  
  # Check the output class
  expect_is(iol, 'numeric')
  
  # Check the output value
  expect_equivalent(iol, 11.2293739)
  
  # Check the recorded input parameters
  expect_equivalent(attr(iol, 'parameters'),
                    list(L = 24, ELP = 4, R = 7))
})

#
# Check normal function
#
test_that("Adjusted Binkhorst power function (public function)", {
  # Test the Binkhorst function
  expect_warning(iol <- Power(L = 24, K = 45, ELP = 3, which = 'Binkhorst.adjusted'))
  
  # Check the output class
  expect_is(iol, 'Power')
  
  # Check the output value
  expect_equivalent(unclass(iol), 13.55711778)
  expect_match(names(iol), 'Binkhorst.adjusted')
  
  # Check the recorded input parameters
  expect_equivalent(attr(iol, 'parameters'),
                    list(Binkhorst = list(L = 24,
                                          ELP = 3,
                                          K = 45,
                                          cornea_n = 4/3)))
  
  # Check the recorded function call
  expect_equal(attr(iol, 'function'),
               'Binkhorst.adjusted')
  expect_match(attr(iol, 'function.arguments'),
               'L, ELP, K, cornea_n')
})

#
# Check warnings and errors
#
test_that("Binkhorst and Adjusted Binkhorst warnings and errors", {
  # Warning to state that R is computed
  expect_warning(IOL:::Binkhorst.Power(L = 24, K = 45, ELP = 3))
  expect_warning(IOL:::Binkhorst.adjusted.Power(L = 24, K = 45, ELP = 3))
  expect_warning(Power(L = 24, K = 45, ELP = 3, which = 'Binkhorst'))
  expect_warning(Power(L = 24, K = 45, ELP = 3, which = 'Binkhorst.adjusted'))
  
  # Errors for missing required arguments
  expect_error(IOL:::Binkhorst.Power(K = 45, ELP = 3))
  expect_error(IOL:::Binkhorst.Power(L = 24, ELP = 3))
  expect_error(IOL:::Binkhorst.Power(L = 24, K = 45))
  expect_error(IOL:::Binkhorst.adjusted.Power(K = 45, ELP = 3))
  expect_error(IOL:::Binkhorst.adjusted.Power(L = 24, ELP = 3))
  expect_error(IOL:::Binkhorst.adjusted.Power(L = 24, K = 45))
  expect_error(Power(K = 45, ELP = 3, which = 'Binkhorst'))
  expect_error(Power(K = 45, ELP = 3, which = 'Binkhorst.adjusted'))
  expect_error(Power(L = 24, ELP = 3, which = 'Binkhorst'))
  expect_error(Power(L = 24, ELP = 3, which = 'Binkhorst.adjusted'))
  expect_error(Power(L = 24, K = 45, which = 'Binkhorst'))
  expect_error(Power(L = 24, K = 45, which = 'Binkhorst.adjusted'))
})