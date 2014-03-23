#
# Check normal function
#
# Test the Haigis function
elp <- ELP(pACD = 5.2, which = 'Haigis')

# Check the output class
expect_is(elp, 'ELP')

# Check the output value
expect_equivalent(unclass(elp), 5.2)
expect_match(names(elp), 'Haigis')

# Check the recorded input parameters
expect_equal(attr(elp, 'parameters'),
             list(Haigis = list(pACD = 5.2)))

# Check the recorded function call
expect_equal(attr(elp, 'function'),
             'Haigis')
expect_match(attr(elp, 'function.arguments'),
             'pACD')

#
# Check abnormal function
#
# Check warning for negative pACD
expect_warning(ELP(pACD = -1, which = 'Haigis'))

# Check error for missing arguments
expect_error(ELP(which = 'Haigis'))

# Check error for incorrect argument type
expect_error(ELP(pACD = NA, which = 'Haigis'))
expect_error(ELP(pACD = NULL, which = 'Haigis'))
expect_error(ELP(pACD = 'foobar', which = 'Haigis'))