context("Holladay functions")

# Test 
a <- ELP(A = 118.4, which = 'Holladay')
expect_that(unclass(a), is_equivalent_to(5.198688))
