## tests for single release tag recapture models


## add some more of these
# test_that("test petersen estimates", {
#   expect_equal(chapman_n(99, 49, 9)["N_hat"], 501)
# })


#N_hat <- ((tags + 1)*(catch + 1))/(recaps + 1) + 1 
# 
#library(tagr)
# chapman_n(tags=100, catch=100, recaps=10)


test_that("test srelease inputs", {
   expect_warning(check_srelease_inputs(-1, 10, 1), "releases, catch and recaptures must not be negative")
})

 