test_that("Entering NA parameters returns Null distribution by default.", {
  ## Loop over each parameter of each distribution in the test set,
  ## replacing it with NA (or dst_null() if the parameter is a distribution)
  ## and checking that the resulting distribution is dst_null().
  for (i in seq_along(test_distributions)) {
    fn <- names(test_distributions)[i]
    for (paramset in test_distributions[[i]]) {
      for (j in seq_along(paramset)) {
        this_paramset <- paramset
        if (is_distribution(this_paramset[[j]])) {
          this_paramset[[j]] <- distionary::dst_null()
        } else {
          this_paramset[[j]] <- NA
        }
        d <- rlang::exec(fn, !!!this_paramset)
        expect_equal(d, dst_null())
      }
    }
  }
})
