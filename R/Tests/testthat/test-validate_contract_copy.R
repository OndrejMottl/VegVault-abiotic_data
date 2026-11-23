#----------------------------------------------------------#
# Copy of the VegVault contracts -----
#----------------------------------------------------------#

# The standard producer check from VegVault's Data/Contracts/README.md: the
#   copy is unchanged and, when the sibling VegVault checkout exists, equals
#   what save_contract_copy() would copy from that checkout now.

testthat::test_that("the VegVault contract copy is intact and current", {
  testthat::expect_no_error(
    validate_contract_copy(
      path_producer = here::here(),
      path_vegvault = here::here("..", "VegVault"),
      verbose = FALSE
    )
  )
})
