testthat::test_that("function getBinaryPredictions", {
  # setup test
  testPredictions <- readRDS(testthat::test_path("testdata", "test-mapOperations.rds"))
  testExpectedColnames <- c("Longitude", "Latitude", "Est", "Sd", "SDPop", "SdTotal", "IntLower",
                            "IntUpper", "IntLowerTotal", "IntUpperTotal", "resError")

  # test the calculation
  testXPredResult <- getBinaryPredictions(testPredictions$testXPred1, testPredictions$testXPred2)
  expect_true(is.data.frame(testXPredResult))
  expect_true(nrow(testXPredResult) > 0)
  expect_equal(colnames(testXPredResult), testExpectedColnames)

  # test plotting of the map
  testInputs <- list(XPred = testXPredResult,
                     type = "difference",
                     independent = "")

  # we cannot test the output, but we can test if the function runs without error
  expect_true(is.list(do.call(plotDS, testInputs)))
})
