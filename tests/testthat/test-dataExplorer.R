testthat::test_that("convertLatLongWrapper", {
  testData <- structure(
    list(
      ID = c(1, 2, 3, 4, 5, 6),
      Site = c("514", "514",
               "514", "514", "514", "514"),
      Latitude = c(2.34, 2.34, 2.34, 2.34,
                   2.34, 2.34),
      Longitude = c(36.28, 36.28, 36.28, 36.28, 36.28,
                    36.28)
    ),
    row.names = c(NA, 6L),
    class = "data.frame"
  )

  convertedData <- convertLatLongWrapper(data = testData,
                                         Longitude = "Longitude",
                                         Latitude = "Latitude",
                                         CoordType = "decimal degrees",
                                         showMessage = FALSE)
  testthat::expect_equal(names(convertedData),
                         c("longitude", "latitude", "ID", "Site", "Latitude", "Longitude", "id"))
  testthat::expect_equal(convertedData$latitude, c(2.34, 2.34, 2.34, 2.34, 2.34, 2.34))
  testthat::expect_equal(convertedData$longitude, c(36.28, 36.28, 36.28, 36.28, 36.28, 36.28))
})
