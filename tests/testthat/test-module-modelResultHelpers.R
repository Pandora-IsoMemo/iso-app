test_that("Test extractCenterEstimates", {
  testXPred <- data.frame(
    row.names = c("51","52","53","54","55","152",
                  "153","154","155","252","253","254"),
    Longitude = c(37.3871681818182,38.5508045454545,
                  39.7144409090909,40.8780772727273,42.0417136363636,
                  38.5508045454545,39.7144409090909,40.8780772727273,
                  42.0417136363636,38.5508045454545,39.7144409090909,40.8780772727273),
    Latitude = c(21.42805,21.42805,21.42805,
                 21.42805,21.42805,22.0098681818182,22.0098681818182,
                 22.0098681818182,22.0098681818182,22.5916863636364,22.5916863636364,
                 22.5916863636364),
    Est = c(-22.1723727867234,-22.165966477672,
            -22.1463965952142,-22.1159040604181,-22.0769062357905,
            -22.1275594045224,-22.1041874655608,-22.0696335795751,
            -22.0265576911994,-22.0878412486116,-22.0604212634462,
            -22.0215740644906),
    Sd = c(1.56688586739737,1.58889281591481,
           1.6125211344271,1.63823111373394,1.6662454189694,
           1.55037151953542,1.57399815054345,1.60000460582804,
           1.62865313242603,1.51126626275399,1.53485528133388,1.56117531276744),
    SDPop = c(1.35906035676739,1.35906035676739,
              1.35906035676739,1.35906035676739,1.35906035676739,
              1.35906035676739,1.35906035676739,1.35906035676739,
              1.35906035676739,1.35906035676739,1.35906035676739,1.35906035676739),
    SdTotal = c(2.07416883950808,2.09084323510932,
                2.10885501215962,2.12857845411971,2.15021367532932,
                2.06172182942396,2.0795468812342,2.09929983375592,
                2.12121570781894,2.03247897166861,2.05007945894182,2.06985830687301),
    IntLower = c(-25.2434690868222,-25.280196396865,
                 -25.3069380186913,-25.3268370433367,-25.3427472569706,
                 -25.1662875828118,-25.189223840626,-25.2056426069981,
                 -25.2187178307544,-25.0499231236094,-25.0687376148606,
                 -25.0814776775148),
    IntUpper = c(-19.1012764866245,-19.051736558479,
                 -18.9858551717371,-18.9049710774996,-18.8110652146105,
                 -19.0888312262329,-19.0191510904957,-18.9336245521521,
                 -18.8343975516443,-19.1257593736138,-19.0521049120318,
                 -18.9616704514664),
    IntLowerTotal = c(-26.2377437121592,-26.2640192184863,
                      -26.2797524190471,-26.2879178304928,-26.291325039436,
                      -26.1685341901933,-26.1800993527798,-26.1842612537367,
                      -26.1841404785245,-26.0715000330821,-26.0785770029722,
                      -26.0784963459617),
    IntUpperTotal = c(-18.1070018612875,-18.0679137368577,
                      -18.0130407713814,-17.9438902903435,-17.8624874321451,
                      -18.0865846188514,-18.0282755783418,-17.9550059054135,
                      -17.8689749038742,-18.1041824641411,-18.0422655239203,
                      -17.9646517830195),
    resError = c(1.3635350248462,1.3635350248462,
                 1.3635350248462,1.3635350248462,1.3635350248462,
                 1.3635350248462,1.3635350248462,1.3635350248462,1.3635350248462,
                 1.3635350248462,1.3635350248462,1.3635350248462),
    EstForCenter = c(-22.1723727867234,-22.165966477672,
            -22.1463965952142,-22.1159040604181,-22.0769062357905,
            -22.1275594045224,-22.1041874655608,-22.0696335795751,
            -22.0265576911994,-22.0878412486116,-22.0604212634462,
            -22.0215740644906)
  )

  expect_equal(extractGridLength(latitude = testXPred$Latitude,
                                 longitude = testXPred$Longitude), c(latitude = 0.58182, longitude = 1.1636))

  testRes <- testXPred %>%
    extractXPredCenter(centerX = 39, centerY = 22, Radius = 1)
  expect_equal(testRes$Est,
               c(-22.165966477672, -22.1463965952142, -22.1275594045224, -22.1041874655608,
                 -22.0878412486116, -22.0604212634462))

  testRes <- testXPred %>%
    extractXPredCenter(centerX = 39, centerY = 22, Radius = 1) %>%
    extractCenterEstimates()
  expect_equal(testRes$mean, -22.115)
  expect_equal(testRes$sd, 1.6009)

  testRes <- testXPred %>%
    extractXPredCenter(centerX = NA, centerY = NA, Radius = 100/111)
  expect_equal(testRes$Est, rep(NA_real_, 12))

  testRes <- testXPred %>%
    extractXPredCenter(centerX = NA, centerY = NA, Radius = 100/111) %>%
    extractCenterEstimates()
  expect_equal(testRes$mean, NA_real_)
  expect_equal(testRes$sd, NA_real_)
})

test_that("Test extractZoomFromLongRange", {
  centering <- "Europe"

  expect_equal(extractZoomFromLongRange(rangeLongitude = c(13, 97), mapCentering = centering), 86)
  expect_equal(extractZoomFromLongRange(rangeLongitude = c(3, 9), mapCentering = centering), 8)
  expect_equal(extractZoomFromLongRange(rangeLongitude = c(-10, 84), mapCentering = centering), 96)
  expect_equal(extractZoomFromLongRange(rangeLongitude = c(-60, 50), mapCentering = centering), 112)

  centering <- "Pacific"

  expect_equal(extractZoomFromLongRange(rangeLongitude = c(13, 97), mapCentering = centering), 86)
  expect_equal(extractZoomFromLongRange(rangeLongitude = c(3, 9), mapCentering = centering), 8)
  expect_equal(extractZoomFromLongRange(rangeLongitude = c(-10, 84), mapCentering = centering), 268)
  expect_equal(extractZoomFromLongRange(rangeLongitude = c(-60, 50), mapCentering = centering), 112)
  })


test_that("Test getZvalues", {
  testModel <- readRDS(testthat::test_path("averageR_testModel_numeric.rds"))

  expect_equal(getZvalues(estimationType = "Mean", model = testModel, mapType = "Map", IndSelect = ""),
               list(minInput = list(value = 6.1, min = 6.1, max = 15), maxInput = list(
                 value = 15, min = 6.1, max = 15)))

  testModel <- readRDS(testthat::test_path("averageR_testModel_categorical.rds"))

  expect_equal(getZvalues(estimationType = "Mean", model = testModel, mapType = "Map", IndSelect = ""),
               list(minInput = list(value = 0, min = 0, max = 1), maxInput = list(
                 value = 1, min = 0, max = 1)))

  expect_equal(getZvalues(estimationType = "Mean", model = testModel, mapType = "Map", IndSelect = "expert"),
               list(minInput = list(value = -0.09, min = -0.09, max = 1.1),
                    maxInput = list(value = 1.1, min = -0.09, max = 1.1)))
})

test_that("Test getDefaultZ", {
  expect_equal(getDefaultZMin(c(4, 5)), 3.9)
  expect_equal(getDefaultZMin(c(4, 50000)), -5000)
  expect_equal(getDefaultZMin(c(4, 4.0001)), 3.99999)
  expect_equal(getDefaultZMin(c(4, 4)), 3.9996)

  expect_equal(getDefaultZMax(c(4, 5)), 5.1)
  expect_equal(getDefaultZMax(c(4, 4.0001)), 4.00011)
  expect_equal(getDefaultZMax(c(4, 50000)), 55000)
  expect_equal(getDefaultZMax(c(4, 4)), 4.0004)
})
