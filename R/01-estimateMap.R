# Prevent Errors in R CMD check in line
# s <- smoothCon(s(Longitude, Latitude, k = nknots, bs = "tp"),
#                data = data, knots = NULL)[[1]]
#
# s <- smoothCon(s(Longitude2, Latitude2, Date2, m = 3, k = nknots, bs = "tp"),
#                data = data, knots = NULL)[[1]]
# s2 <- smoothCon(s(Longitude2, Latitude2, Date3, m = 3, k = nknots, bs = "tp"),
#                 data = data, knots = NULL)[[1]]
utils::globalVariables(c("Longitude", "Latitude", "Longitude2", "Latitude2", "Date2", "Date3"))

#' Estimates spatial average model with (optional) random effects (GAMM /Generalized Additive Mixed Model)
#'
#' Note regarding IndependentType = "categorical": This follows a one vs. all approach using
#' logistic regression, which in the Bayesian case is performed using a Polya-Gamma latent variable
#' during Gibbs-sampling (https://arxiv.org/abs/1205.0310).
#'
#' @param data data.frame: data
#' @param independent character: name of independent variable
#' @param IndependentType character: type ("numeric" or "categorical") of independent variable
#' @param Longitude character: name of longitude variable
#' @param Latitude character: name of latitude variable
#' @param Site character: name of site variable (optional)
#' @param independentUncertainty character: uncertainty of independent variable in sd (optional)
#' @param burnin integer: number of burn-in iterations for Bayesian model (default = 500)
#' @param iter integer: number of iterations for Bayesian model (default = 2000)
#' @param nChains integer: number of chains for Bayesian model (default = 1)
#' @param K integer: number of basis functions for tprs (thin plate regression spline)
#' @param Bayes boolean: Bayesian model TRUE/FALSE?
#' @param CoordType character: type of longitude/latitude coordinates.
#'  One of "decimal degrees", "degrees minutes seconds" and "degrees decimal minutes"
#' @param smoothConst numeric: adjust smoothing parameter (> 0) for Bayesian model (optional)
#' @param splineType numeric: 1 for classical tprs, 2 for spherical spline
#' @param penalty numeric: 1 for constant extrapolation, 2 for linear extrapolation
#' @param outlier boolean: model outlier removal TRUE/FALSE
#' @param outlierValue numeric: if outlier removal is TRUE, threshold for removals in sd
#' @param outlierD boolean: data outlier removal TRUE/FALSE
#' @param outlierValueD numeric: if outlierD removal is TRUE, threshold for removals in sd
#' @param restriction numeric vector: spatially restricts model data 4 entries for latitude (min/max) and longitude(min/max)
#' @param correctionPac boolean: correction (data augmentation) for pacific centering
#' @param sdVar boolean: variable standard deviation
#' @param thinning numeric: mcmc thinning for bayesian models
#' @inheritParams centerData
#' @examples
#' \dontrun{
#' #load data
#' data <- readRDS(system.file("extData", "exampleData.Rds", package = "MpiIsoApp"))
#' # estimate model-map
#' map <- estimateMap(data = data, independent = "d13C", Longitude = "longitude",
#' Latitude = "latitude", Site = "site")
#' # Plot the map
#' plotMap(model = map)
#'
#' # Alternative: use app
#' shiny::runApp(paste0(system.file(package = "MpiIsoApp"),"/app"))
#'
#' }
#' @export
estimateMap <- function(data,
                        independent,
                        Longitude,
                        Latitude,
                        center = c("Europe", "Pacific"),
                        IndependentType = "numeric",
                        Site = "",
                        independentUncertainty = "",
                        burnin = 500,
                        iter = 2000,
                        nChains = 1,
                        K = 50,
                        Bayes = FALSE,
                        CoordType = "decimal degrees",
                        smoothConst = 1,
                        penalty = 2,
                        splineType = 2,
                        outlier = FALSE,
                        outlierValue = 4,
                        outlierD = FALSE,
                        outlierValueD = 4,
                        restriction = c(-90, 90, -180, 180),
                        correctionPac = FALSE,
                        sdVar = FALSE,
                        thinning = 2){
  set.seed(1234)
  center <- match.arg(center)

  dataOrg <- data
  if (is.null(data)) return(NULL)
  if (Longitude == "" || Latitude == "") return(NULL)
  if (!(all(c(Longitude, Latitude, independent) %in% names(data)))) return(NULL)

  # prepare data ----
  data <- data %>%
    convertLatLongWrapper(Longitude = Longitude,
                          Latitude = Latitude,
                          CoordType = CoordType)
  # if conversion fails Long/Lat are removed -> columns will be missing
  if (!all(c(Longitude, Latitude) %in% names(data)) ||
      all(is.na(data[, Longitude])) || all(is.na(data[, Latitude])) )
    return("Longitude or Latitude not available.")

  # process coordinate data
  data <- data %>%
    shiftDataToDefaultRestriction() %>%
    removeDataOutsideRestriction(Latitude = Latitude,
                                 Longitude = Longitude,
                                 restriction = restriction)

  if ( (!is.numeric(data[, independent]) || all(is.na(data[, independent]))) & IndependentType == "numeric")
    return("non-numeric independent variable")

  if ( Site != "" && all(is.na(data[, Site]))) return("wrong site variable")
  if ( Site == ""){
    data$Site = 1:nrow(data)
    Site = "Site"
  }
  data$Site <- data[, Site]

  if(independentUncertainty != "" && !all(is.na(data[, independentUncertainty]))){
    data$independentUncertainty <- data[, independentUncertainty]
    data$independentUncertainty[is.na(data$independentUncertainty)] <- 0
    data <- na.omit(data[, c(independent, Longitude, Latitude, "Site", "independentUncertainty")])
  } else {
    data <- na.omit(data[, c(independent, Longitude, Latitude, "Site")])
  }
  if (nrow(unique(data[, c(Longitude, Latitude)])) <= K) {
    K <- ceiling(0.9 * nrow(unique(data[, c(Longitude, Latitude)])))
    if (K < 4) {return("less than 4 rows")}
  }
  independentnew <- independent
  if (grepl("[^a-zA-Z]", substr(independent,1,1))){
    independentnew <- paste0("x", independent)
  }
  if (grepl("[^a-zA-Z0-9._]", independent)){
    independentnew <- gsub("[^a-zA-Z0-9._]", "", independentnew)
  }
  data$independentnew <- data[, independent]
  names(data)[names(data) == "independentnew"] <- independentnew
  independent <- independentnew
  data$independentnew <- NULL

  data$Longitude <- data[, Longitude]
  data$Latitude <- data[, Latitude]
  if(outlierD == TRUE & IndependentType == "numeric"){
    moD <- mean(data[, independent], na.rm = TRUE)
    soD <- sd(data[, independent], na.rm = TRUE)
    data <- data[data[, independent] >= moD - outlierValueD * soD, ]
    data <- data[data[, independent] <= moD + outlierValueD * soD, ]
  }

  ### data augmentation ----
  if (correctionPac & splineType == 1 & center == "Europe") {
    data2 <- augmentData(data)
    K <- ceiling(K * nrow(data2) / nrow(data))
  } else {
    data2 <- data
  }

  ### data centering ----
  data2 <- centerData(data2, center = center)

  # calculate model ----
  if (splineType == 2){
    bs = "\"sos\""
  } else {
    bs = "\"ds\""
  }
  if (Bayes == FALSE){
    #outlier
    sc <- NULL
    scV <- NULL
    if(IndependentType == "numeric"){
      fm = "gaussian"
      model <- estimateModel2D(data2, fm, independent, penalty, K, bs)
      if ( class(model)[1] == "try-error") {return("Error in Model Fitting.")}

      if(outlier == TRUE){
        data2 <- dataOrg[as.numeric(rownames(data2)[which(abs(scale(residuals(model$lme))) < outlierValue)]), ]
        return(list(model = model, data = data2, sc = sc, scV = scV, independent = independent, IndependentType = IndependentType))

      }
      predRange <- predict(model$gam, se.fit = TRUE, type = "response")
      model$range <- list(mean = range(predRange$fit), se = range(predRange$se.fit),
                          seTotal = sqrt(range(var(residuals(model$gam)) + max(predRange$se.fit)^2)))
    } else {
      fm = "binomial"
      dummy_matrix <- model.matrix(~ . -1, data = data2[,independent, drop = FALSE])
      colnames(dummy_matrix) <- sapply(strsplit(colnames(dummy_matrix), split = independent), function(x) x[2])
      data2 <- cbind(data2, dummy_matrix)
      model <- lapply(colnames(dummy_matrix), function(x){
        model <- estimateModel2D(data2, fm, x, penalty, K, bs)
        if ( class(model)[1] == "try-error") {return("Error in Model Fitting.")}
        if(outlier == TRUE){
          data2 <- dataOrg[as.numeric(rownames(data2)[which(abs(scale(residuals(model$lme))) < outlierValue)]), ]
          return(list(model = model, data = data2, sc = sc, scV = scV, independent = independent, IndependentType = IndependentType))

        }
        predRange <- predict(model$gam, se.fit = TRUE, type = "response")
        model$range <- list(mean = range(predRange$fit), se = range(predRange$se.fit),
                            seTotal = sqrt(range(var(residuals(model$gam)) + max(predRange$se.fit)^2)))
        model
        })
      names(model) <- colnames(dummy_matrix)
    }
  } else { # Bayes == TRUE
    if(IndependentType == "numeric"){
      model <- try(modelLocalAvgMC(data = data2, K = K, iter = iter, burnin = burnin,
                                   independent = independent, smoothConst = smoothConst,
                                   penalty = penalty, splineType = splineType, IndependentType = IndependentType,
                                   sdVar = sdVar, nChains = nChains, thinning = thinning), silent = TRUE)
      if ( class(model)[1] == "try-error") {return("Error in Model Fitting.")}
      sRe = model$sRe
      mRe = model$mRe
      sc = model$sc
      scV = model$scV
      } else{
        dummy_matrix <- model.matrix(~ . -1, data = data2[,independent, drop = FALSE])
        colnames(dummy_matrix) <- sapply(strsplit(colnames(dummy_matrix), split = independent), function(x) x[2])
        data2 <- cbind(data2, dummy_matrix)
        model <- lapply(colnames(dummy_matrix), function(x){
                  model <- try(modelLocalAvgMC(data = data2, K = K, iter = iter, burnin = burnin,
                                       independent = x, smoothConst = smoothConst,
                                       penalty = penalty, splineType = splineType, IndependentType = IndependentType,
                                       sdVar = sdVar, nChains = nChains, thinning = thinning), silent = TRUE)
                  if ( class(model)[1] == "try-error") {return("Error in Model Fitting.")}
                  model
        })
        names(model) <- colnames(dummy_matrix)
        sRe = 1
        mRe = 0
        sc = model[[1]]$sc
        scV = model[[1]]$scV
    }

    return(list(model = model, data = data2, sc = sc, scV = scV,
                independent = independent, mRe = mRe, sRe = sRe,
                nChains = nChains, IndependentType = IndependentType))
  }

  return(list(model = model, data = data, sc = sc, scV = scV,
              independent = independent,
              nChains = nChains, IndependentType = IndependentType))
}

estimateMapWrapper <- function(data, input) {
  if(input$modelArea){
    restriction <- c(input$mALat1, input$mALat2, input$mALong1, input$mALong2)
    restriction[is.na(restriction)] <- c(-90, 90, -180, 180)[is.na(restriction)]
  } else {
    restriction <- c(-90, 90, -180, 180)
  }


  if(input$Outlier == TRUE){
    withProgress({
      dataOld <- data
      dataD <- data
      moD <- mean(dataD[, input$IndependentX], na.rm = TRUE)
      soD <- sd(dataD[, input$IndependentX], na.rm = TRUE)
      dataD <- dataD[dataD[, input$IndependentX] >= moD - input$OutlierValueD * soD, ]
      dataD <- dataD[dataD[, input$IndependentX] <= moD + input$OutlierValueD * soD, ]
      outlierDR <- rownames(dataOld)[which(!(rownames(dataOld) %in% rownames(dataD)))]
      rm(dataD)
      data <- estimateMap(
        data = data, Bayes = FALSE, independent = input$IndependentX,
        independentUncertainty = input$IndependentUnc,
        IndependentType = input$IndependentType,
        Longitude = input$Longitude, Latitude = input$Latitude, center = input$centerOfData,
        Site = input$Site, CoordType = input$coordType,
        penalty = as.numeric(input$Penalty),
        splineType = as.numeric(input$SplineType),
        iter = input$Iter, burnin = input$burnin,
        nChains = input$nChains, K = input$Smoothing,
        smoothConst = input$smoothConst,
        correctionPac = input$correctionPac,
        restriction = restriction,
        outlier = TRUE,
        outlierValue = input$OutlierValue,
        outlierD = input$OutlierD,
        outlierValueD = input$OutlierValueD,
        thinning = input$thinning)$data
      },
      value = 0,
      message = "Removing outliers"
    )
    outlier <- rownames(dataOld)[which(!(rownames(dataOld) %in% rownames(data)))]
  } else {
    outlier <- character()
    outlierDR <- character()
  }

  if(input$Bayes != TRUE){
    withProgress(
      model <- estimateMap(
        data = data, Bayes = input$Bayes, independent = input$IndependentX,
        independentUncertainty = input$IndependentUnc,
        IndependentType = input$IndependentType,
        Longitude = input$Longitude, Latitude = input$Latitude, center = input$centerOfData,
        Site = input$Site, CoordType = input$coordType,
        penalty = as.numeric(input$Penalty),
        restriction = restriction,
        correctionPac = input$correctionPac,
        splineType = as.numeric(input$SplineType),
        iter = input$Iter, burnin = input$burnin,
        nChains = input$nChains, K = input$Smoothing,
        smoothConst = input$smoothConst,
        outlierD = input$OutlierD,
        outlierValueD = input$OutlierValueD,
        thinning = input$thinning
      ),
      value = 0,
      message = "Generating local average model"
    )
  } else {
    model <- estimateMap(
      data = data, Bayes = input$Bayes, independent = input$IndependentX,
      independentUncertainty = input$IndependentUnc,
      IndependentType = input$IndependentType,
      Longitude = input$Longitude, Latitude = input$Latitude, center = input$centerOfData,
      Site = input$Site, CoordType = input$coordType,
      penalty = as.numeric(input$Penalty),
      restriction = restriction,
      correctionPac = input$correctionPac,
      splineType = as.numeric(input$SplineType),
      iter = input$Iter, burnin = input$burnin,
      nChains = input$nChains,
      K = input$Smoothing, smoothConst = input$smoothConst,
      outlierD = input$OutlierD,
      outlierValueD = input$OutlierValueD,
      sdVar = input$sdVar,
      thinning = input$thinning
    )
  }
  if(class(model) == "list"){
    model$outlier <- outlier
    model$outlierDR <- outlierDR
  }

  model
}

#' Estimates spatial spread model (first or latest occurence of event)
#'
#' @param data data.frame: data
#' @param Longitude character: name of longitude variable
#' @param Latitude character: name of latitude variable
#' @param MinMax character: estimate minimum or maximum of distribution. choices: "Max", "Min"
#' @param DateOne character: name of date variable 1 (lower interval point / mean / single point)
#' @param DateTwo character: name of date variable 2 (upper interval point / sd / )
#' @param DateType character: one of "Interval", "Mean + 1 SD uncertainty" and "Single Point"
#' @param dateUnc character: one of "uniform", "normal", "point"
#' @param burnin integer: number of burn-in iterations for Bayesian model (default = 500)
#' @param iter integer: number of iterations for Bayesian model (default = 2000)
#' @param nChains integer: number of chains for Bayesian model (default = 1)
#' @param K integer: number of basis functions for tprs (thin plate regression spline)
#' @param CoordType character: type of longitude/latitude coordinates.
#'  One of "decimal degrees", "degrees minutes seconds" and "degrees decimal minutes"
#' @param smoothConst numeric: adjust smoothing parameter for Bayesian model (optional)
#' @param splineType numeric: 1 for classical tprs, 2 for spherical spline
#' @param penalty numeric: 1 for constant extrapolation, 2 for linear extrapolation
#' @param shinyApp boolean: If called inside shinyApp: Set to true
#' @param outlier boolean: outlier removal TRUE/FALSE
#' @param outlierValue numeric: if outlier removal is TRUE, threshold for removals in sd
#' @param outlierD boolean: data outlier removal TRUE/FALSE
#' @param outlierValueD numeric: if outlierD removal is TRUE, threshold for removals in sd
#' @param restriction numeric vector: spatially restricts model data 4 entries for latitude (min/max) and longitude(min/max)
#' @param correctionPac boolean: correction (data augmentation) for pacific centering
#' @param thinning numeric: mcmc thinning for bayesian models
#' @param spreadQ numeric: exceedance quantile as buffer
#' @param minValue numeric: minValue restriction
#' @inheritParams centerData
#' @examples
#' \dontrun{
#' # load data
#' data <- readRDS(system.file("extData", "exampleData.Rds", package = "MpiIsoApp"))
#' # estimate model-map
#' map <- estimateMapSpread(data = data, Longitude = "longitude",
#' Latitude = "latitude", DateOne = "dateLower", DateTwo = "dateUpper", iter = 200)
#' # Plot the map
#' plotMap(model = map)
#' }
#'
#' @export
estimateMapSpread <- function(data,
                              Longitude,
                              Latitude,
                              DateOne,
                              DateTwo,
                              center = c("Europe", "Pacific"),
                              burnin = 500,
                              iter = 2000,
                              nChains = 1,
                              K = 50,
                              MinMax = "Max",
                              DateType = "Interval",
                              dateUnc = "mid point",
                              CoordType = "decimal degrees",
                              smoothConst = 1,
                              penalty = 1,
                              splineType = 2,
                              shinyApp = FALSE,
                              outlier = FALSE,
                              outlierValue = 4,
                              outlierD = FALSE,
                              outlierValueD = 4,
                              restriction = c(-90, 90, -180, 180),
                              correctionPac = FALSE,
                              thinning = 2,
                              spreadQ = 0.01,
                              minValue = -Inf){
  set.seed(1234)
  center <- match.arg(center)

  dataOrg <- data
  if (is.null(data)) return(NULL)
  if (Longitude == "" || Latitude == "" || DateOne == "") return(NULL)
  if (!(all(c(Longitude, Latitude, DateOne) %in% names(data)))) return(NULL)

  # prepare data ----
  data <- data %>%
    convertLatLongWrapper(Longitude = Longitude,
                          Latitude = Latitude,
                          CoordType = CoordType)
  # if conversion fails Long/Lat are removed -> columns will be missing
  if (!all(c(Longitude, Latitude) %in% names(data)) ||
      all(is.na(data[, Longitude])) || all(is.na(data[, Latitude])) ) return("Longitude or Latitude not available.")

  # process coordinate data
  data <- data %>%
    shiftDataToDefaultRestriction() %>%
    removeDataOutsideRestriction(Latitude = Latitude,
                                 Longitude = Longitude,
                                 restriction = restriction)

  data <- data %>%
    prepareDate(DateOne = DateOne,
                DateTwo = DateTwo,
                DateType = DateType,
                dateUnc = dateUnc)
  if (all(is.na(data[, DateOne]))) return("non-numeric date field 1 variable")
  if (DateType != "Single point" && (!all(is.na(data[, DateTwo])))) return("non-numeric date field 2 variable")

  # select columns
  data <- na.omit(data[, c("Date", "Uncertainty", Longitude, Latitude)])

  if (nrow(unique(data[, c(Longitude, Latitude)])) <= K) {
    K <- ceiling(0.9 * (nrow(unique(data[, c(Longitude, Latitude)])) - 1))
    if (K < 4) {return("less than 4 rows")}
  }
  data$Longitude <- data[, Longitude]
  data$Latitude <- data[, Latitude]

  if(outlierD == TRUE){
    moD <- mean(data$Date, na.rm = TRUE)
    soD <- sqrt(var(data$Date, na.rm = TRUE) + data$Uncertainty ^ 2)
    data <- data[(data$Date - 2 * data$Uncertainty) >= moD - outlierValueD * soD, ]
    data <- data[(data$Date + 2 * data$Uncertainty) <= moD + outlierValueD * soD, ]
  }
  if(outlier == TRUE){
    model <- gam(Date ~ s(Longitude, Latitude, k = K), data = data)
    sc <- NULL
    if ( class(model)[1] == "try-error") {return("Error in Model Fitting.")}
    data <- dataOrg[as.numeric(rownames(data)[which(abs(scale(residuals(model))) < outlierValue)]), ]
    return(list(model = model, data = data, sc = sc, independent = "Date"))
  }

  if(MinMax == "Min"){
    dataJoin <- aggregate(data$Date + data$Uncertainty,
                          list(data$Longitude, data$Latitude),
                          min, simplify = T)
    names(dataJoin) <- c("Longitude", "Latitude", "value")
    data$rownames <- as.numeric(rownames(data))
    data2 <- merge(data,
                   dataJoin,
                   by.x = c("Longitude", "Latitude"),
                   by.y = c("Longitude", "Latitude"))
    data2 <- data2[which((data2$Date - data2$Uncertainty) <= data2$value ), ]
    rownames(data2) <- data2$rownames
    data2 <- data2[order(data2$rownames),]
    data2$rownames <- NULL
  } else {
    dataJoin <- aggregate(data$Date - data$Uncertainty,
                          list(data$Longitude, data$Latitude),
                          max, simplify = T)
    names(dataJoin) <- c("Longitude", "Latitude", "value")
    data$rownames <- as.numeric(rownames(data))
    data2 <- merge(data, dataJoin,
                   by.x = c("Longitude", "Latitude"),
                   by.y = c("Longitude", "Latitude"))
    data2 <- data2[which((data2$Date + data2$Uncertainty) >= data2$value ), ]
    rownames(data2) <- data2$rownames
    data2 <- data2[order(data2$rownames),]
    data2$rownames <- NULL
  }

  data <- unique(data2)
  if(dateUnc == "mid point"){
    data$Uncertainty2 <- rep(0, nrow(data))
    data$Uncertainty <- rep(0, nrow(data))
  }

  ### data augmentation ----
  if (correctionPac && splineType == 1 && center == "Europe") {
    data2 <- augmentData(data)
    K <- ceiling(K * nrow(data2) / nrow(data))
  } else {
    data2 <- data
  }

  ### data centering ----
  data2 <- centerData(data2, center = center)

  # calculate model ----
  model <- try(modelSpreadMC(data = data2, K = K, iter = iter,
                               burnin = burnin, nChains = nChains,
                               MinMax = MinMax, smoothConst = smoothConst,
                               shinyApp = shinyApp, penalty = penalty,
                               dateUnc = dateUnc,
                               splineType = splineType, thinning = thinning,
                               spreadQ = spreadQ, minValue = minValue), silent = TRUE)
    if ( class(model)[1] == "try-error") {return("Error in Model Fitting.")}

    return(list(model = model, data = data, sc = model$sc, independent = "Date",
                mRe = model$mRe, sRe = model$sRe, nChains = nChains))

  return(list(model = model, data = data, sc = sc, independent = "Date", nChains = nChains))
}

estimateMapSpreadWrapper <- function(data, input) {
  if(input$modelArea){
    restriction <- c(input$mALat1, input$mALat2, input$mALong1, input$mALong2)
    restriction[is.na(restriction)] <- c(-90, 90, -180, 180)[is.na(restriction)]
  } else {
    restriction <- c(-90, 90, -180, 180)
  }

  if(input$Outlier == TRUE){
    withProgress({
      dataOld <- data
      dataD <- data
      y <- dataD[, input$DateOne]
      if(input$DateType == "Interval"){
        y <- (y + dataD[, input$DateTwo]) / 2
      }
      moD <- mean(y, na.rm = TRUE)
      soD <- sd(y, na.rm = TRUE)
      dataD <- dataD[y >= moD - input$OutlierValueD * soD, ]
      dataD <- dataD[y <= moD + input$OutlierValueD * soD, ]
      outlierDR <- rownames(dataOld)[which(!(rownames(dataOld) %in% rownames(dataD)))]
      rm(dataD, y)
      data <- estimateMapSpread(data = data,
                          iter = input$Iter, burnin = input$burnin,
                          nChains = input$nChains, MinMax = input$MinMax, DateOne = input$DateOne,
                          DateTwo = input$DateTwo, DateType = input$DateType,
                          Longitude = input$Longitude, Latitude = input$Latitude, center = input$centerOfData,
                          CoordType = input$coordType,
                          penalty = as.numeric(input$Penalty),
                          splineType = as.numeric(input$SplineType),
                          correctionPac = input$correctionPac,
                          dateUnc = input$dateUnc,
                          K = input$Smoothing, smoothConst = input$smoothConst,
                          restriction = restriction,
                          shinyApp = TRUE,
                          outlier = TRUE,
                          outlierValue = input$OutlierValue,
                          outlierD = input$OutlierD,
                          outlierValueD = input$OutlierValueD,
                          thinning = input$thinning,
                          spreadQ = input$spreadQ,
                          minValue = input$minValueConstraint)$data},
      value = 0,
      message = "Removing outliers"
    )
    outlier <- rownames(dataOld)[which(!(rownames(dataOld) %in% rownames(data)))]
  } else {
    outlier <- character()
    outlierDR <- character()
  }

  model <- estimateMapSpread(data = data,
                    iter = input$Iter, burnin = input$burnin,
                    nChains = input$nChains, MinMax = input$MinMax, DateOne = input$DateOne,
                    DateTwo = input$DateTwo, DateType = input$DateType,
                    Longitude = input$Longitude, Latitude = input$Latitude,
                    CoordType = input$coordType,
                    restriction = restriction,
                    dateUnc = input$dateUnc,
                    penalty = as.numeric(input$Penalty),
                    correctionPac = input$correctionPac,
                    splineType = as.numeric(input$SplineType),
                    K = input$Smoothing, smoothConst = input$smoothConst,
                    shinyApp = TRUE,
                    outlierD = input$OutlierD,
                    outlierValueD = input$OutlierValueD,
                    thinning = input$thinning,
                    spreadQ = input$spreadQ,
                    minValue = input$minValueConstraint)
  if(class(model) == "list"){
    model$outlier <- outlier
    model$outlierDR <- outlierDR
  }

  model
}

#' Estimates spatio-temporal average model with (optional) random effects
#' (GAMM /Generalized Additive Mixed Model)
#'
#' Note regarding IndependentType = "categorical": This follows a one vs. all approach using
#' logistic regression, which in the Bayesian case is performed using a Polya-Gamma latent variable
#' during Gibbs-sampling (https://arxiv.org/abs/1205.0310).
#'
#' @param data data.frame: data
#' @param independent character: name of independent variable
#' @param IndependentType character: type ("numeric" or "categorical") of independent variable
#' @param Longitude character: name of longitude variable
#' @param Latitude character: name of latitude variable
#' @param Site character: name of site variable (optional)
#' @param burnin integer: number of burn-in iterations for Bayesian model (default = 500)
#' @param iter integer: number of iterations for Bayesian model (default = 2000)
#' @param nChains integer: number of chains for Bayesian model (default = 1)
#' @param DateOne character: name of date variable 1 (lower interval point / mean / single point)
#' @param DateTwo character: name of date variable 2 (upper interval point / sd / )
#' @param DateType character: one of "Interval", "Mean + 1 SD uncertainty" and "Single Point"
#' @param dateUnc character: one of "uniform", "normal", "point"
#' @param independentUncertainty character: uncertainty of independent variable in sd (optional)
#' @param K integer: number of basis functions for sos (spline on a sphere)
#' @param splineType numeric: 1 for classical tprs, 2 for spherical spline
#' @param KT integer: number of basis functions for tprs (thin plate regression spline)
#' @param penalty numeric: 1 for constant extrapolation, 2 for linear extrapolation
#' @param Bayes boolean: Bayesian model TRUE/FALSE?
#' @param CoordType character: type of longitude/latitude coordinates.
#'  One of "decimal degrees", "degrees minutes seconds" and "degrees decimal minutes"
#' @param smoothConst numeric: adjust smoothing parameter(>0) for Bayesian model (optional)
#' @param outlier boolean: outlier removal TRUE/FALSE
#' @param outlierValue numeric: if outlier removal is TRUE, threshold for removals in sd
#' @param outlierD boolean: data outlier removal TRUE/FALSE
#' @param outlierValueD numeric: if outlierD removal is TRUE, threshold for removals in sd
#' @param restriction numeric vector: spatially restricts model data 4 entries for latitude (min/max) and longitude(min/max)
#' @param sdVar boolean: variable standard deviation
#' @param correctionPac boolean: correction (data augmentation) for pacific centering
#' @param thinning numeric: mcmc thinning for bayesian models
#' @inheritParams centerData
#' @examples
#' \dontrun{
#' # load data
#' data <- readRDS(system.file("extData", "exampleData.Rds", package = "MpiIsoApp"))
#' # estimate model-map
#' map <- estimateMap3D(data = data, independent = "d13C", Longitude = "longitude",
#' Latitude = "latitude", DateOne = "dateLower", DateTwo = "dateUpper", Site = "site")
#' # Plot the map
#' plotMap3D(model = map, time = median(data$dateLower, na.rm = TRUE))
#' }
#' @export
estimateMap3D <- function(data,
                          independent,
                          Longitude,
                          Latitude,
                          DateOne,
                          DateTwo,
                          center = c("Europe", "Pacific"),
                          IndependentType = "numeric",
                          Site = "",
                          DateType = "Interval",
                          dateUnc = "uniform",
                          independentUncertainty = "",
                          CoordType = "decimal degrees",
                          burnin = 500,
                          iter = 2000,
                          nChains = 1,
                          splineType = 1,
                          K = 25,
                          KT = 10,
                          Bayes = FALSE,
                          penalty = 1,
                          smoothConst = 1,
                          outlier = FALSE,
                          outlierValue = 4,
                          outlierD = FALSE,
                          outlierValueD = 4,
                          restriction = c(-90, 90, -180, 180),
                          sdVar = FALSE,
                          correctionPac = FALSE,
                          thinning = 2) {
  set.seed(1234)
  center <- match.arg(center)

  dataOrg <- data
  if (is.null(data)) return(NULL)
  if (Longitude == "" || Latitude == "" || DateOne == "") return(NULL)
  if (!(all(c(Longitude, Latitude, independent, DateOne) %in% names(data)))) return(NULL)

  if ( (!is.numeric(data[, independent]) || all(is.na(data[, independent]))) & IndependentType == "numeric") return("non-numeric independent variable")

  # prepare data ----
  data <- data %>%
    prepareDate(DateOne = DateOne,
                DateTwo = DateTwo,
                DateType = DateType,
                dateUnc = dateUnc,
                useMaxUnc = FALSE)
  if (all(is.na(data[, DateOne]))) return("non-numeric date field 1 variable")
  if (DateType != "Single point" && (all(is.na(data[, DateTwo])))) return("non-numeric date field 2 variable")

  if ( Site != "" && all(is.na(data[, Site]))) return("wrong site variable")

  data <- data %>%
    convertLatLongWrapper(Longitude = Longitude,
                          Latitude = Latitude,
                          CoordType = CoordType)
  # if conversion fails Long/Lat are removed -> columns will be missing
  if (!all(c(Longitude, Latitude) %in% names(data)) ||
      all(is.na(data[, Longitude])) || all(is.na(data[, Latitude])) ) return("Longitude or Latitude not available.")

  # process coordinate data
  data <- data %>%
    shiftDataToDefaultRestriction() %>%
    removeDataOutsideRestriction(Latitude = Latitude,
                                 Longitude = Longitude,
                                 restriction = restriction)

  if (Site == ""){
    data$Site = 1:nrow(data)
    Site = "Site"
  }

  data$Site <- data[, Site]

  if (DateType == "Interval"){
    if(independentUncertainty != "" && !all(is.na(data[, independentUncertainty]))){
      data$independentUncertainty <- data[, independentUncertainty]
      data$independentUncertainty[is.na(data$independentUncertainty)] <- 0
      data <- na.omit(data[, c(independent, Longitude, Latitude, "Site",
                               "Date", "Uncertainty", "independentUncertainty")])
    } else {
      data <- na.omit(data[, c(independent, Longitude, Latitude, "Site",
                               "Date", "Uncertainty")])
    }
    data$Uncertainty2 <- pmax(0, data$Uncertainty / sd(data$Date))
  }
  if (DateType == "Single point"){
    if(independentUncertainty != "" && !all(is.na(data[, independentUncertainty]))){
      data$independentUncertainty <- data[, independentUncertainty]
      data <- na.omit(data[, c(independent, Longitude, Latitude, "Site",
                               "Date", "Uncertainty", "independentUncertainty")])
    } else {
      data <- na.omit(data[, c(independent, Longitude, Latitude, "Site",
                               "Date", "Uncertainty")])
    }
    data$Uncertainty2 <- 0
  }
  if (DateType == "Mean + 1 SD uncertainty"){
    if(independentUncertainty != "" && !all(is.na(data[, independentUncertainty]))){
      data$independentUncertainty <- data[, independentUncertainty]
      data <- na.omit(data[, c(independent, Longitude, Latitude, "Site",
                               "Date", "Uncertainty", "independentUncertainty")])
    } else {
      data <- na.omit(data[, c(independent, Longitude, Latitude, "Site",
                               "Date", "Uncertainty")])
    }
    data$Uncertainty2 <- pmax(0, data$Uncertainty / sd(data$Date))
  }

  data$Longitude2 <- (data[, Longitude] - mean(data[, Longitude])) / (sd(data[, Longitude]))
  data$Latitude2 <- (data[, Latitude] - mean(data[, Latitude])) / (sd(data[, Latitude]))
  data$Date2 <- (data$Date - mean(data$Date)) / (sd(data$Date))

  if(dateUnc == "point"){
    data$Uncertainty2 <- rep(0, nrow(data))
    data$Uncertainty <- rep(0, nrow(data))
  }

  if (nrow(unique(data[, c(Longitude, Latitude)])) <= K) {
    K <- ceiling(0.9 * nrow(unique(data[, c(Longitude, Latitude)])))
    if (K < 4) {return("less than 4 rows")}
  }
  if (length(unique(data[, c("Date")])) <= KT) {
    KT <- ceiling(0.9 * length(unique(data[, c("Date")])))
    if (KT < 4) {return("less than 4 rows")}
  }

  independentnew <- independent
  if (grepl("[^a-zA-Z]", substr(independent,1,1))){
    independentnew <- paste0("x", independent)
  }
  if (grepl("[^a-zA-Z0-9._]", independent)){
    independentnew <- gsub("[^a-zA-Z0-9._]", "", independentnew)
  }
  data$independentnew <- data[, independent]
  names(data)[names(data) == "independentnew"] <- independentnew
  independent <- independentnew
  data$independentnew <- NULL
  data$Longitude <- data[, Longitude]
  data$Latitude <- data[, Latitude]

  if(outlierD == TRUE & IndependentType == "numeric"){
    moD <- mean(data[, independent], na.rm = TRUE)
    soD <- sd(data[, independent], na.rm = TRUE)
    data <- data[data[, independent] >= moD - outlierValueD * soD, ]
    data <- data[data[, independent] <= moD + outlierValueD * soD, ]
  }

  ### data augmentation ----
  if (correctionPac && splineType == 1 && center == "Europe") {
    data2 <- augmentData(data)
    data2$Longitude2 <- (data2$Longitude - mean(data$Longitude)) / (sd(data$Longitude))
    data2$Latitude2 <- (data2$Latitude - mean(data$Latitude)) / (sd(data$Latitude))
    K <- ceiling(K * nrow(data2) / nrow(data))
  } else {
    data2 <- data
  }

  ### data centering ----
  data2 <- centerData(data2, center = center)

  # calculate model ----
  if (splineType == 2){
    splineExpr <- paste("te(Latitude, Longitude, Date2, d = c(2,1), m = c(", penalty, ",", penalty, ")", ", k = c(", K, ",", KT, ")" , ", bs = c(\"sos\", \"tp\"))")
  } else {
    splineExpr <- paste("s(Latitude2, Longitude2, Date2, m = ", penalty, ", k = ", K, ", bs = \"ds\")")
  }

  if (Bayes == FALSE){
    #outlier
    sc <- NULL
    scV <- NULL
    if(IndependentType == "numeric"){
      fm = "gaussian"
      model <- estimateModel3D(data2, fm, independent, splineExpr)
      if ( class(model)[1] == "try-error") {return("Error in Model Fitting.")}
      if(outlier == TRUE){
        data2 <- dataOrg[as.numeric(rownames(data2)[which(abs(scale(residuals(model$lme))) < outlierValue)]), ]
        return(list(model = model, data = data2, sc = sc, scV = scV, independent = independent, IndependentType = IndependentType))
      }
      predRange <- predict(model$gam, se.fit = TRUE)
      model$range <- list(mean = range(predRange$fit), se = range(predRange$se.fit),
                          seTotal = sqrt(range(var(residuals(model$gam)) + max(predRange$se.fit)^2)))
    } else {
      fm = "binomial"
      dummy_matrix <- model.matrix(~ . -1, data = data2[,independent, drop = FALSE])
      colnames(dummy_matrix) <- sapply(strsplit(colnames(dummy_matrix), split = independent), function(x) x[2])
      data2 <- cbind(data2, dummy_matrix)
      #only data instead of data2 is exported, dummy matrix is important for time course plot
      dummy_matrix1 <- model.matrix(~ . -1, data = data[,independent, drop = FALSE])
      colnames(dummy_matrix1) <- sapply(strsplit(colnames(dummy_matrix1), split = independent), function(x) x[2])
      data <- cbind(data, dummy_matrix1)
      model <- lapply(colnames(dummy_matrix), function(x){
        model <- estimateModel3D(data2, fm, x, splineExpr)
        if ( class(model)[1] == "try-error") {return("Error in Model Fitting.")}
        if(outlier == TRUE){
          data2 <- dataOrg[as.numeric(rownames(data2)[which(abs(scale(residuals(model$lme))) < outlierValue)]), ]
          return(list(model = model, data = data2, sc = sc, scV = scV, independent = independent, IndependentType = IndependentType))
        }
        predRange <- predict(model$gam, se.fit = TRUE)
        model$range <- list(mean = range(predRange$fit), se = range(predRange$se.fit),
                            seTotal = sqrt(range(var(residuals(model$gam)) + max(predRange$se.fit)^2)))
        model
      })
      names(model) <- colnames(dummy_matrix)
    }
    } else {
      if(IndependentType == "numeric"){
      model <- try(modelLocalTempAvgMC(data = data2, K = K, KT = KT, iter = iter,
                                     burnin = burnin, nChains = nChains,
                                     independent = independent,
                                     smoothConst = smoothConst,
                                     penalty = penalty, splineType = splineType,
                                     sdVar = sdVar, dateUnc = dateUnc, thinning = thinning), silent = TRUE)
    if ( class(model)[1] == "try-error") {return("Error in Model Fitting.")}
      sRe = model$sRe
      mRe = model$mRe
      sc = model$sc
      scV = model$scV

      } else {
        dummy_matrix <- model.matrix(~ . -1, data = data2[,independent, drop = FALSE])
        colnames(dummy_matrix) <- sapply(strsplit(colnames(dummy_matrix), split = independent), function(x) x[2])
        data2 <- cbind(data2, dummy_matrix)
        #only data instead of data2 is exported, dummy matrix is important for time course plot
        dummy_matrix1 <- model.matrix(~ . -1, data = data[,independent, drop = FALSE])
        colnames(dummy_matrix1) <- sapply(strsplit(colnames(dummy_matrix1), split = independent), function(x) x[2])
        data <- cbind(data, dummy_matrix1)
        model <- lapply(colnames(dummy_matrix), function(x){
          model <- try(modelLocalTempAvgMC(data = data2, K = K, KT = KT, iter = iter,
                                           burnin = burnin, nChains = nChains,
                                           independent = x,
                                           smoothConst = smoothConst,
                                           penalty = penalty, splineType = splineType,
                                           sdVar = sdVar, dateUnc = dateUnc, thinning = thinning), silent = TRUE)
          if ( class(model)[1] == "try-error") {return("Error in Model Fitting.")}
          model
        })
        names(model) <- colnames(dummy_matrix)
        sRe = 1
        mRe = 0
        sc = model[[1]]$sc
        scV = model[[1]]$scV
      }
    return(list(model = model, data = data, sc = sc, scV = scV, independent = independent,
                mRe = mRe, sRe = sRe, nChains = nChains, IndependentType = IndependentType))
  }
  return(list(model = model, data = data, sc = sc, scV = scV, independent = independent, nChains = nChains, IndependentType = IndependentType))
}

estimateMap3DWrapper <- function(data, input) {
    if(as.numeric(input$SplineType) == 2){
        K  <- input$Smoothing
      } else {
        K  <- input$SmoothingClassic
      }
    if(input$modelArea){
      restriction <- c(input$mALat1, input$mALat2, input$mALong1, input$mALong2)
      restriction[is.na(restriction)] <- c(-90, 90, -180, 180)[is.na(restriction)]
    } else {
      restriction <- c(-90, 90, -180, 180)
    }

    if(input$Outlier == TRUE){
      withProgress({
        dataOld <- data
        dataD <- data
        moD <- mean(dataD[, input$IndependentX], na.rm = TRUE)
        soD <- sd(dataD[, input$IndependentX], na.rm = TRUE)
        dataD <- dataD[dataD[, input$IndependentX] >= moD - input$OutlierValueD * soD, ]
        dataD <- dataD[dataD[, input$IndependentX] <= moD + input$OutlierValueD * soD, ]
        outlierDR <- rownames(dataOld)[which(!(rownames(dataOld) %in% rownames(dataD)))]
        rm(dataD)

        data <- estimateMap3D(data = data, Bayes = FALSE, independent = input$IndependentX,
                            independentUncertainty = input$IndependentUnc,
                            IndependentType = input$IndependentType,
                            Longitude = input$Longitude, Latitude = input$Latitude, center = input$centerOfData,
                            Site = input$Site, CoordType = input$coordType,
                            iter = input$Iter, burnin = input$burnin,
                            nChains = input$nChains, DateOne = input$DateOne,
                            penalty = as.numeric(input$Penalty),
                            splineType = as.numeric(input$SplineType),
                            DateTwo = input$DateTwo, DateType = input$DateType,
                            K = K, KT = input$SmoothingT,
                            correctionPac = input$correctionPac,
                            restriction = restriction,
                            smoothConst = input$smoothConst,
                            outlier = TRUE,
                            outlierValue = input$OutlierValue,
                            outlierD = input$OutlierD,
                            outlierValueD = input$OutlierValueD,
                            thinning = input$thinning)$data},
        value = 0,
        message = "Removing outliers"
      )
      outlier <- rownames(dataOld)[which(!(rownames(dataOld) %in% rownames(data)))]
    } else {
    outlier <- character()
    outlierDR <- character()
  }

  if(input$Bayes != TRUE){
    withProgress(
      model <- estimateMap3D(data = data, Bayes = input$Bayes, independent = input$IndependentX,
                    independentUncertainty = input$IndependentUnc,
                    IndependentType = input$IndependentType,
                    Longitude = input$Longitude, Latitude = input$Latitude,
                    Site = input$Site, CoordType = input$coordType,
                    iter = input$Iter, burnin = input$burnin,
                    nChains = input$nChains, DateOne = input$DateOne,
                    penalty = as.numeric(input$Penalty),
                    splineType = as.numeric(input$SplineType),
                    restriction = restriction,
                    correctionPac = input$correctionPac,
                    DateTwo = input$DateTwo, DateType = input$DateType,
                    outlierD = input$OutlierD,
                    outlierValueD = input$OutlierValueD,
                    K = K, KT = input$SmoothingT,
                    smoothConst = input$smoothConst,
                    thinning = input$thinning),
      value = 0,
      message = "Generating spatio-temporal model"
    )
  } else {
    model <- estimateMap3D(data = data, Bayes = input$Bayes, independent = input$IndependentX,
                  independentUncertainty = input$IndependentUnc,
                  IndependentType = input$IndependentType,
                  Longitude = input$Longitude, Latitude = input$Latitude,
                  Site = input$Site, CoordType = input$coordType,
                  iter = input$Iter, burnin = input$burnin,
                  nChains = input$nChains, DateOne = input$DateOne,
                  penalty = as.numeric(input$Penalty),
                  restriction = restriction,
                  dateUnc = input$dateUnc,
                  correctionPac = input$correctionPac,
                  splineType = as.numeric(input$SplineType),
                  DateTwo = input$DateTwo, DateType = input$DateType,
                  outlierD = input$OutlierD,
                  outlierValueD = input$OutlierValueD,
                  K = K,
                  KT = input$SmoothingT, smoothConst = input$smoothConst,
                  sdVar = input$sdVar,
                  thinning = input$thinning)
  }
  if(class(model) == "list"){
    model$outlier <- outlier
    model$outlierDR <- outlierDR
  }

  model
}


alertBayesMessage <- function() "Are you sure? The Bayesian model may take a while!"

modelLocalAvgMC <- function(data, K, iter, burnin, independent, smoothConst,
                            IndependentType = "numeric",
                          penalty = 1, splineType = 2, sdVar = FALSE,
                          nChains = 1, thinning = thinning){
  ret <- lapply(1:nChains, function(x){
    modelLocalAvg(data = data, K = K, iter = iter, burnin = burnin, independent = independent,
                  smoothConst = smoothConst,
                  IndependentType = IndependentType,
                              penalty = penalty,
                  splineType = splineType, sdVar = sdVar,
                  nChains = x, thinning = thinning)
  })
  res <- ret[[1]]
  res$beta <- do.call("rbind", lapply(1:length(ret), function(x) ret[[x]]$beta))
  res$betaSigma <- do.call("rbind", lapply(1:length(ret), function(x) ret[[x]]$betaSigma))
  res$sigma <- do.call("rbind", lapply(1:length(ret), function(x) ret[[x]]$sigma))
  res$tau <- do.call("rbind", lapply(1:length(ret), function(x) ret[[x]]$tau))
  return(res)
}


modelLocalAvg <- function(data, K, iter, burnin, independent, smoothConst,
                          IndependentType = "numeric",
                          penalty = 1, splineType = 2, sdVar = FALSE, nChains = 1,
                          thinning = 2){
  n <- nrow(data)
  data$Y <- data[, independent]
  nknots <- K
  burnInProp <- pmax(pmin(0.8, burnin / iter), 0.01)
  #thinning <- max(1, floor(iter * (1 - burnInProp) / 1000))
  burnin <- round(burnInProp * iter)
  every <- thinning  #nur die x-te MCMC-Iteration soll genutzt werden
  #Vektor der tatsaechlich benutzten Beobachtungen
  usedsamples <- seq(from = burnin, to = iter, by = every)

  if (splineType == 2){
    bs = "sos"
  } else {
    bs = "ds"
  }

  s <- smoothCon(s(Latitude, Longitude, k = nknots, bs = bs, m = penalty),
                 data = data, knots = NULL)[[1]]

  #Strafmatrizen
  P <- s$S[[1]]
  #Rang der Strafmatrix P
  M <- qr(P)$rank
  nknots <- dim(P)[1]

  sV <- smoothCon(s(Latitude, Longitude, m = 1,
                    k = max(10, min(100, ceiling(K / 2))), bs = bs),
                  data = data, knots = NULL)[[1]]

  #VarianzSpline
  PV <- sV$S[[1]]
  #Rang der Strafmatrix P
  nknots <- dim(PV)[1]
  MV <- qr(PV)$rank
  XXV <- Predict.matrix(sV, data)


  #Designmatrix
  XX <- Predict.matrix(s, data)
  cXX <- Crossprod(XX,XX)
  cXXV <- Crossprod(XXV,XXV)
  data$Site <- as.character(data$Site)
  if(length(unique(data$Site)) == nrow(data)){
    U <- diag(nrow(data))
    noU <- T
    cU <- rep(1, nrow(data))
    tU <- U
  } else{
    U <- model.matrix( ~ Site - 1, data = data)
    noU <- F
    cU <- diag(Crossprod(U, U))
    tU <- t(U)
  }
  uMatch <- unlist(sapply(1:ncol(U), function(x){which(U[, x]== 1)}))
  #####################################
  ###Starting Values
  #####################################
  #Chain 1
  sigma <- rep(1, nrow(data))
  tau <- 1
  gamma <- rep(0, dim(U)[2])
  lam <- 1E-5
  beta <- rep(0, ncol(XX))

  if(sdVar & IndependentType == "numeric"){
    sigmaSigma <- rep(1, nrow(data))
    betaSigma<- rep(0, ncol(XXV))
    lamSigma <- 1E-5
  }

  ######################################
  ###Tuningparameter der a-priori Verteilungen:
  ######################################
  a.eps <- 1E-5
  b.eps <- 1E-5
  a.mu <- 1E-5
  b.mu <- 1E-5
  a.tau <- 1E-5
  b.tau <- 1E-5
  lam.mu <- 1E-5
  lam.sigma <- 1E-5

  #######################################
  ###Parametermatrizen zur Speicherung der MCMC Iterationen
  #######################################
  betamc <- matrix(ncol = dim(XX)[2], nrow = length(usedsamples))
  betamcSigma <- matrix(ncol = dim(XXV)[2], nrow = length(usedsamples))

  taumc <- matrix(ncol = 1, nrow = length(usedsamples))
  # gammamc <- matrix(ncol = length(gamma), nrow = iter)
  smc <- matrix(ncol = 1, nrow = length(usedsamples))
  # sigmamc <- matrix(ncol = 1, nrow = iter)
  # lambdamc <- matrix(ncol = 1, nrow = iter)

  ########################################
  #MCMC-Algorithmus
  ########################################

  #rescale
  if(IndependentType == "numeric"){
    mRe <- mean(data$Y)
    sRe <- sd(data$Y)
    data$Y <- (data$Y - mRe) / sRe
  } else {
    mRe = 0
    sRe = 1
  }
  if(!is.null(data$independentUncertainty)){
    data$independentUncertainty <- data$independentUncertainty / sRe
  }
  YMean <- data$Y
  MCMC_LocalAvg <- function(start, iter){
    for (i in start:iter) {
      #conditional posterioris:
      # nolint start
      if(!is.null(data$independentUncertainty)){
        sdmY <- 1 / (1 / sigma + 1 / (data$independentUncertainty ^ 2 + 1E-6))
        mY <- ((XX %*% beta + U %*% gamma) / sigma +
                 YMean / (data$independentUncertainty ^ 2 + 1E-6)) * sdmY
        if(IndependentType == "numeric"){
          data$Y <- rnorm(length(data$independentUncertainty), mY, sd = sqrt(sdmY))
        } else {
          data$Y <- pmax(0, pmin(1, rnorm(length(data$independentUncertainty), mY, sd = sqrt(sdmY))))
        }
      }
      if(IndependentType == "numeric"){
      if(!sdVar){
        scale <- (b.eps + 0.5 * sum((((data$Y - XX %*% beta - U %*% gamma)) ^ 2))) ^ - 1
        sigma <<- 1 / rgamma(1, shape = a.eps + n / 2, scale = scale)
      } else {
        scale0 <- (b.eps + 0.5 * sum((((data$Y - XX %*% beta - U %*% gamma)) ^ 2))) ^ - 1
        sigma0 <- 1 / rgamma(1, shape = a.eps + n / 2, scale = scale0)
        scaleSigma <- (b.eps + 0.5 * sum((((XXV %*% betaSigma) - log((data$Y - XX %*% beta - U %*% gamma)^2))) ^ 2)) ^ - 1
        sigmaSigma <<- 1 / rgamma(1, shape = a.eps + n / 2, scale = scaleSigma)
        inverseSigma <- spdinv(cXXV / sigmaSigma + lamSigma * PV)
        betaSigma <<- as.vector(rmvnorm(1, mu = inverseSigma %*%
                                          crossprod(XXV / sigmaSigma,
                                                    log((data$Y - XX %*% beta - U %*% gamma) ^ 2)), sigma = inverseSigma))
        sigmaTmp <-  as.numeric(exp(XXV %*% (betaSigma)))
        sigma0 <- mean(sigmaTmp) / sigma0
        sigma <<- sigmaTmp / sigma0 + 1E-4
      }
      } else {
        sigma <<- pgdraw(1, XX %*% beta + U %*% gamma)
      }
      # nolint end
      if(IndependentType == "numeric"){
      if(!sdVar){
        inverse <- spdinv(cXX / sigma + lam * P)
        beta <<- as.vector(rmvnorm(1, mu = inverse %*% crossprod(XX / sigma, (data$Y - U %*% gamma)), sigma = inverse))
      } else {
        inverse <- spdinv(crossprod((XX/sigma), (XX)) + lam * P)
        beta <<- as.vector(rmvnorm(1, mu = inverse %*% crossprod(XX / sigma, (data$Y - U %*% gamma)), sigma = inverse))
      }
      }  else {
        inverse <- spdinv(crossprod((XX*sigma), (XX)) + lam * P)
        beta <<- as.vector(rmvnorm(1, mu = inverse %*% (crossprod(XX, (data$Y - 0.5)) - crossprod(XX*sigma, U %*% gamma)), sigma = inverse))
      }
      #gamma
      # nolint start
      if(IndependentType == "numeric"){
      if(!sdVar){
        inverse2 <-  1 / (cU / sigma + 1 / tau)
        gamma <<-  rnorm(n = length(inverse2), mean = (tU * inverse2 / sigma) %*%
                           ((data$Y - (XX) %*% beta)), sd = sqrt(inverse2))
      } else {
        uTmp <- sapply(1:length(cU), function(i) mean(sigma[uMatch[(1 + c(0, cumsum(cU))[i]): c(0, cumsum(cU))[i+1]]]))
        inverse2 <-  1 / (cU / uTmp + 1 / tau)
        gamma <<-  rnorm(n = length(inverse2), mean = (tU * inverse2 / uTmp) %*%
                           ((data$Y - (XX) %*% beta)), sd = sqrt(inverse2))
      }
      } else {
        uTmp <- sapply(1:length(cU), function(i) mean(sigma[uMatch[(1 + c(0, cumsum(cU))[i]): c(0, cumsum(cU))[i+1]]]))
        inverse2 <-  1 / (cU * uTmp + 1 / tau)
        gamma <<-  rnorm(n = length(inverse2), mean = (inverse2) * (crossprod(U,data$Y - 0.5) - (crossprod(U*sigma, XX %*% beta))), sd = sqrt(inverse2))
      }
      # nolint end

      if(length(unique(data$Site)) == nrow(data)){
        gamma <<- rep(0, length(gamma))
      }

      #Sigma
      #Tau
      tau <<- 1 /
        rgamma(
          1,
          shape = a.tau + length(gamma) / 2,
          scale = (b.tau + 0.5 * sum(gamma ^ 2)) ^ - 1)
      #Smoothing Parameter lambda
      lam <<- rgamma(
        1,
        shape = lam.mu + M / 2,
        scale = (lam.sigma + 0.5 * crossprod(beta, P) %*% beta) ^ - 1
      ) * smoothConst
      if(sdVar & IndependentType == "numeric"){
        lamSigma <<- rgamma(
          1,
          shape = lam.mu + MV / 2,
          scale = (lam.sigma + 0.5 * crossprod(betaSigma, PV) %*% betaSigma) ^ - 1
        ) * smoothConst
      }

      #Werte in Parametermatrizen einsetzen
      if(i %in% usedsamples){
      pointer <- which(usedsamples == i)
      betamc[pointer, ] <<- beta
      if(IndependentType == "numeric"){
      if(sdVar){
        betamcSigma[pointer, ] <<- betaSigma
      }
      if(sdVar){
        smc[pointer, ] <<- sigma0
      } else {
        smc[pointer, ] <<- mean(sigma)
      }
      } else {
        smc[pointer, ] <<- mean(invLogit(XX %*% beta + U %*% gamma) * (1-invLogit(XX %*% beta + U %*% gamma)))
      }
      # gammamc[i, ] <- gamma
      taumc[pointer, ] <<- tau
      # lambdamc[i, ] <- lam
      }
    }
  }
  if(IndependentType == "numeric"){
    msg <- "Calculating Local Average Model"
  } else {
    msg <- paste0("Calculating Local Average Model - ", independent)
  }
  for ( k in 1:10) {
    j <- seq(1, iter, iter / 10)[k]
    showMessage(
      MCMC_LocalAvg,
      msg = msg,
      detail = paste0("Chain ", nChains),
      value = k / 10)(
        start = j, iter = j + iter / 10 - 1
      )
  }
  # burnin <- round(burnInProp * iter)
  # every <- thinning  #nur die x-te MCMC-Iteration soll genutzt werden
  # #Vektor der tatsaechlich benutzten Beobachtungen
  # usedsamples <- seq(from = burnin, to = iter, by = every)
  if(IndependentType == "numeric"){
  if(sdVar & IndependentType == "numeric"){
    seTotal = range(sqrt(apply(sapply(1:length(usedsamples), function(x)
      (XX %*% betamc[x, ]) * sRe + mRe), 1, var) +
        rowMeans(sapply(1:length(usedsamples), function(x)
          exp((XXV %*% betamcSigma[x, ])) / smc[x] * sRe^2))))
  } else {
    betamcSigma <- NULL
    seTotal = range(sqrt(apply(sapply(1:length(usedsamples), function(x)
      (XX %*% betamc[x, ]) * sRe + mRe), 1, var) + mean(smc)))
  }
  } else {
    betamcSigma <- NULL
    pred_probs <- sapply(1:length(usedsamples), function(x) invLogit(XX %*% betamc[x, ]) * sRe + mRe)
    seTotal = range(sqrt(apply(pred_probs, 1, var) + pred_probs * (1-pred_probs)))
  }
  return(list(beta = betamc, betaSigma = betamcSigma, sc = s, scV = sV, sigma = smc,
              tau = taumc, mRe = mRe, sRe = sRe,
              range = list(mean = range(rowMeans(sapply(1:length(usedsamples), function(x)
                (XX %*% betamc[x, ]) * sRe + mRe))),
                se = range(sqrt(apply(sapply(1:length(usedsamples), function(x)
                  (XX %*% betamc[x, ]) * sRe + mRe), 1, var))),
                seTotal = seTotal)))
}


modelLocalTempAvgMC <- function(data, K, KT, iter, burnin, independent,
                                smoothConst, IndependentType = "numeric", penalty, dateUnc = "uniform",
                                splineType = 1, sdVar = FALSE, nChains = 1, thinning = thinning){
  ret <- lapply(1:nChains, function(x){
    modelLocalTempAvg(data = data, K = K, KT = KT, iter = iter,
                      burnin = burnin, independent = independent,
                  smoothConst = smoothConst,
                  IndependentType = IndependentType,
                  penalty = penalty, dateUnc = dateUnc,
                  splineType = splineType, sdVar = sdVar,
                  nChains = x, thinning = thinning)
  })
  res <- ret[[1]]
  res$beta <- do.call("rbind", lapply(1:length(ret), function(x) ret[[x]]$beta))
  res$betaSigma <- do.call("rbind", lapply(1:length(ret), function(x) ret[[x]]$betaSigma))
  res$sigma <- do.call("rbind", lapply(1:length(ret), function(x) ret[[x]]$sigma))
  res$tau <- do.call("rbind", lapply(1:length(ret), function(x) ret[[x]]$tau))
  return(res)
}

modelLocalTempAvg <- function(data, K, KT, iter, burnin, independent,
                              smoothConst, IndependentType = "numeric", penalty,
                              dateUnc = "uniform",splineType = 1, sdVar = FALSE,
                              nChains = 1, thinning = 2){
  data$Date4 <- data$Date2
  set.seed(1234)
  data$Date2 <- sapply(1:length(data$Date2), function(x)
                                   runif(1, min = data$Date2[x] - 2 * data$Uncertainty2[x],
                                   max =  data$Date2[x] + 2 * data$Uncertainty2[x]))
  n <- nrow(data)
  data$Y <- data[, independent]

  nknots <- K
  burnInProp <- pmax(pmin(0.8, burnin / iter), 0.01)
  #thinning <- max(1, floor(iter * (1 - burnInProp) / 1000))
  burnin <- round(burnInProp * iter)
  every <- thinning  #nur die x-te MCMC-Iteration soll genutzt werden
  #Vektor der tatsaechlich benutzten Beobachtungen
  usedsamples <- seq(from = burnin, to = iter, by = every)

  if (splineType == 2){
    s <- smoothCon(te(Latitude, Longitude, Date2, m = c(penalty, penalty), d = c(2,1),
                      k = c(K, KT), bs = c("sos", "tp")),
                   data = data, knots = NULL)[[1]]
    data$Date3 <- data$Date2
    s2 <- smoothCon(te(Latitude, Longitude, Date3, m = c(penalty, penalty), d = c(2,1),
                       k = c(K, KT), bs = c("sos", "tp")),
                    data = data, knots = NULL)[[1]]
  } else {
    s <- smoothCon(s(Latitude2, Longitude2, Date2, m = c(penalty),
                     k = c(K), bs = c("ds")),
                   data = data, knots = NULL)[[1]]
    data$Date3 <- data$Date2
    s2 <- smoothCon(s(Latitude2, Longitude2, Date3, m = c(penalty),
                      k = c(K), bs = c("ds")),
                    data = data, knots = NULL)[[1]]
  }
  sV <- smoothCon(s(Latitude2, Longitude2, Date2, m = 1,
                    k = min(100, ceiling(K / 2)), bs = c("ds")),
                  data = data, knots = NULL)[[1]]

  #Strafmatrizen
  P <- s$S[[1]]
  #Rang der Strafmatrix P
  nknots <- dim(P)[1]
  M <- qr(P)$rank

  #VarianzSpline
  PV <- sV$S[[1]]
  #Rang der Strafmatrix P
  nknots <- dim(PV)[1]
  MV <- qr(PV)$rank
  XXV <- Predict.matrix(sV, data)

  if (splineType == 2){
    M <- M / 2
    P2 <- s$S[[2]]
    M2 <- qr(P2)$rank / 2
    nknots2 <- dim(P2)[1]
  }
  #Zur jeweiligen Erstellung der Praediktionsmatrix

  #Designmatrix
  XX <- Predict.matrix(s, data)

  data$Site <- as.character(data$Site)

  if(length(unique(data$Site)) == nrow(data)){
    U <- diag(nrow(data))
    noU <- T
    cU <- rep(1, nrow(data))
    tU <- U
  } else{
    U <- model.matrix( ~ Site - 1, data = data)
    noU <- F
    cU <- diag(Crossprod(U, U))
    tU <- t(U)
  }

  uMatch <- unlist(sapply(1:ncol(U), function(x){which(U[, x]== 1)}))
  #####################################
  ###Starting Values
  #####################################
  #Chain 1
  if(!sdVar | IndependentType != "numeric"){
    sigma <- 1
  } else {
    sigma <- rep(1, nrow(data))
  }
  tau <- 1
  gamma <- rep(0, dim(U)[2])
  lam <- 1E-5
  lam2 <- 1E-5
  beta <- rep(0, ncol(XX))
  XX2 <- XX

  if(sdVar){
    sigmaSigma <- rep(1, nrow(data))
    betaSigma<- rep(0, ncol(XXV))
    lamSigma <- 1E-5
    lamSigma2 <- 1E-5
  }

  ######################################
  ###Tuningparameter der a-priori Verteilungen:
  ######################################
  a.eps <- 1E-5
  b.eps <- 1E-5
  a.mu <- 1E-5
  b.mu <- 1E-5
  a.tau <- 1E-5
  b.tau <- 1E-5
  lam.mu <- 1E-5
  lam.sigma <- 1E-5

  #######################################
  ###Parametermatrizen zur Speicherung der MCMC Iterationen
  #######################################
  betamc <- matrix(ncol = dim(XX)[2], nrow = length(usedsamples))
  betamcSigma <- matrix(ncol = dim(XXV)[2], nrow = length(usedsamples))
  taumc <- matrix(ncol = 1, nrow = length(usedsamples))
  # gammamc <- matrix(ncol = length(gamma), nrow = iter)
  smc <- matrix(ncol = 1, nrow = length(usedsamples))
  # sigmamc <- matrix(ncol = 1, nrow = iter)
  # lambdamc <- matrix(ncol = 1, nrow = iter)
  # xmc <- matrix(ncol = n, nrow = iter)


  ########################################
  #MCMC-Algorithmus
  ########################################
  changeX <- which(data$Uncertainty2 > 0)

  #rescale
  if(IndependentType == "numeric"){
  mRe <- mean(data$Y)
  sRe <- sd(data$Y)
  data$Y <- (data$Y - mRe) / sRe
  } else {
    mRe = 0
    sRe = 1
  }
  if(!is.null(data$independentUncertainty)){
    data$independentUncertainty <- data$independentUncertainty / sRe
  }
  YMean <- data$Y
  MCMC_LocalTempAvg <- function(start, iter){
    for (i in start:iter) {
      print(i)
      if(!is.null(data$independentUncertainty)){
        sdmY <- 1 / (1 / sigma + 1 / (data$independentUncertainty ^ 2 + 1E-6))
        mY <- ((XX2 %*% beta + U %*% gamma) / sigma +
                 YMean / (data$independentUncertainty ^ 2 + 1E-6)) * sdmY
        if(IndependentType == "numeric"){
          data$Y <- rnorm(length(data$independentUncertainty), mY, sd = sqrt(sdmY))
        } else {
          data$Y <- pmax(0, pmin(1, rnorm(length(data$independentUncertainty), mY, sd = sqrt(sdmY))))
        }
      }
      #Betas
      if (splineType == 2){
        if(IndependentType == "numeric"){
        inverse <- spdinv(Crossprod(XX2 / sigma, XX2) + lam * P + lam2 * P2)
        } else {
          inverse <- spdinv(crossprod((XX2*sigma), (XX2)) + lam * P + lam2 * P2)
        }
      } else {
        if(IndependentType == "numeric"){
          inverse <- spdinv(Crossprod(XX2 / sigma, XX2) + lam * P)
        } else {
          inverse <- spdinv(crossprod((XX2*sigma), (XX2)) + lam * P)
        }
      }
      if(IndependentType == "numeric"){
        beta <<- as.vector(rmvnorm(1, mu = inverse %*% crossprod(XX2 / sigma, (data$Y - U %*% gamma)), sigma = inverse))
      } else {
        beta <<- as.vector(rmvnorm(1, mu = inverse %*% (crossprod(XX2, (data$Y - 0.5)) - crossprod(XX2*sigma, U %*% gamma)), sigma = inverse))
      }
        #beta <<- mvrnorm(mu = inverse %*% crossprod(XX2 / sigma, (data$Y - U %*% gamma)), Sigma = inverse, tol = 1E-5)
      # #MH-step for time
      if ((i %% 10 == 0) & length(changeX) > 0){
        data$Date3 <- sapply(1:length(data$Date4), function(x)
          runif(1,min = data$Date4[x] - 2 * data$Uncertainty2[x],
                max =  data$Date4[x] + 2 * data$Uncertainty2[x]))
        XX3 <- Predict.matrix(s2, data)
        alphas <- rep(0, n)
        if(sdVar){
          sigmaChange <- sigma[changeX]
        } else {
          sigmaChange <- sigma
        }
        alphas[changeX] <- AcceptanceTime(data[changeX, ],
                                          XX3[changeX, ],
                                          XX2[changeX, ],
                                          U[changeX, ],
                                          sigmaChange,
                                          beta, gamma,
                                          dateUnc = dateUnc,
                                          IndependentType = IndependentType)
        alphas[is.na(alphas)] <- 0
        randomAlpha <- runif(n)
        updated <- which(randomAlpha < alphas)
        data$Date2[updated] <- data$Date3[updated]
        if(length(updated) > 0){
          XX2[updated, ] <- (Predict.matrix(s, data[updated,]))
        }
      }
      #gamma
      # nolint start
      if(IndependentType == "numeric"){
        if(!sdVar){
          inverse2 <-  1 / (cU / sigma + 1 / tau)
          gamma <<-  rnorm(n = length(inverse2), mean = (tU * inverse2 / sigma) %*%
                            ((data$Y - (XX2) %*% beta)), sd = sqrt(inverse2))
        } else {
          uTmp <- sapply(1:length(cU), function(i) mean(sigma[uMatch[(1 + c(0, cumsum(cU))[i]): c(0, cumsum(cU))[i+1]]]))
          inverse2 <-  1 / (cU / uTmp + 1 / tau)
          gamma <<-  rnorm(n = length(inverse2), mean = (tU * inverse2 / uTmp) %*%
                            ((data$Y - (XX2) %*% beta)), sd = sqrt(inverse2))
        }
      } else {
          uTmp <- sapply(1:length(cU), function(i) mean(sigma[uMatch[(1 + c(0, cumsum(cU))[i]): c(0, cumsum(cU))[i+1]]]))
          inverse2 <-  1 / (cU * uTmp + 1 / tau)
          gamma <<-  rnorm(n = length(inverse2), mean = (inverse2) * (crossprod(U,data$Y - 0.5) - (crossprod(U*sigma, XX2 %*% beta))), sd = sqrt(inverse2))
      }
      if(length(unique(data$Site)) == nrow(data)){
        gamma <<- rep(0, length(gamma))
      }

      #Sigma
      #Tau
      tau <<- 1 / rgamma(
        1,
        shape = a.tau + length(gamma) / 2,
        scale = (b.tau + 0.5 * sum(gamma ^ 2)) ^ - 1)

      #Smoothing Parameter lambda
      lam <<- rgamma(
        1,
        shape = lam.mu + M / 2,
        scale = (lam.sigma + 0.5 * crossprod(beta, P) %*% beta) ^ - 1
      ) * smoothConst
      if (splineType == 2){
        lam2 <<- rgamma(
          1,
          shape = lam.mu + M2 / 2,
          scale = (lam.sigma + 0.5 * crossprod(beta, P2) %*% beta) ^ - 1
        ) * smoothConst
      }

      # nolint start
      #conditional posterioris:
      if(IndependentType == "numeric"){
        if(!sdVar){
          scale <- (b.eps + 0.5 * sum((((data$Y - XX2 %*% beta - U %*% gamma)) ^ 2))) ^ - 1
          sigma <<- 1 / rgamma(1, shape = a.eps + n / 2, scale = scale)
        } else {
          scale0 <- (b.eps + 0.5 * sum((((data$Y - XX2 %*% beta - U %*% gamma)) ^ 2))) ^ - 1
          sigma0 <- 1 / rgamma(1, shape = a.eps + n / 2, scale = scale0)

          scaleSigma <- (b.eps + 0.5 * sum((log((data$Y - XX2 %*% beta - U %*% gamma)^2) - (XXV %*% betaSigma)) ^ 2)) ^ - 1
          sigmaSigma <<- 1 / rgamma(1, shape = a.eps + n / 2, scale = scaleSigma)
          if (splineType == 2){
            inverseSigma <- spdinv(Crossprod(XXV / sigmaSigma, XXV) + lamSigma * PV)
          } else {
            inverseSigma <- spdinv(Crossprod(XXV / sigmaSigma, XXV) + lamSigma * PV)
          }
          betaSigma <<- as.vector(rmvnorm(1, mu = inverseSigma %*%
                                            crossprod((XXV / sigmaSigma),
                                                      log((data$Y - XX2 %*% beta - U %*% gamma) ^ 2)),
                                          sigma = inverseSigma))
          sigmaTmp <-  as.numeric(exp(XXV %*% (betaSigma)))
          sigma0 <- mean(sigmaTmp) / sigma0
          sigma <<- sigmaTmp / sigma0 + 1E-4
        }
      } else{
        sigma <<- pgdraw(1, XX2 %*% beta + U %*% gamma)
      }

      if(sdVar & IndependentType == "numeric"){
        lamSigma <<- rgamma(
          1,
          shape = lam.mu + MV / 2,
          scale = (lam.sigma + 0.5 * crossprod(betaSigma, PV) %*% betaSigma) ^ - 1
        )
        if (splineType == 2){
          lamSigma2 <<- rgamma(
            1,
            shape = lam.mu + MV / 2,
            scale = (lam.sigma + 0.5 * crossprod(betaSigma, PV) %*% betaSigma) ^ - 1
          )
        }
      }

      # nolint end

      #Werte in Parametermatrizen einsetzen
      if(i %in% usedsamples){
        pointer <- which(usedsamples == i)
      betamc[pointer, ] <<- beta
      if(sdVar){
        betamcSigma[pointer, ] <<- betaSigma
      }
      if(sdVar){
        smc[pointer, ] <<- sigma0
      } else {
        smc[pointer, ] <<- mean(sigma)
      }
      # gammamc[i, ] <- gamma
      taumc[pointer, ] <<- tau
      # lambdamc[i, ] <- lam
      }
    }
    return(betamc)
  }

  if(IndependentType == "numeric"){
    msg <- "Calculating Spatio-Temporal Average Model"
  } else {
    msg <- paste0("Calculating Spatio-Temporal Average Model - ", independent)
  }

  for ( k in 1:10) {
    j <- seq(1, iter, iter / 10)[k]
    showMessage(
      MCMC_LocalTempAvg,
      msg = msg,
      detail = paste0("Chain ", nChains),
      value = k / 10)(
        start = j, iter = j + iter / 10 - 1
      )
  }
  # burnin <- round(burnInProp * iter)
  # every <- thinning  #nur die x-te MCMC-Iteration soll genutzt werden
  #
  # #Vektor der tatsaechlich benutzten Beobachtungen
  # usedsamples <- seq(from = burnin, to = iter, by = every)
  if(IndependentType == "numeric"){
    if(sdVar & IndependentType == "numeric"){
      seTotal = range(sqrt(apply(sapply(1:length(usedsamples), function(x)
      (XX2 %*% betamc[x, ]) * sRe + mRe), 1, var) +
        rowMeans(sapply(1:length(usedsamples), function(x)
          exp((XXV %*% betamcSigma[x, ])) / smc[x] * sRe^2))))
    } else {
      betamcSigma <- NULL
      seTotal = range(sqrt(apply(sapply(1:length(usedsamples), function(x)
        (XX2 %*% betamc[x, ]) * sRe + mRe), 1, var) + mean(smc)))
    }
  } else{
    betamcSigma <- NULL
    pred_probs <- sapply(1:length(usedsamples), function(x) invLogit(XX2 %*% betamc[x, ]) * sRe + mRe)
    seTotal = range(sqrt(apply(pred_probs, 1, var) + pred_probs * (1-pred_probs)))
  }
  return(list(beta = betamc, betaSigma = betamcSigma, sc = s, scV = sV, sigma = smc,
              tau = taumc, mRe = mRe, sRe = sRe,
              range = list(mean = range(rowMeans(sapply(1:length(usedsamples), function(x)
                (XX2 %*% betamc[x, ]) * sRe + mRe))),
                se = range(sqrt(apply(sapply(1:length(usedsamples), function(x)
                  (XX2 %*% betamc[x, ]) * sRe + mRe), 1, var))),
                seTotal = seTotal)
  ))
}

AcceptanceTime <- function(data, XX, XX2, U, sigma, beta, gamma, dateUnc, IndependentType){
  pmin(1, exp(
    cpostX(
      XX = XX,
      xtru = data$Date3,
      xobs = data$Date4,
      sigma.obs = data$Uncertainty2 ^ 2,
      sigma.eps = sigma,
      beta = beta,
      U = U,
      gamma = gamma,
      y = data$Y,
      dateUnc = dateUnc,
      IndependentType = IndependentType) -
      cpostX(
        XX = XX2,
        xtru = data$Date2,
        xobs = data$Date4,
        sigma.obs = data$Uncertainty2 ^ 2,
        sigma.eps = sigma,
        beta = beta,
        U = U,
        gamma = gamma,
        y = data$Y,
        dateUnc = dateUnc,
        IndependentType = IndependentType
      )
  ))
}

########################################
## Metropolis Hastings Conditional Posteriori-functions
## for X-variables with measurement error (time)
########################################

cpostX <- function(XX,
                   xtru,
                   xobs,
                   sigma.obs,
                   sigma.eps,
                   beta,
                   U,
                   gamma,
                   y,
                   dateUnc,
                   IndependentType) {
  pred <- XX %*% beta + U %*% gamma
  if(IndependentType == "numeric"){
    ret <- (y - pred) ^ 2 / (-2 * sigma.eps)
  } else {
    ret <- log(pred) * y * log(1-pred) * (1-y)
  }

  if(dateUnc == "uniform"){
    return(ret + log(dunif (
                xtru,
                min = xobs - 2 * sqrt(sigma.obs),
                max = xobs + 2 * sqrt(sigma.obs)
              ))
    )
  } else {
    return(ret + (xobs - xtru) ^ 2 / (-2 * sigma.obs))
  }
}


modelSpreadMC <- function(data, K, iter, burnin,
                          MinMax, smoothConst, penalty,
                          shinyApp = FALSE, splineType = 2,
                          dateUnc = "uniform", nChains = 1,
                          thinning = 2,
                          spreadQ = 0.005,
                          minValue = -Inf){

  ret <- lapply(1:nChains, function(x){
    modelSpread(data = data, K = K, iter = iter, burnin = burnin, MinMax = MinMax,
                      smoothConst = smoothConst,
                      penalty = penalty, shinyApp = shinyApp,
                      splineType = splineType, dateUnc = dateUnc,
                      nChains = x, thinning = thinning,
                spreadQ = spreadQ, minValue = minValue)
  })
  res <- ret[[1]]
  res$beta <- do.call("rbind", lapply(1:length(ret), function(x) ret[[x]]$beta))
  res$pred <- do.call("rbind", lapply(1:length(ret), function(x) ret[[x]]$pred))
  return(res)
}


modelSpread <- function(data, K, iter, burnin, MinMax, smoothConst, penalty,
                        shinyApp = FALSE, splineType = 2,
                        dateUnc = "uniform", nChains = 1,
                        thinning = 2, spreadQ = 0.005,
                        minValue = -Inf){
  n <- nrow(data)
  data$Y <- data[, "Date"]
  data$Y2 <- data$Y
  nknots <- K
  burnInProp <- pmax(pmin(0.8, burnin / iter), 0.01)
  #thinning <- max(1, floor(iter * (1 - burnInProp) / 1000))

  if (splineType == 2){
    bs = "sos"
  } else {
    bs = "ds"
  }

  s <- smoothCon(s(Latitude, Longitude, k = nknots, bs = bs, m = penalty),
                 data = data, knots = NULL)[[1]]

  #Strafmatrizen
  P <- s$S[[1]]
  #Rang der Strafmatrix P
  M <- qr(P)$rank
  nknots <- dim(P)[1]

  #Designmatrix
  XX <- Predict.matrix(s, data)

  #####################################
  ###Starting Values
  #####################################
  #Chain 1
  sigma <- rep(1, nrow(data))
  delta <- 1
  w <- rep(1, nrow(data))
  lam <- 1E-5
  beta <- rep(0, ncol(XX))
  betamin <- rep(0, ncol(XX))

  ######################################
  ###Tuningparameter der a-priori Verteilungen:
  ######################################
  a.eps <- 1E-5
  b.eps <- 1E-5
  a.mu <- 1E-5
  b.mu <- 1E-5
  a.tau <- 1E-5
  b.tau <- 1E-5
  lam.mu <- 1E-5
  lam.sigma <- 1E-5

  #######################################
  ###Parametermatrizen zur Speicherung der MCMC Iterationen
  #######################################
  betamc <- matrix(ncol = dim(XX)[2], nrow = iter)
  # taumc <- matrix(ncol = 1, nrow = iter)
  # gammamc <- matrix(ncol = length(gamma), nrow = iter)
  # smc <- matrix(ncol = nrow(data), nrow = iter)
  # sigmamc <- matrix(ncol = 1, nrow = iter)
  # lambdamc <- matrix(ncol = 1, nrow = iter)

  #rescale
  mRe <- mean(data$Y2)
  sRe <- sd(data$Y2)
  data$Y2 <- (data$Y2 - mRe) / sRe
  data$Y <- (data$Y - mRe) / sRe
  if(!is.null(data$Uncertainty)){
    data$Uncertainty <- data$Uncertainty / sRe
  }


  ########################################
  #MCMC-Algorithmus
  ########################################
  uniqueN <- nrow(unique(na.omit(data)[, c("Longitude", "Latitude", "Y")]))
  if (MinMax == "Max"){q = 1 - spreadQ}
  else{q = spreadQ}
  eta <- (1 - 2 * q) / (q * (1-q))
  sigma <- (2) / (q * (1 - q))
  XBeta <- XX %*% beta
  print(nrow(data))
  if (MinMax == "Max" && (minValue == -Inf | is.na(minValue))){
    minValue <- Inf
  }
  if ((MinMax == "Min" && (minValue == Inf | is.na(minValue)))){
    minValue <- -Inf
  }

  minValue = (minValue - mRe) / sRe

  MCMC_Spread <- function(start, iter){
    selection <- which(data$Uncertainty > 0)
    for (i in start:iter) {
      #Betas
      delta <<- rgamma(1, a.tau + 3 * n / 2,
                       b.tau + 1 / (2 * sigma) * sum((data$Y2 - XBeta - eta * w) ^ 2 / w) + sum(w))

      w <<- 1 / rig(n = n, sqrt((eta ^ 2 + 2 * sigma) / (data$Y2 - XBeta) ^ 2),
                    1 / (delta * (eta ^ 2 + 2 * sigma) / sigma))

      inverse <- (Crossprod(XX, XX/w) * delta / sigma + lam * P)
      Sigma <- spdinv(inverse)

        beta <<- as.vector(rmvnorm(1, mu = Sigma  %*% crossprod(XX, (((data$Y2 - eta * w) * delta / sigma) / w)), sigma = Sigma))
        XBeta <<- XX %*% beta

      #Smoothing Parameter lambda
      lam <<- rgamma(1, shape = lam.mu + M / 2,
                     scale = (lam.sigma + 0.5 * crossprod(beta, P) %*% beta) ^ - 1) * smoothConst

      if (!is.null(data$Uncertainty)){
        data$Y2 <<- vapply(1:length(data$Y2), function(x){
          ysel <- data$Y[x]
          uncsel <- data$Uncertainty[x]
          if(uncsel > 0){
            if(MinMax == "Min"){
              uncSeq <- seq(pmax(minValue, ysel - 3 * uncsel),
                            ysel + 3 * uncsel, length.out = 20)
            } else {
              uncSeq <- seq(ysel - 3 * uncsel, pmin(minValue, ysel + 3 * uncsel), length.out = 20)
            }
            if(dateUnc == "uniform"){
              return(uncSeq[sample.int(20, size = 1, prob = exp(log(dALDFast(uncSeq, mu = XBeta[x], sigma = 1 / delta, p = q)) +
                                                           log(dunif(uncSeq, min = ysel - 2 * uncsel, max = ysel + 2 * uncsel)) + 1E-16))])
            } else {
              return(uncSeq[sample.int(20, size = 1, prob = exp(log(dALDFast(uncSeq, mu = XBeta[x], sigma = 1 / delta, p = q)) +
                                                           log(dnorm(uncSeq, mean = ysel,sd = uncsel)) + 1E-16))])
            }
          } else {
            return(ysel)
          }
        }, c(0))
      }
      betamin <- beta
      if(bs == "ds"){
        if (MinMax == "Max"){
          diffVal <- max(c(0, data$Y2 - XBeta))
          betamin[K-2] <- beta[K-2] + diffVal
          betamin[K-2] <- betamin[K-2] - max(0, (max(c(XBeta + diffVal)) - minValue))
        }
        else{
          diffVal <- max(c(0, XBeta - data$Y2))
          betamin[K-2] <- beta[K-2] - diffVal
          betamin[K-2] <- betamin[K-2] + max(0, (minValue - min(c(XBeta - diffVal))))
        }
      } else {
        if (MinMax == "Max"){
          diffVal <- max(c(0, data$Y2 - XBeta))
          betamin[K] <- beta[K] + diffVal
          betamin[K] <- betamin[K] - max(0, (max(c(XBeta + diffVal)) - minValue))
        }
        else{
          diffVal <- max(c(0, XBeta - data$Y2))
          betamin[K] <- beta[K] - diffVal
          betamin[K] <- betamin[K] + max(0, (minValue - min(c(XBeta - diffVal))))
          }
      }
      #print(betamin[K-2] - beta[K-2])
      #Werte in Parametermatrizen einsetzen
      betamc[i, ] <<- betamin
    }
  }
  if(shinyApp){
    for ( k in 1:10) {
      j <- seq(1, iter, iter / 10)[k]
      showMessage(
        MCMC_Spread,
        msg = "Calculating Spread Model",
        detail = paste0("Chain ", nChains),
        value = k / 10)(
          start = j, iter = j + iter / 10 - 1
        )
    }
  } else {
    MCMC_Spread(1, iter)
  }
  burnin <- round(burnInProp * iter)
  every <- thinning  #nur die x-te MCMC-Iteration soll genutzt werden
  #Vektor der tatsaechlich benutzten Beobachtungen
  usedsamples <- seq(from = burnin, to = iter, by = every)

  return(list(beta = betamc[usedsamples, ], sc = s, sigma = 0, tau = 0,
              pred = XX %*% colMeans(betamc[usedsamples, ]),
              mRe = mRe, sRe = sRe,
              range = list(mean = range(rowMeans(sapply(1:length(usedsamples), function(x)
                (XX %*% betamc[usedsamples[x], ]) * sRe + mRe))),
                se = range(sqrt(apply(sapply(1:length(usedsamples), function(x)
                  (XX %*% betamc[usedsamples[x], ]) * sRe + mRe), 1, var))))))
}

dALDFast <- function(x, mu, sigma, p){
  ret <- x
  ret[x < mu] <- (p * (1 - p) / sigma) * exp((1 - p) * (x[x < mu] - mu)/sigma)
  ret[x >= mu] <- (p * (1 - p) / sigma) * exp(-(p) * (x[x >= mu] - mu)/sigma)
  ret
}

invLogit <- function(x){
  1 / (1+exp(-x))
}

#' Estimates spatial kernel density model
#'
#' @param data data.frame: data
#' @param Longitude character: name of longitude variable
#' @param Latitude character: name of latitude variable
#' @param independent character: name of presence/absence variable (optional)
#' @param CoordType character: type of longitude/latitude coordinates.
#'  One of "decimal degrees", "degrees minutes seconds" and "degrees decimal minutes"
#' @param Weighting character: name of weighting variable
#' @param clusterMethod character: cluster method
#' @param kMeansAlgo character: kmeans algorithm as in stats:kmeans
#' @param nClust numeric: how many clusters
#' @param nClustRange numeric: range of potential mclust cluster
#' @param restriction numeric vector: spatially restricts model data 4 entries for latitude (min/max) and longitude(min/max)
#' @param nSim numeric: number of bootstrap samples
#' @param kdeType character: "1" for correlated bandwidth, "2" for diagonal bandwidth, "3" for diagonal, equal long/lat bandwidth
#' @inheritParams centerData
#' @examples
#' \dontrun{
#' #load data
#' data <- readRDS(system.file("extData", "exampleData.Rds", package = "MpiIsoApp"))
#' # estimate model-map
#' map <- estimateMap(data = data, independent = "d13C", Longitude = "longitude",
#' Latitude = "latitude", Site = "site")
#' # Plot the map
#' plotMap(model = map)
#'
#' # Alternative: use app
#' shiny::runApp(paste0(system.file(package = "MpiIsoApp"),"/app"))
#'
#' }
#' @export
estimateMapKernel <- function(data,
                              Longitude,
                              Latitude,
                              center = c("Europe", "Pacific"),
                              independent = NULL,
                              CoordType = "decimal degrees",
                              Weighting = NULL,
                              clusterMethod = NULL,
                              nClust = 5,
                              nClustRange = c(2,10),
                              kMeansAlgo = "Hartigan-Wong",
                              restriction = c(-90, 90, -180, 180),
                              nSim = 10,
                              kdeType = "1"){
  center <- match.arg(center)

  dataOrg <- data
  if ( is.null(data)) return(NULL)
  if (Longitude == "" || Latitude == "") return(NULL)
  if (!(all(c(Longitude, Latitude) %in% names(data)))) return(NULL)
  if(!is.null(independent) & !(independent == "")){
    if(!(independent %in% names(data))) return("independent variable is missing in data")
  }

  # prepare data ----
  data <- data %>%
    convertLatLongWrapper(Longitude = Longitude,
                          Latitude = Latitude,
                          CoordType = CoordType)
  # if conversion fails Long/Lat are removed -> columns will be missing
  if (!all(c(Longitude, Latitude) %in% names(data)) ||
      all(is.na(data[, Longitude])) || all(is.na(data[, Latitude])) ) return("Longitude or Latitude not available.")

  # process coordinate data
  data <- data %>%
    shiftDataToDefaultRestriction() %>%
    removeDataOutsideRestriction(Latitude = Latitude,
                                 Longitude = Longitude,
                                 restriction = restriction)

  if(!is.null(independent) & !(independent == "")){
    if(!is.null(Weighting) & !(Weighting == "")){
      if(!is.numeric(data[, Weighting]) || !(all(data[, Weighting] >= 0, na.rm = TRUE))) return("Weights must be non-negative numeric values.")
      data <- na.omit(data[data[, independent] == 1, c(independent, Longitude, Latitude, Weighting)])
    } else {
      data <- na.omit(data[data[, independent] == 1, c(independent, Longitude, Latitude)])
    }
    if(!is.numeric(data[, independent]) || !(all(data[, independent] %in% c(0,1), na.rm = TRUE))) return("presence/absence variable must be numeric 0 or 1 values")
    if(sum(data[,independent], na.rm = TRUE) == 0) return("At least one present observation must be included in presence/absence variable.")
  } else {
    if(!is.null(Weighting) & !(Weighting == "")){
      if(!is.numeric(data[, Weighting]) || !(all(data[, Weighting] >= 0, na.rm = TRUE))) return("Weights must be non-negative numeric values.")
      data <- na.omit(data[, c(Longitude, Latitude, Weighting)])
    } else {
      data <- na.omit(data[, c(Longitude, Latitude)])
    }
  }

  if(nrow(data) <= 2) return("Not enough data available.")
  data$Longitude <- data[, Longitude]
  data$Latitude <- data[, Latitude]

  ### data augmentation ----
  data2 <- data %>%
    augmentData(restriction = c(-120, 120, -240, 240)) %>%
    centerData(center = center)

  # calculate model ----
  set.seed(1234)
  if(clusterMethod == "kmeans"){
    clust <- kmeans(cbind(data2$Longitude, data2$Latitude), nClust, nstart = 25, algorithm = kMeansAlgo)
    data2$cluster <- clust$cluster
    clust <- as.data.frame(clust$centers)
    names(clust) <- c("long_centroid_spatial_cluster", "lat_centroid_spatial_cluster")
    clust$cluster <- 1:nrow(clust)
    data2 <- merge(data2, clust, sort = FALSE)
    colnames(data2)[colnames(data2)=="cluster"] <- "spatial_cluster"
  } else if (clusterMethod == "mclust"){

    numClusters <- seq(nClustRange[1],nClustRange[2])
    cluster_list <- vector("list", length(numClusters))
    for(i in 1:length(numClusters)){
        set.seed(1234)
      cluster_list[[i]] <- mclust::Mclust(data2[,c("Longitude","Latitude")], G = numClusters[i])
    }

    # select best cluster solution based on bic
    cluster_solution <- cluster_list[[which.max(sapply(1:length(cluster_list),
                                             function(x) cluster_list[[x]]$bic))]]

    # assign cluster to data
    data2$cluster <- cluster_solution$classification

    # merge cluster centers
    cluster_centers <- data.frame(t(cluster_solution$parameters$mean))
    colnames(cluster_centers) <- c("long_centroid_spatial_cluster", "lat_centroid_spatial_cluster")
    cluster_centers$cluster <- 1:nrow(cluster_centers)
    data2 <- merge(data2, cluster_centers, sort = FALSE)
    colnames(data2)[colnames(data2)=="cluster"] <- "spatial_cluster"
  }
  if(!is.null(Weighting) & !(Weighting == "")){
    model <- try(lapply(1:nSim, function(x){
      data3 <- data2[sample(1:nrow(data2), nrow(data2), replace = T), ]
      if(kdeType == "1"){
        H = Hpi(cbind(data3$Longitude, data3$Latitude))
      }
      if(kdeType == "2"){
        H = Hpi.diag(cbind(data3$Longitude, data3$Latitude))
      }
      if(kdeType == "3"){
        H = Hpi.diag(cbind(data3$Longitude, data3$Latitude))
        diag(H) <- prod(diag(H)^0.5)
      }

      kde(cbind(data3$Longitude, data3$Latitude), w = data3[,Weighting], H=H)
    }),
    silent = TRUE)
  } else {
    model <- try(lapply(1:nSim, function(x){
      data3 <- data2[sample(1:nrow(data2), nrow(data2), replace = T), ]
      if(kdeType == "1"){
        H = Hpi(cbind(data3$Longitude, data3$Latitude))
      }
      if(kdeType == "2"){
        H = Hpi.diag(cbind(data3$Longitude, data3$Latitude))
      }
      if(kdeType == "3"){
        H = Hpi.diag(cbind(data3$Longitude, data3$Latitude))
        diag(H) <- prod(diag(H)^0.5)
      }

      kde(cbind(data3$Longitude, data3$Latitude), H=H)
    }), silent = TRUE)
  }
  if ( class(model)[1] == "try-error") {return("Error in Model Fitting.")}
  sc <- NULL
  class(model) <- c(class(model), "kde")
  return(list(model = model, data = data, sc = sc, independent = independent))
}

estimateMapKernelWrapper <- function(data, input) {
    if(input$modelArea){
      restriction <- c(input$mALat1, input$mALat2, input$mALong1, input$mALong2)
      restriction[is.na(restriction)] <- c(-90, 90, -180, 180)[is.na(restriction)]
    } else {
      restriction <- c(-90, 90, -180, 180)
    }

    estimateMapKernel(data = data, independent = input$IndependentX,
                Longitude = input$Longitude, Latitude = input$Latitude, center = input$centerOfData,
                CoordType = input$CoordType,
                Weighting = input$Weighting,
                clusterMethod = input$clusterMethod,
                nClust = input$nClust,
                nClustRange = input$nClustRange,
                kMeansAlgo = input$kMeansAlgo,
                restriction = restriction,
                nSim = input$nSim,
                kdeType = input$kdeType)
}


#' Estimates spatio-temporal kernel density model
#'
#' @param data data.frame: data
#' @param Longitude character: name of longitude variable
#' @param Latitude character: name of latitude variable
#' @param DateOne character: name of date variable 1 (lower interval point / mean / single point)
#' @param DateTwo character: name of date variable 2 (upper interval point / sd / )
#' @param independent character: name of presence/absence variable (optional)
#' @param DateType character: one of "Interval", "Mean + 1 SD uncertainty" and "Single Point"
#' @param CoordType character: type of longitude/latitude coordinates.
#'  One of "decimal degrees", "degrees minutes seconds" and "degrees decimal minutes"
#' @param Weighting character: name of weighting variable
#' @param clusterMethod character: cluster method
#' @param nClust numeric: how many clusters
#' @param nClustRange numeric: range of potential mclust cluster
#' @param kMeansAlgo character: kmeans algorithm as in stats:kmeans
#' @param clusterTimeRange numeric vector: time range of cluster
#' @param modelUnc boolean: Include dating uncertainty
#' @param dateUnc character: one of "uniform", "normal", "point"
#' @param restriction numeric vector: spatially restricts model data 4 entries for latitude (min/max) and longitude(min/max)
#' @param nSim numeric: number of bootstrap samples
#' @param kdeType character: "1" for correlated bandwidth, "2" for diagonal bandwidth, "3" for diagonal, equal long/lat bandwidth
#' @inheritParams centerData
#' @examples
#' \dontrun{
#' # load data
#' data <- readRDS(system.file("extData", "exampleData.Rds", package = "MpiIsoApp"))
#' # estimate model-map
#' map <- estimateMap3D(data = data, independent = "d13C", Longitude = "longitude",
#' Latitude = "latitude", DateOne = "dateLower", DateTwo = "dateUpper", Site = "site")
#' # Plot the map
#' plotMap3D(model = map, time = median(data$dateLower, na.rm = TRUE))
#' }
#' @export
estimateMap3DKernel <- function(data,
                                Longitude,
                                Latitude,
                                DateOne,
                                DateTwo,
                                center = c("Europe", "Pacific"),
                                independent = NULL,
                                DateType = "Interval",
                                CoordType = "decimal degrees",
                                Weighting = NULL,
                                clusterMethod = NULL,
                                nClust = 5,
                                nClustRange = c(2,10),
                                kMeansAlgo = "Hartigan-Wong",
                                clusterTimeRange = c(0,1000),
                                modelUnc = FALSE,
                                dateUnc = "point",
                                restriction = c(-90, 90, -180, 180),
                                nSim = 10,
                                kdeType = "1") {
  center <- match.arg(center)

  if (is.null(data)) return(NULL)
  if (Longitude == "" || Latitude == "" || DateOne == "") return(NULL)
  if (!(all(c(Longitude, Latitude, DateOne) %in% names(data)))) return(NULL)
  if(!is.null(independent) & !(independent == "")){
    if(!(independent %in% names(data))) return("independent variable is missing in data")
  }

  # prepare data ----
  data <- data %>%
    prepareDate(DateOne = DateOne,
                DateTwo = DateTwo,
                DateType = DateType,
                dateUnc = dateUnc,
                useMaxUnc = FALSE)
  if (all(is.na(data[, DateOne]))) return("non-numeric date field 1 variable")
  if (DateType != "Single point" && (all(is.na(data[, DateTwo])))) return("non-numeric date field 2 variable")

  data <- data %>%
    convertLatLongWrapper(Longitude = Longitude,
                          Latitude = Latitude,
                          CoordType = CoordType)
  # if conversion fails Long/Lat are removed -> columns will be missing
  if (!all(c(Longitude, Latitude) %in% names(data)) ||
      all(is.na(data[, Longitude])) || all(is.na(data[, Latitude])) ) return("Longitude or Latitude not available.")

  # process coordinate data
  data <- data %>%
    shiftDataToDefaultRestriction() %>%
    removeDataOutsideRestriction(Latitude = Latitude,
                                 Longitude = Longitude,
                                 restriction = restriction)

  if (DateType == "Interval"){
    if(!is.null(independent) & !(independent == "")){
      if(!is.null(Weighting) & !(Weighting == "")){
        if(!is.numeric(data[, Weighting]) || !(all(data[, Weighting] >= 0, na.rm = TRUE))) return("Weights must be non-negative numeric values.")
        data <- na.omit(data[data[, independent] == 1, c(independent, Longitude, Latitude, Weighting, "Date", "Uncertainty")])
      } else {
        data <- na.omit(data[data[, independent] == 1, c(independent, Longitude, Latitude, "Date", "Uncertainty")])
      }
      if(!is.numeric(data[, independent]) || !(all(data[, independent] %in% c(0,1), na.rm = TRUE))) return("presence/absence variable must be numeric 0 or 1 values")
      if(sum(data[,independent], na.rm = TRUE) == 0) return("At least one present observation must be included in presence/absence variable.")
    } else {
      if(!is.null(Weighting) & !(Weighting == "")){
        if(!is.numeric(data[, Weighting]) || !(all(data[, Weighting] >= 0, na.rm = TRUE))) return("Weights must be non-negative numeric values.")
        data <- na.omit(data[, c(Longitude, Latitude, "Date", "Uncertainty", Weighting)])

      } else {
        data <- na.omit(data[, c(Longitude, Latitude,
                                 "Date", "Uncertainty")])
      }
    }
    data$Uncertainty2 <- pmax(0, data$Uncertainty / sd(data$Date))
  }
  if (DateType == "Single point"){
    if(!is.null(independent) & !(independent == "")){
      if(!is.null(Weighting) & !(Weighting == "")){
        if(!is.numeric(data[, Weighting]) || !(all(data[, Weighting] >= 0, na.rm = TRUE))) return("Weights must be non-negative numeric values.")
        data <- na.omit(data[data[, independent] == 1, c(independent, Longitude, Latitude, Weighting, "Date", "Uncertainty")])
      } else {
        data <- na.omit(data[data[, independent] == 1, c(independent, Longitude, Latitude, "Date", "Uncertainty")])
      }
      if(!is.numeric(data[, independent]) || !(all(data[, independent] %in% c(0,1), na.rm = TRUE))) return("presence/absence variable must be numeric 0 or 1 values")
      if(sum(data[,independent], na.rm = TRUE) == 0) return("At least one present observation must be included in presence/absence variable.")
    } else {
      if(!is.null(Weighting) & !(Weighting == "")){
        if(!is.numeric(data[, Weighting]) || !(all(data[, Weighting] >= 0, na.rm = TRUE))) return("Weights must be non-negative numeric values.")
        data <- na.omit(data[, c(Longitude, Latitude, "Date", "Uncertainty", Weighting)])

      } else {
        data <- na.omit(data[, c(Longitude, Latitude,
                                 "Date", "Uncertainty")])
      }
    }
    data$Uncertainty2 <- 0
  }
  if (DateType == "Mean + 1 SD uncertainty"){
    if(!is.null(independent) & !(independent == "")){
      if(!is.null(Weighting) & !(Weighting == "")){
        if(!is.numeric(data[, Weighting]) || !(all(data[, Weighting] >= 0, na.rm = TRUE))) return("Weights must be non-negative numeric values.")
        data <- na.omit(data[data[, independent] == 1, c(independent, Longitude, Latitude, Weighting, "Date", "Uncertainty")])
      } else {
        data <- na.omit(data[data[, independent] == 1, c(independent, Longitude, Latitude, "Date", "Uncertainty")])
      }
      if(!is.numeric(data[, independent]) || !(all(data[, independent] %in% c(0,1), na.rm = TRUE))) return("presence/absence variable must be numeric 0 or 1 values")
      if(sum(data[,independent], na.rm = TRUE) == 0) return("At least one present observation must be included in presence/absence variable.")
    } else {
      if(!is.null(Weighting) & !(Weighting == "")){
        if(!is.numeric(data[, Weighting]) || !(all(data[, Weighting] >= 0, na.rm = TRUE))) return("Weights must be non-negative numeric values.")
        data <- na.omit(data[, c(Longitude, Latitude, "Date", "Uncertainty", Weighting)])

      } else {
        data <- na.omit(data[, c(Longitude, Latitude,
                                 "Date", "Uncertainty")])
      }
    }
    data$Uncertainty2 <- pmax(0, data$Uncertainty / sd(data$Date))
  }

  data$Longitude2 <- (data[, Longitude] - mean(data[, Longitude])) / (sd(data[, Longitude]))
  data$Latitude2 <- (data[, Latitude] - mean(data[, Latitude])) / (sd(data[, Latitude]))
  data$Date2 <- (data$Date - mean(data$Date)) / (sd(data$Date))

  data$Longitude <- data[, Longitude]
  data$Latitude <- data[, Latitude]

  ### data augmentation ----
  data2 <- data %>%
    augmentData(restriction = c(-120, 120, -240, 240)) %>%
    centerData(center = center)

  # calculate model ----
  set.seed(1234)
  if(!is.null(Weighting) & !(Weighting == "")){
    model <- try(lapply(1:nSim, function(x){
      data3 <- data2[sample(1:nrow(data2), nrow(data2), replace = T), ]
      if(modelUnc){
        data3$Date2 <- data3$Date2 + rnorm(length(data3$Date2), 0, data3$Uncertainty2)
      }
      if(kdeType == "1"){
        H = Hpi(cbind(data3$Longitude, data3$Latitude, data3$Date2))
      }
      if(kdeType == "2"){
        H = Hpi.diag(cbind(data3$Longitude, data3$Latitude, data3$Date2))
      }
      if(kdeType == "3"){
        H = Hpi.diag(cbind(data3$Longitude, data3$Latitude, data3$Date2))
        diag(H)[1:2] <- prod(diag(H)[1:2]^0.5)
      }

      kde(cbind(data3$Longitude, data3$Latitude, data3$Date2), H= H, w = data3[,Weighting])
    }), silent = TRUE)
  } else {
    model <- try(lapply(1:nSim, function(x){
      data3 <- data2[sample(1:nrow(data2), nrow(data2), replace = T), ]
      if(modelUnc){
        if(dateUnc == "normal"){
        data3$Date2 <- data3$Date2 + rnorm(length(data3$Date2), 0, data3$Uncertainty2)
        } else {
          data3$Date2 <- data3$Date2 + runif(length(data3$Date2), data3$Date2 - 2*data3$Uncertainty2, data3$Date2 + 2*data3$Uncertainty2)
        }
      }
      if(kdeType == "1"){
        H = Hpi(cbind(data3$Longitude, data3$Latitude, data3$Date2))
      }
      if(kdeType == "2"){
        H = Hpi.diag(cbind(data3$Longitude, data3$Latitude, data3$Date2))
      }
      if(kdeType == "3"){
        H = Hpi.diag(cbind(data3$Longitude, data3$Latitude, data3$Date2))
        diag(H) <- prod(diag(H)^0.5)
      }
      kde(cbind(data3$Longitude, data3$Latitude, data3$Date2), H = H)}), silent = TRUE)
  }
  if(clusterMethod == "kmeans"){
# K-Means Clustering ----
    # discussion about the clustering here: https://github.com/Pandora-IsoMemo/iso-app/issues/54
    # In KernelTimeR clustering is applied to filtered data according to clusterTimeRange.
    # After clusters have been calculated the algorithm designed by marcus is recalculating the cluster centers by
    # finding the point with the highest density in each cluster, while the density also takes the temporal aspect into account.
    # In the last step all data points (not only the filtered points) are assigned to the cluster.
    # This is done by assigning the cluster to the point for which the distance of the cluster center is closest.

    # In the map, per default, all points are displayed. In the excel export only the filtered dataset is included.

    data$id <- 1:nrow(data)
    set.seed(1234)
    ## Clustering on filtered data ----
    dataC <- data[((data$Date - 2*data$Uncertainty) <= clusterTimeRange[2] & (data$Date - 2*data$Uncertainty) >= clusterTimeRange[1]) |
                     ((data$Date + 2*data$Uncertainty) <= clusterTimeRange[2] & (data$Date + 2*data$Uncertainty) >= clusterTimeRange[1]) |
                     ((data$Date) <= clusterTimeRange[2] & (data$Date) >= clusterTimeRange[1]), ]
    clust <- kmeans(cbind(dataC$Longitude, dataC$Latitude), nClust, nstart = 25, algorithm = kMeansAlgo)

    ## Clustering on full data (implemented but then removed again) ----
    # clust_full <- kmeans(cbind(data$Longitude, data$Latitude), nClust, nstart = 25, algorithm = kMeansAlgo)
    #
    # Add centroids to data ----
    ## Full data ----
    # clust_full_centroid <- data.frame(cluster=1:nrow(clust_full$centers),clust_full$centers)
    # names(clust_full_centroid) <- c("cluster","long_cluster_all_centroid","lat_cluster_all_centroid")
    # data$cluster <- clust_full$cluster
    # data <- merge(data, clust_full_centroid, by = "cluster", sort = FALSE)
    # data$cluster <- NULL

    ## Filtered data ----
    dataC$cluster <- clust$cluster
    clust_centroid <- data.frame(cluster=1:nrow(clust$centers),clust$centers)
    names(clust_centroid) <- c("cluster","long_centroid_spatial_cluster","lat_centroid_spatial_cluster")
    dataC <- merge(dataC, clust_centroid, by = "cluster", sort = FALSE)
    data <- data %>% left_join(dataC[,c("id","cluster","long_centroid_spatial_cluster","lat_centroid_spatial_cluster")], by = "id")
    data$id <- NULL
    colnames(data)[colnames(data)=="cluster"] <- "spatial_cluster"
    dataC$cluster <- NULL
    dataC <- dataC[order(dataC$id),]

    ## Optimal Centroids ----
    clustDens <- sapply(1:nrow(dataC), function(z) {rowMeans(sapply(1:nSim, function(k) predict(model[[k]], x = cbind(dataC[rep(z, 100), c("Longitude", "Latitude")],
                                  Date2 = (seq(clusterTimeRange[1], clusterTimeRange[2],
                                               length.out = 100) - mean(data$Date)) / (sd(data$Date))))))})
    dataC$cluster <- clust$cluster

    densM <- colMeans(clustDens)
    densSD <- apply(clustDens, 2, sd)
    densQ <- densM / densSD

    clusterCentroids <- do.call("rbind", (lapply(1:nClust, function(j){
      dataC[dataC$cluster == j, ][which.max(densQ[dataC$cluster == j]), c("Longitude", "Latitude")]
    })))
    #clust <- kmeans(cbind(dataC$Longitude, dataC$Latitude), centers = clusterCentroids, iter = 0)

    data$cluster <- sapply(1:nrow(data),
                           function(x) which.min(rowSums((data[rep(x, nClust), c("Longitude", "Latitude")] -
                                                        as.matrix(clusterCentroids))^2)))

    clust <- clusterCentroids
    names(clust) <- c("long_temporal_group_reference_point", "lat_temporal_group_reference_point")
    clust$cluster <- 1:nrow(clust)
    data <- merge(data, clust, sort = FALSE)
    colnames(data)[colnames(data)=="cluster"] <- "temporal_group"
  } else if (clusterMethod == "mclust"){
  # MCLUST Clustering ----
    data$id <- 1:nrow(data)

    # Clustering on filtered data
    dataC <- data[((data$Date - 2*data$Uncertainty) <= clusterTimeRange[2] & (data$Date - 2*data$Uncertainty) >= clusterTimeRange[1]) |
                    ((data$Date + 2*data$Uncertainty) <= clusterTimeRange[2] & (data$Date + 2*data$Uncertainty) >= clusterTimeRange[1]) |
                    ((data$Date) <= clusterTimeRange[2] & (data$Date) >= clusterTimeRange[1]), ]

    numClusters <- seq(nClustRange[1],nClustRange[2])
    cluster_list <- vector("list", length(numClusters))
    for(i in 1:length(numClusters)){
      set.seed(1234)
      cluster_list[[i]] <- mclust::Mclust(dataC[,c("Longitude","Latitude")], G = numClusters[i])
    }

    # select best cluster solution based on bic
    best_solution_idx <- which.max(sapply(1:length(cluster_list),function(x) cluster_list[[x]]$bic))
    best_solution_cluster <- numClusters[[best_solution_idx]]
    cluster_solution <- cluster_list[[best_solution_idx]]

    ## Clustering on full data (implemented but then removed again) ----
    # set.seed(1234)
    # clust_full <- mclust::Mclust(data[,c("Longitude","Latitude")], G = best_solution_cluster)
    #
    # # Add centroids to data
    # # Full data
    # clust_full_centroid <- data.frame(cluster=1:nrow(t(clust_full$parameters$mean)),t(clust_full$parameters$mean))
    # names(clust_full_centroid) <- c("cluster","long_cluster_all_centroid","lat_cluster_all_centroid")
    # data$cluster <- clust_full$classification
    # data <- merge(data, clust_full_centroid, by = "cluster", sort = FALSE)
    # data$cluster <- NULL

    ## Filtered data ----
    dataC$cluster <- cluster_solution$classification
    clust_centroid <- data.frame(cluster=1:nrow(t(cluster_solution$parameters$mean)),t(cluster_solution$parameters$mean))
    names(clust_centroid) <- c("cluster","long_centroid_spatial_cluster","lat_centroid_spatial_cluster")
    dataC <- merge(dataC, clust_centroid, by = "cluster", sort = FALSE)
    data <- data %>% left_join(dataC[,c("id","cluster","long_centroid_spatial_cluster","lat_centroid_spatial_cluster")], by = "id")
    data$id <- NULL
    colnames(data)[colnames(data)=="cluster"] <- "spatial_cluster"
    dataC$cluster <- NULL
    dataC <- dataC[order(dataC$id),]

    ## optimal centroids: ----
    clustDens <- sapply(1:nrow(dataC), function(z) {rowMeans(sapply(1:nSim, function(k) predict(model[[k]], x = cbind(dataC[rep(z, 100), c("Longitude", "Latitude")],
                                                                                                                      Date2 = (seq(clusterTimeRange[1], clusterTimeRange[2],
                                                                                                                                   length.out = 100) - mean(data$Date)) / (sd(data$Date))))))})
    # assign cluster to data
    dataC$cluster <- cluster_solution$classification

    densM <- colMeans(clustDens)
    densSD <- apply(clustDens, 2, sd)
    densQ <- densM / densSD

    clusterCentroids <- do.call("rbind", (lapply(1:best_solution_cluster, function(j){
      dataC[dataC$cluster == j, ][which.max(densQ[dataC$cluster == j]), c("Longitude", "Latitude")]
    })))

    data$cluster <- sapply(1:nrow(data),
                           function(x) which.min(rowSums((data[rep(x, best_solution_cluster), c("Longitude", "Latitude")] -
                                                            as.matrix(clusterCentroids))^2)))
    if(length(unique(data$cluster)) < length(unique(dataC$cluster))){
    showNotification(paste0("Note: mclust selected ",length(unique(dataC$cluster))," cluster. However the temporal algorithm assigned all data to only ",length(unique(data$cluster))," of these clusters."))
    }

    clust <- clusterCentroids
    names(clust) <- c("long_temporal_group_reference_point", "lat_temporal_group_reference_point")
    clust$cluster <- 1:nrow(clust)
    data <- merge(data, clust, sort = FALSE)
    colnames(data)[colnames(data)=="cluster"] <- "temporal_group"
  }
  if ( class(model)[1] == "try-error") {return("Error in Model Fitting.")}
  sc <- NULL
  class(model) <- c(class(model), "kde")
  return(list(model = model, data = data, sc = sc, independent = independent))
}

estimateMap3DKernelWrapper <- function(data, input) {
  if(input$modelArea){
    restriction <- c(input$mALat1, input$mALat2, input$mALong1, input$mALong2)
    restriction[is.na(restriction)] <- c(-90, 90, -180, 180)[is.na(restriction)]
  } else {
    restriction <- c(-90, 90, -180, 180)
  }

  estimateMap3DKernel(
    data = data, independent = input$IndependentX,
    Longitude = input$Longitude, Latitude = input$Latitude, center = input$centerOfData,
    CoordType = input$coordType, DateOne = input$DateOne,
    DateTwo = input$DateTwo, DateType = input$DateType,
    Weighting = input$Weighting,
    clusterMethod = NULL,
    dateUnc = input$dateUnc,
    kMeansAlgo = input$kMeansAlgo,
    nClust = input$nClust,
    nClustRange = input$nClustRange,
    clusterTimeRange = input$timeClust,
    modelUnc = input$modelUnc,
    restriction = restriction,
    nSim = input$nSim,
    kdeType = input$kdeType
  )
}

estimateModel2D <- function(data2, fm, independent, penalty, K, bs){
  if(length(unique(data2$Site)) < nrow(data2)){
    model <- try(gamm(as.formula(paste(independent, " ~ s(Latitude, Longitude, m =", penalty, ",k = K, bs =",bs, ")")),
                      random = list(Site = ~ 1), data = data2, family = fm), silent = TRUE)
    if(!is.null(data2$independentUncertainty)){
      weights <- pmax(1E-7,pmax(data2$independentUncertainty ^ 2, (data2$independentUncertainty ^ 2 + sd(residuals(model$gam)) ^ 2 - mean(data2$independentUncertainty ^ 2 ))))
      model <- try(gamm(as.formula(paste(independent, " ~ s(Latitude, Longitude, m =", penalty, ",k = K, bs =",bs, ")")),
                        random = list(Site = ~ 1), data = data2, weights = weights, family = fm), silent = TRUE)
    }
  } else {
    model <- try(gamm(as.formula(paste(independent, " ~ s(Latitude, Longitude, m =", penalty, ",k = K, bs =",bs, ")")),
                      data = data2, family = fm), silent = TRUE)
    if(!is.null(data2$independentUncertainty)){
      weights <- pmax(1E-7,pmax(data2$independentUncertainty ^ 2, (data2$independentUncertainty ^ 2 + sd(residuals(model$gam)) ^ 2 - mean(data2$independentUncertainty ^ 2 ))))
      model <- try(gamm(as.formula(paste(independent, " ~ s(Latitude, Longitude, m =", penalty, ",k = K, bs =",bs, ")")),
                        data = data2, weights = weights, family = fm), silent = TRUE)
    }
  }
  return(model)
}

estimateModel3D <- function(data2, fm, independent, splineExpr){
  if(length(unique(data2$Site)) < nrow(data2)){
    model <- try(gamm(as.formula(paste(independent, " ~ ", splineExpr)),
                      random = list(Site = ~ 1), data = data2, family = fm), silent = TRUE)
    if(!is.null(data2$independentUncertainty)){
      weights <- pmax(1E-7,pmax(data2$independentUncertainty ^ 2, (data2$independentUncertainty ^ 2 + sd(residuals(model$gam)) ^ 2 - mean(data2$independentUncertainty ^ 2 ))))
      model <- try(gamm(as.formula(paste(independent, " ~ ", splineExpr)),
                      random = list(Site = ~ 1), data = data2, weights = weights, family = fm), silent = TRUE)
    }
  } else {
    model <- try(gamm(as.formula(paste(independent, " ~ ", splineExpr)),
                    data = data2, family = fm), silent = TRUE)
    if(!is.null(data2$independentUncertainty)){
      weights <- pmax(1E-7,pmax(data2$independentUncertainty ^ 2, (data2$independentUncertainty ^ 2 + sd(residuals(model$gam)) ^ 2 - mean(data2$independentUncertainty ^ 2 ))))
      model <- try(gamm(as.formula(paste(independent, " ~ ", splineExpr)),
                      data = data2, weights = weights, family = fm), silent = TRUE)
    }
  }
return(model)
}

#' Prepare Date
#'
#' Adds new columns 'Date' and 'Uncertainty' that are used in the model dependent on user inputs.
#'
#' @inheritParams estimateMapSpread
#' @param useMaxUnc (logical) True if max uncertainty should be used.
prepareDate <- function(data, DateOne, DateTwo, DateType, dateUnc, useMaxUnc = TRUE) {
  # check date columns
  if (!is.numeric(data[, DateOne])) {
    data[, DateOne] <- as.numeric(data[, DateOne])
  }
  if (all(is.na(data[, DateOne]))) return(data)

  if (DateType != "Single point" && (!is.numeric(data[, DateTwo]))) {
    data[, DateTwo] <- as.numeric(data[, DateTwo])
  }

  if (DateType != "Single point" && (all(is.na(data[, DateTwo])))) return(data)

  # get date uncertainty
  if (DateType == "Interval"){
    data$Date <- (data[, DateTwo] + data[, DateOne]) / 2
    data$Uncertainty <- abs(data[, DateOne] - data[, DateTwo]) / 4
    if (useMaxUnc) data$Uncertainty <- pmax(0, data$Uncertainty)
    if(dateUnc == "normal2"){
      dateUnc <- "normal"
      data$Uncertainty <- data$Uncertainty / 2
    }
  }

  if (DateType == "Single point"){
    data$Date <- data[, DateOne]
    data$Uncertainty <- 0
  }

  if (DateType == "Mean + 1 SD uncertainty"){
    data$Date <- data[, DateOne]
    data$Uncertainty <- data[, DateTwo]
    if (useMaxUnc) data$Uncertainty <- pmax(0, data$Uncertainty)
    if(dateUnc == "uniform2"){
      dateUnc <- "uniform"
      data$Uncertainty <- data$Uncertainty / 2
    }
  }

  data
}
