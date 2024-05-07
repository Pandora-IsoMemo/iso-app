#' Plots map of a spatial average or spread model from estimateMap() or estimateMapSpread() functions
#'
#' @param model return object of spatial or spread model
#'  from estimateMap() or estimateMapSpread() functions
#' @param IndSelect for categorical model: selected category; shifts between categories in the center
#' @param arrow display north arrow TRUE/FALSE
#' @param scale display scale TRUE/FALSE
#' @param terrestrial show only estimates on land masses (1), oceans (-1) or all (0)
#' @param resolution spatial grid resolution of displayed (higher is slower but better quality)
#' @param interior show only convex hull TRUE/FALSE
#' @param grid show coordinate grid TRUE/FALSE
#' @param ncol number of colors for estimates
#' @param colors color scheme of estimates from RColorBrewer. defaults to "RdYlGn"
#' @param reverseColors inverse colour scheme
#' @param mapType Type of map. "Map" for simple estimates. "speed" for gradient
#'  (mainly for spread model)
#' @param estType one of "Mean", "SE" and "Quantile". defaults to "Mean"
#' @param estQuantile quantile (only applicable if estType = "Quantile")
#' @param points show points on map TRUE/FALSE
#' @param pColor color of location marks drawn
#' @param colorsP color scale of location marks
#' @param pointShape pch shape of location marks
#' @param pointSize point size (if points = TRUE)
#' @param centerMap center of map, one of "Europe" and "Pacific"
#' @param StdErr maximum standard error to which estimates are displayed
#' @param rangex range of longitude values (x axis limits)
#' @param rangey range of latitude values (y axis limits)
#' @param mask boolean: Show output within range of points
#' @param maskRadius numeric: Show output within range of points in km
#' @param limitz restrict range of z (possible values "0-1", "0-100")
#' @param rangez range of estimated values (z axis limits)
#' @param dataCenter optional data.frame with two columns (longitude / latitude) for batch estimates
#' of a series of spatial points
#' @param RadiusBatch radius of batch spatial point estimates
#' @param textLabels text labels
#' @param pointLabels point size labels
#' @param pointColLabels point colour labels
#' @param fontSize font size
#' @param showScale show colour scale
#' @param setAxisLabels show axis/main/scale labels
#' @param mainLabel main label
#' @param yLabel y lab label
#' @param xLabel y lab label
#' @param scLabel scale lab label
#' @param northSize size of north arrow
#' @param scalSize size of scale
#' @param scaleX scale x orientation
#' @param scaleY scale y orientation
#' @param NorthX north arrow x orientation
#' @param NorthY north arrow y orientation
#' @param showModel show model
#' @param fontType font type
#' @param fontCol font color
#' @param titleMain show main title
#' @param titleScale show scale title
#' @param AxisSize axis title font size
#' @param AxisLSize axis label font size
#' @param cluster show clusters
#' @param clusterCol Cluster colors
#' @param pointDat data frame of points to add to plot
#' @param MinMax Min or Max, only for spread model
#' @param nMin Number of minima to compare, only for spread model
#' @param minDist Distance between minima/maxima, only for spread model
#' @param showMinOnMap Show minima on map yes/no, only for spread model if maptype = "Minima/Maxima"
#' @inheritParams filled.contour2
#'
#' @export
plotMap <- function(model,
                    IndSelect = NULL,
                    estType = "Mean", estQuantile = 0.9,
                    points = TRUE, pointSize = 1, StdErr = Inf,
                    rangex = range(model$data$Longitude, na.rm = TRUE),
                    rangey = range(model$data$Latitude, na.rm = TRUE),
                    rangez = NULL,
                    contourType = c("filled.contour", "contour"),
                    limitz = NULL, resolution = 100, interior = TRUE,
                    ncol = 10, colors = "RdYlGn", reverseColors = FALSE,
                    colorsP = NULL,
                    centerMap = "Europe",
                    pointShape = 4,
                    showScale = TRUE,
                    textLabels = NULL,
                    pointLabels = NULL,
                    pointColLabels = NULL,
                    fontSize = 11,
                    showModel = TRUE,
                    fontType = "sans",
                    fontCol = "black",
                    mask = FALSE,
                    maskRadius = 100,
                    pColor = "#000000",
                    dataCenter = NULL, RadiusBatch = NULL, terrestrial = 1,
                    grid = TRUE, arrow = TRUE, scale = TRUE, mapType = "Map",
                    titleMain = TRUE,
                    titleScale = TRUE,
                    setAxisLabels = FALSE,
                    mainLabel = "",
                    yLabel =  "Latitude",
                    xLabel =  "Longitude",
                    northSize = 0.2,
                    scalSize = 0.1,
                    scaleX = 0,
                    scaleY = 0.1,
                    NorthX = 0.025,
                    NorthY = 0.925,
                    scLabel =  "",
                    AxisSize = 1,
                    AxisLSize = 1,
                    cluster = FALSE,
                    clusterCol = "Set1",
                    pointDat = NULL,
                    MinMax = "Min",
                    nMin = 3,
                    minDist = 250,
                    showMinOnMap = FALSE){
  options(scipen=999)
  contourType <- match.arg(contourType)
  minRangeFactor <- 0.75
  if((diff(rangex) / diff(rangey)) < minRangeFactor){
    rangex[1] <- max(-180, mean(rangex) - minRangeFactor / 2 * diff(rangey))
    rangex[2] <- min(180, mean(rangex) + minRangeFactor / 2 * diff(rangey))
  }
  minRangeFactor <- 0.5
  if((diff(rangey) / diff(rangex)) < minRangeFactor){
    rangey[1] <- max(-90, mean(rangey) - minRangeFactor / 2 * diff(rangex))
    rangey[2] <- min(90, mean(rangey) + minRangeFactor / 2 * diff(rangex))
  }
  independent <- model$independent

  if(!is.null(model$IndependentType) && model$IndependentType != "numeric"){
    if(IndSelect == "" | is.null(IndSelect)){
      return(NULL)
    }
    model$model <- model$model[[IndSelect]]
  }

  if(is.null(rangez)){
    rangez = range(model$data[, independent], na.rm = TRUE)
  }

  Bayes = TRUE
  GAM = FALSE
  if("gamm" %in% class(model$model)){
    Bayes <- FALSE
  }
  if("kde" %in% class(model$model)){
    GAM <- TRUE
    independent <- "Density"
  }

  Maps <- loadMaps()
  data <- model$data

  if(!is.null(textLabels)){
    textLabels <- textLabels[as.numeric(rownames(data)),1]
  }
  if(!is.null(pointLabels)){
    pointLabels <- pointLabels[as.numeric(rownames(data)),1]
  }
  if(!is.null(pointColLabels)){
    pointColLabels <- pointColLabels[as.numeric(rownames(data)),1]
  }

  sc <- model$sc
  if (is.null(data)) return(NULL)
  RadiusBatch <-  RadiusBatch / 111
  maskRadius <- maskRadius / 111

  longitudes <- seq(max(-180, rangex[1] - 0.1 * diff(rangex)),
                    min(180, rangex[2] + 0.1 * diff(rangex)), length.out = resolution)
  latitudes <- seq(max(-90, rangey[1] - 0.1 * diff(rangey)),
                   min(90, rangey[2] + 0.1 * diff(rangey)), length.out = resolution)
  XPred <- expand.grid(Longitude = longitudes, Latitude = latitudes)

  if(centerMap != "Europe"){
    rangexEU <- rangex
    # rangex[rangexEU < 0] <- rangex[rangexEU < 0] + 180
    # rangex[rangexEU >= 0] <- (-180 + rangex[rangexEU >= 0])
    rangex <- shiftLongitudesToPacific(rangex)
    rangex <- rangex[order(rangex)]
    longitudesPac <- longitudes
    # longitudes[longitudesPac < 0] <- longitudes[longitudesPac < 0] + 180
    # longitudes[longitudesPac >= 0] <- (-180 + longitudes[longitudesPac >= 0])
    longitudes <- shiftLongitudesToPacific(longitudes)
    XPredPac <- XPred
    # XPred$Longitude[XPredPac$Longitude < 0] <- XPred$Longitude[XPredPac$Longitude < 0] + 180
    # XPred$Longitude[XPredPac$Longitude >= 0] <- (-180 + XPred$Longitude[XPredPac$Longitude >= 0])
    XPred$Longitude <- shiftLongitudesToPacific(XPred$Longitude)
    dataPac <- data
    # dataPac$Longitude[data$Longitude < -0] <- dataPac$Longitude[data$Longitude < -0] + 180
    # dataPac$Longitude[data$Longitude >= -0] <- (- 180 + dataPac$Longitude[data$Longitude >= -0])
    dataPac$Longitude <- shiftLongitudesToPacific(dataPac$Longitude)
  }

  if (interior == TRUE){
    if(centerMap != "Europe"){
      poly.pnts <-
        cbind(dataPac[chull(x = dataPac$Longitude, y = dataPac$Latitude), "Longitude"],
              dataPac[chull(x = dataPac$Longitude, y = dataPac$Latitude), "Latitude"])

      poly.pnts <- rbind(poly.pnts, poly.pnts[1, ])
      draw <- point.in.polygon(XPredPac[, 1], XPredPac[, 2], poly.pnts[, 1], poly.pnts[, 2])
    } else {
      poly.pnts <-
        cbind(data[chull(x = data$Longitude, y = data$Latitude), "Longitude"],
              data[chull(x = data$Longitude, y = data$Latitude), "Latitude"])

      poly.pnts <- rbind(poly.pnts, poly.pnts[1, ])

      draw <- point.in.polygon(XPred[, 1], XPred[, 2], poly.pnts[, 1], poly.pnts[, 2])
    }
  }
  if(mask == TRUE){
    maskDraw <- extractMaskDraw(dat = centerPlotData(data, centerMap = centerMap),
                                maskRadius = maskRadius,
                                XPred = centerXPred(XPred = XPred,
                                                    XPredPac = XPredPac,
                                                    centerMap = centerMap))
  }
  if (!is.null(dataCenter)){
    # this is batch mode
    XPred <- do.call("rbind", lapply(1:nrow(dataCenter), function(x) {
      longitudes <- seq(dataCenter[x, 1] - RadiusBatch,
                        dataCenter[x, 1] + RadiusBatch, length.out = 4)
      latitudes <- seq(dataCenter[x, 2] - RadiusBatch,
                       dataCenter[x, 2] + RadiusBatch, length.out = 4)
      XPred <- expand.grid(Longitude = longitudes, Latitude = latitudes)
      cbind(XPred, id = x)
    }))
  }

  XPred <- centerData(XPred, center = centerMap)

  if (Bayes == TRUE & GAM == FALSE){
    PredMatr <- Predict.matrix(sc, data = XPred)

    betas <- model$model$beta
    betaSigma <- model$model$betaSigma

    Predictions <-
      sapply(1:nrow(betas), function(x)
        (PredMatr %*% betas[x, ]) * model$sRe + model$mRe)

    if(!is.null(model$IndependentType) && model$IndependentType != "numeric"){
      Predictions <- invLogit(Predictions)
    }

    if(!is.null(betaSigma)){
      PredMatrV <- Predict.matrix(model$scV, data = XPred)
      PredictionsSigma <-
        rowMeans(sqrt(sapply(1:nrow(betaSigma), function(x)
          exp((PredMatrV %*% betaSigma[x, ])) / model$model$sigma[x]) * model$sRe^2))
    } else {
      if(!is.null(model$IndependentType) && model$IndependentType != "numeric"){
        PredictionsSigma <- sqrt(Predictions * (1-Predictions))
      } else {
        PredictionsSigma <- sqrt(mean(model$model$sigma) * model$sRe^2)
      }
    }
    if(estType == "Mean"){
      Est <- rowMeans(Predictions)
    }
    if(estType == "1 SE"){
      Est <- apply(Predictions, 1, sd)
    }
    if(estType == "2 SE"){
      Est <- apply(Predictions, 1, sd) * 2
    }
    if(estType == "1 SETOTAL"){
      Est <- sqrt(PredictionsSigma^2 + apply(Predictions, 1, sd)^2)
    }
    if(estType == "2 SETOTAL"){
      Est <- sqrt(PredictionsSigma^2 + apply(Predictions, 1, sd)^2) * 2
    }
    if(estType == "1 SD Population"){
      Est <- PredictionsSigma * 1
    }
    if(estType == "2 SD Population"){
      Est <- PredictionsSigma * 2
    }

    if(estType == "Quantile"){
      Est <- apply(Predictions, 1, quantile, estQuantile)
    }
    if(estType == "QuantileTOTAL"){
      Est <- rowMeans(Predictions + qnorm(estQuantile) * sqrt(PredictionsSigma^2 + apply(Predictions, 1, sd)^2))
    }
    XPred <- data.frame(XPred,
                        Est = Est,
                        Sd = apply(Predictions, 1, sd),
                        SDPop = PredictionsSigma,
                        SdTotal = sqrt(PredictionsSigma^2 + apply(Predictions, 1, sd)^2),
                        IntLower = apply(Predictions, 1, quantile, 0.025),
                        IntUpper = apply(Predictions, 1, quantile, 0.975),
                        IntLowerTotal = apply(Predictions + sqrt(PredictionsSigma^2 + apply(Predictions, 1, sd)^2),
                                              1, quantile, 0.025),
                        IntUpperTotal = apply(Predictions + sqrt(PredictionsSigma^2 + apply(Predictions, 1, sd)^2),
                                              1, quantile, 0.975),
                        resError = sqrt(mean(model$model$sigma + model$model$tau) * model$sRe^2))
  }
  if (Bayes == FALSE & GAM == FALSE){
    Est <- predict(model$model$gam, XPred, se.fit = TRUE, type = "response")
    if(estType == "1 SE"){
      Est$fit <- Est$se.fit
    }
    if(estType == "2 SE"){
      Est$fit <- Est$se.fit * 2
    }
    if(!is.null(model$IndependentType) && model$IndependentType != "numeric"){
      varM = Est$fit * (1-Est$fit)
    }  else {
      varM = var(residuals(model$model$gam))
    }
    if(estType == "1 SETOTAL"){
      Est$fit <- sqrt(Est$se.fit^2 + varM)
    }
    if(estType == "2 SETOTAL"){
      Est$fit <- sqrt(Est$se.fit^2 + varM) * 2
    }
    if(estType == "1 SD Population"){
      Est$fit <- sqrt(varM) * 1
    }
    if(estType == "2 SD Population"){
      Est$fit <- sqrt(varM) * 2
    }
    if(estType == "Quantile"){
      Est$fit <- Est$fit + qnorm(estQuantile) * Est$se.fit
    }
    if(estType == "QuantileTOTAL"){
      Est$fit <- Est$fit + qnorm(estQuantile) *
        sqrt(Est$se.fit^2 + varM)
    }

    XPred <- data.frame(XPred,
                        Est = Est$fit,
                        Sd = Est$se.fit,
                        SDPop = sqrt(varM),
                        SdTotal = sqrt(Est$se.fit^2 + varM),
                        IntLower = Est$fit - 1.96 * Est$se.fit,
                        IntUpper = Est$fit + 1.96 * Est$se.fit,
                        IntLowerTotal = Est$fit - 1.96 * sqrt(Est$se.fit^2 + varM),
                        IntUpperTotal = Est$fit + 1.96 * sqrt(Est$se.fit^2 + varM),
                        resError = sqrt(mean((predict(model$model$gam, type = "response") - model$model$gam$y)^2)))
  }
  if(GAM == TRUE){
    Predictions <- sapply(1:length(model$model), function(x) predict(model$model[[x]], x = XPred[, 1:2]))
    if(estType == "Mean"){
      Est <- rowMeans(Predictions)
    }
    if(estType == "1 SE"){
      Est <- apply(Predictions, 1, sd)
    }
    if(estType == "2 SE"){
      Est <- apply(Predictions, 1, sd) * 2
    }

    if(estType == "Quantile"){
      Est <- apply(Predictions, 1, quantile, estQuantile)
    }

    XPred <- data.frame(XPred,
                        Est = Est,
                        Sd = apply(Predictions, 1, sd),
                        IntLower = pmax(0, apply(Predictions, 1, quantile, 0.025)),
                        IntUpper = apply(Predictions, 1, quantile, 0.975))
  }
  if (estType != "1 SE" && estType != "2 SE" &&
      estType != "1 SETOTAL" &&
      estType != "2 SETOTAL" &&
      !is.null(limitz) && !missing(limitz)){
    if (limitz == "0-1"){
      XPred$Est <- pmin(1, XPred$Est)
      XPred$Est <- pmax(0, XPred$Est)
      if(!is.null(XPred$IntLower)){
        XPred$IntLower <- pmax(0, pmin(1, XPred$IntLower))
        XPred$IntUpper <-  pmax(0, pmin(1, XPred$IntUpper))
      }
      if(!is.null(XPred$IntLowerTotal)){
        XPred$IntLowerTotal <- pmax(0, pmin(1, XPred$IntLowerTotal))
        XPred$IntUpperTotal <- pmax(0, pmin(1, XPred$IntUpperTotal))
      }
    }
    if (limitz == "0-100"){
      XPred$Est <- pmin(100, XPred$Est)
      XPred$Est <- pmax(0, XPred$Est)
      if(!is.null(XPred$IntLower)){
        XPred$IntLower <- pmax(0, pmin(100, XPred$IntLower))
        XPred$IntUpper <-  pmax(0, pmin(100, XPred$IntUpper))
      }
      if(!is.null(XPred$IntLowerTotal)){
        XPred$IntLowerTotal <- pmax(0, pmin(100, XPred$IntLowerTotal))
        XPred$IntUpperTotal <- pmax(0, pmin(100, XPred$IntUpperTotal))
      }
    }
  }

  if (!is.null(dataCenter)){
    # this is batch mode
    XPredCenter <- lapply(1:nrow(dataCenter), function(x){
      XPred %>%
        extractXPredCenter(centerX = dataCenter[x, 1],
                           centerY = dataCenter[x, 2],
                           Radius = RadiusBatch)
    })

    centerEstimates <- lapply(1:nrow(dataCenter), function(x){
      XPredCenter[[x]] %>%
        extractCenterEstimates(batch = TRUE)
    })
    meanCenter <- sapply(1:nrow(dataCenter), function(x) centerEstimates[[x]]$mean)
    sdCenter <- sapply(1:nrow(dataCenter), function(x) centerEstimates[[x]]$sd)

    IntLower <- sapply(1:nrow(dataCenter),
                       function(x) signif(mean(XPredCenter[[x]]$IntLower), 5))
    IntUpper <- sapply(1:nrow(dataCenter),
                       function(x) signif(mean(XPredCenter[[x]]$IntUpper), 5))

    if(is.null(XPredCenter[[1]]$SDPop)) {
      dataCenter <- cbind(dataCenter, mean = meanCenter, sd = sdCenter,
                          IntLower = IntLower, IntUpper = IntUpper)
    } else {
      #population SD
      SDPop <- sapply(1:nrow(dataCenter),
                              function(x) signif(sqrt(mean(XPredCenter[[x]]$SDPop, na.rm = TRUE)^2), 5))

      sdCenterTotal <- sapply(1:nrow(dataCenter),
                              function(x) signif(sqrt(sum(sd(XPredCenter[[x]]$Est) ^ 2,
                                                          mean(XPredCenter[[x]]$Sd) ^ 2,
                                                          mean(XPredCenter[[x]]$SDPop) ^ 2,
                                                          na.rm = TRUE)), 5))
      IntLowerTotal <- sapply(1:nrow(dataCenter),
                              function(x) signif(min(XPredCenter[[x]]$IntLowerTotal), 5))
      IntUpperTotal <- sapply(1:nrow(dataCenter),
                              function(x) signif(max(XPredCenter[[x]]$IntUpperTotal), 5))

      dataCenter <- cbind(dataCenter, mean = meanCenter, sd = sdCenter,
                          SDPop = SDPop,
                          sdTotal = sdCenterTotal,
                          IntLower = IntLower, IntUpper = IntUpper,
                          IntLowerTotal = IntLowerTotal, IntUpperTotal = IntUpperTotal)
    }

    return(dataCenter)
  }

  # keep $Est in XPred for later calculation of mean and sd
  XPred$EstForCenter <- XPred$Est

  if (interior == TRUE){
    # this step can remove all $Est
    XPred$Est[draw == 0] <- NA
  }
  if (mask == TRUE){
    XPred$Est[maskDraw == 0] <- NA
  }

  if(GAM == TRUE){
    XPred$Est[is.na(XPred$Est) | XPred$Est < 0] <- 0
  }

  XPred$Est[XPred$Sd > StdErr] <- NA
  levels <- pretty(c(rangez[1], rangez[2]), n = ncol)


  if(mapType == "Minima/Maxima"){
    mins <- getMinima(XPred, nMin = nMin, minDist = minDist, minima = MinMax)
    if(MinMax == "Min"){
      probs <- round(as.vector(prop.table(table(factor(apply(Predictions[mins, ], 2, which.min), levels = 1:nMin)))), 3)
    } else{
      probs <- round(as.vector(prop.table(table(factor(apply(Predictions[mins, ], 2, which.max), levels = 1:nMin)))), 3)
    }
    dataMinPlot <- XPred[mins,]

    dataMinPlotPred <- (t(Predictions[mins,]))
    dataMinPlotPred <- data.frame(name = rep(paste0(MinMax, "ima ", 1:nMin,
                                                    "\n Coord. = (",round(dataMinPlot$Latitude, 2),", ",
                                                    round(dataMinPlot$Longitude, 2),") \n Probability = ", prob = probs),
                                             each = nrow(dataMinPlotPred)),
                                             estimate = as.vector(dataMinPlotPred))


    minMeans <- aggregate(dataMinPlotPred$estimate, by=list(name=dataMinPlotPred$name), FUN=mean)
    minMedians <- aggregate(dataMinPlotPred$estimate, by=list(name=dataMinPlotPred$name), FUN=median)
    minq68 <- aggregate(dataMinPlotPred$estimate, by=list(name=dataMinPlotPred$name), FUN=quantile, 0.68)
    minq32 <- aggregate(dataMinPlotPred$estimate, by=list(name=dataMinPlotPred$name), FUN=quantile, 0.32)
    minq95 <- aggregate(dataMinPlotPred$estimate, by=list(name=dataMinPlotPred$name), FUN=quantile, 0.975)
    minq05 <- aggregate(dataMinPlotPred$estimate, by=list(name=dataMinPlotPred$name), FUN=quantile, 0.025)

    meanEst <- q32 <- q68 <- q95 <- q05 <- NULL

    dataSummary <- data.frame(name = unique(dataMinPlotPred$name), meanEst = minMeans[,2], median = minMedians[,2],
               q68 = minq68[,2], q32 = minq32[,2],
               q95 = minq95[,2], q05 = minq05[,2])

    if(showMinOnMap == "1"){
      g <- ggplot(dataSummary, aes_(x = ~name, fill = ~name)) + theme_light()
      g <- g + geom_boxplot(
        mapping = aes(
          lower = q32,
          upper = q68,
          middle = median,
          ymin = q05,
          ymax = q95
        ),
        stat = "identity"
      ) + geom_errorbar(aes(ymin = meanEst, ymax = meanEst), linetype = "dashed", data = dataSummary)+ theme(legend.position="none") +
        labs(title=paste0("Comparison of local ", MinMax, "ima")) + xlab("")

    print(g)
    return(list(XPred = XPred))
    }
  }

  #if (!all(is.na(XPred$Est))){
  if (mapType == "Speed"){
    if(centerMap != "Europe"){
      longitudes <- longitudesPac[order(longitudesPac)]
      latitudes <- latitudes[order(latitudes, longitudesPac)]

      XPredPlot <- data.frame(XPredPac[order(XPredPac$Latitude, XPredPac$Longitude),],
                              Est = XPred$Est[order(XPredPac$Latitude, XPredPac$Longitude)])
      rangex <- rangexEU

    } else {
      XPredPlot <- XPred
    }
    z = matrix(XPredPlot$Est, ncol = resolution)
    test.rast <- raster(ncol= nrow(z), nrow = nrow(z), xmn = 0, xmx = 1,  ymn = 0, ymx = 1)
    test.rast[] <- as.vector(z)
    z2 <- abs(matrix(getValues(terrain(test.rast, unit = "tangent")), ncol = nrow(z)))
    tankilometers <- sqrt(diff(latitudes)[1] * diff(longitudes)[1]) * 111
    z2 <- log(((1/z2) / tankilometers) + 1E-10)
    z2levels <- signif(exp(pretty(as.vector(z2), n = ncol)), 2)
    q99 <- quantile(as.vector(exp(z2)), na.rm = TRUE, 0.975)
    z2levels <- z2levels[z2levels < q99]
    z2levels <- unique(c(z2levels, signif(q99, 2),
                         signif(max(as.vector(exp(z2)), na.rm = TRUE), 2)))
    # z2levels <- signif(pretty(c((rangez[1])^0.05, (rangez[2])^0.05), n = ncol)^20, 2)
    # z2levels[1] <- rangez[1]
    # z2levels[length(z2levels)] <- rangez[2]
    # if(length(unique(z2levels)) < 4){
    #   z2levels <- signif(pretty(c((1)^0.05, (1000)^0.05), n = ncol)^20, 2)
    # }
    colors <- colorRampPalette(brewer.pal(9, colors))(length(z2levels) - 1)
    if(reverseColors){
      colors <- rev(colors)
    }
    levelsLegend <- z2levels

    if(!showModel){
      z2 <- rep(NA, length(z2))
    }

    if(titleMain){
      main = ""
    } else {
      if(setAxisLabels){
        main = mainLabel
      } else {
        main = independent
      }
    }

    if(titleScale){
      mainS = ""
    } else {
      if(setAxisLabels){
        mainS = scLabel
      } else {
        mainS = "km/time unit"
      }
    }

    if(setAxisLabels){
      xlab = xLabel
      ylab = yLabel

    } else {
      xlab = "Longitude"
      ylab = "Latitude"
    }

    #for geotiff export
    if(centerMap != "Europe"){
      XPred$Est <- exp(as.vector(z2))[order(XPred$Latitude, XPred$Longitude)]
    } else {
      XPred$Est <- exp(as.vector(z2))
    }
    filled.contour2(longitudes, latitudes, z = z2,
                    contourType = contourType,
                    cex.axis = 1.5, cex.main = 1.5, cex.lab = 1.5,
                    xlim = rangex, ylim = rangey,
                    zlim = range(z2, finite = TRUE),
                    levels = log(z2levels + 1E-10), nlevels = length(z2levels),
                    col = colors,
                    asp = 1,
                    showScale = showScale,
                    key.axes = axis(4, at = log(levelsLegend + 1E-10), labels = levelsLegend),
                    key.title = title(main = mainS, cex.main = 0.8),
                    plot.title = {title(cex.lab = AxisSize, xlab = xlab, ylab = ylab, main = main)},
                    plot.axes = {
                      par(fg = "black", col="black");
                      if (terrestrial == 1){
                        if(centerMap != "Europe"){
                          sp::plot(Maps$ocean160, add = T, col = "lightblue", lwd = 1, border = NA);
                          sp::plot(Maps$ocean200, add = T, col = "lightblue", lwd = 1, border = NA);
                        } else {
                          sp::plot(Maps$ocean, add = T, col = "lightblue", lwd = 1);
                        }
                      }
                      if (terrestrial == -1){
                        if(centerMap != "Europe"){
                          sp::plot(Maps$land160, add = T, lwd = 1, col = "grey96", border = NA);
                          sp::plot(Maps$land200, add = T, lwd = 1, col = "grey96", border = NA);
                        } else {
                          sp::plot(Maps$land, add = T, lwd = 1, col = "grey96", border = NA);
                        }
                      }
                      if(centerMap != "Europe"){
                        sp::plot(Maps$coast160, add = T, lwd = 1);
                        sp::plot(Maps$coast200, add = T, lwd = 1);
                      } else {
                        sp::plot(Maps$coast, add = T, lwd = 1);
                      }
                      if (grid == TRUE){
                        if(centerMap != "Europe"){
                          sp::plot(Maps$grids160, add = T, col = "grey", lty = 2, xlim = c(0, 1));
                          sp::plot(Maps$grids200, add = T, col = "grey", lty = 2, xlim = c(0, 1));
                        } else {
                          sp::plot(Maps$grids, add = T, col = "grey", lty = 2, xlim = c(0, 1));
                        }
                      }
                      if(centerMap != "Europe"){
                        sp::plot(Maps$borders160, add = T, col = "darkgrey", lwd = 1);
                        sp::plot(Maps$borders200, add = T, col = "darkgrey", lwd = 1);

                      } else {
                        sp::plot(Maps$borders, add = T, col = "darkgrey", lwd = 1);
                      }
                      if (points == TRUE){
                        if(!is.null(pointLabels)){
                          pointLabels <- as.numeric(pointLabels)
                          pointLabels <- pointLabels - min(pointLabels, na.rm = TRUE)
                          pointLabels <- pointLabels / max(pointLabels, na.rm = TRUE)
                          pointSize <- (pointSize * pointLabels + 0.1) * 2
                        }
                        if(!is.null(pointColLabels)){
                          pointColLabels <- as.numeric(pointColLabels)
                          pointColLabels <- pointColLabels - min(pointColLabels, na.rm = TRUE)
                          pointColLabels <- pointColLabels / max(pointColLabels, na.rm = TRUE)
                          pColor <- (colorRampPalette(brewer.pal(9, colorsP))(101))[round(pointColLabels, 2) * 100]
                        }
                        if(cluster & !is.null(data$spatial_cluster)){
                          pColor <- colorRampPalette(brewer.pal(8, clusterCol))(max(data$spatial_cluster, na.rm=TRUE))[data$spatial_cluster]
                          if((!"long_temporal_group_reference_point" %in% names(data))){
                            data_names <- c("spatial_cluster", "long_centroid_spatial_cluster", "lat_centroid_spatial_cluster")
                          } else {
                            data_names <- c("temporal_group", "long_temporal_group_reference_point", "lat_temporal_group_reference_point")
                          }
                          centroids <- unique(data[, data_names])
                          centroids <- centroids[order(centroids[,1]), ]
                          if(centerMap != "Europe"){
                            centroids2 <- centroids
                            centroids2[,2][centroids[,2] < -20] <- centroids2[,2][centroids[,2] < -20] + 200
                            centroids2[,2][centroids[,2] >= -20] <- (- 180 + centroids2[,2][centroids[,2] >= -20])
                            centroids <- centroids2
                          }
                        }
                        if(centerMap != "Europe"){
                          points(dataPac$Latitude ~ dataPac$Longitude,
                                 col = pColor, lwd = 2,
                                 pch = pointShape, cex = pointSize);
                        } else {
                          points(data$Latitude ~ data$Longitude,
                                 col = pColor, lwd = 2,
                                 pch = pointShape, cex = pointSize);
                        }
                        if(cluster & !is.null(data$spatial_cluster)){
                          points(centroids[, 2:3], lwd = 2,
                                 pch = pointShape, cex = pointSize * 2.5, col = colorRampPalette(brewer.pal(8, clusterCol))(max(data$spatial_cluster, na.rm=TRUE)))
                          text(centroids[, 2:3], labels = paste0("Cluster_", centroids[,1]), pos = 4,
                               cex = fontSize * 1.5, col = fontCol, family = fontType)
                        }
                      }
                      if(!is.null(textLabels)){
                        if(centerMap != "Europe"){
                          text(dataPac$Latitude ~ dataPac$Longitude,
                               labels = as.character(textLabels), pos = 4,
                               cex = fontSize, col = fontCol, family = fontType)
                        } else {
                          text(data$Latitude ~ data$Longitude,
                               labels = as.character(textLabels), pos = 4,
                               cex = fontSize, col = fontCol, family = fontType)
                        }
                      }
                      if(centerMap != "Europe"){
                        lab <- pretty(rangex)
                        lab[pretty(rangex) >= 20] <- lab[pretty(rangex) >= 20] - 200
                        lab[pretty(rangex) < 20] <- lab[pretty(rangex) < 20] + 180
                        axis(1, at = pretty(rangex), labels = lab, cex.axis = AxisLSize);
                      } else{
                        axis(1, cex.axis = AxisLSize);
                      }
                      axis(2, cex.axis = AxisLSize);
                    }
    )
    if (arrow == TRUE){
      if(showScale == TRUE){
        north.arrow(rangex[1] + diff(rangey) * NorthX, rangey[1] + diff(rangey) * NorthY,
                    diff(rangey) * 0.04 * northSize * 5,
                    1 * ((2 * diff(rangey)) / diff(rangex)) ^ 0.3* northSize * 5, c(0.5, - 0.25))
      } else {
        north.arrow(rangex[1] + diff(rangey) * NorthX, rangey[1] + diff(rangey) * NorthY,
                    diff(rangey) * 0.04 * northSize * 5,
                    1 * ((2 * diff(rangey)) / diff(rangex)) ^ 0.3* northSize * 5, c(0.5, - 0.25))
      }
    }
    if (scale == TRUE){
      if(showScale == TRUE){
        maps::map.scale(x = rangex[1] + diff(rangex) * scaleX,
                        y = rangey[1] + diff(rangey) * scaleY,
                        rel = scalSize * ((2 * diff(rangey)) / diff(rangex)) ^ 0.15,
                        cex = 0.75 * ((2 * diff(rangey)) / diff(rangex)) ^ -0.2, ratio = FALSE)
      } else {
        maps::map.scale(x = rangex[1] + diff(rangex) * scaleX,
                        y = rangey[1] + diff(rangey) * scaleY,
                        rel = scalSize * ((2 * diff(rangey)) / diff(rangex)) ^ 0.15,
                        cex = 0.75 * ((2 * diff(rangey)) / diff(rangex)) ^ -0.2, ratio = FALSE)

      }
    }

    return(list(XPred = XPred))
  }

  if(GAM == TRUE){
    colors <- c("#FFFFFF", colorRampPalette(brewer.pal(9, colors))(length(levels) - 2))
  } else{
    colors <- colorRampPalette(brewer.pal(9, colors))(length(levels) - 1)
  }
  if(reverseColors){
    colors <- rev(colors)
  }

  levelsLegend <- levels
  if(length(levels) > 25){
    par(fg = NA, col="black")
    levelsLegend <- pretty(c(rangez[1], rangez[2]),
                           n = pmin(20, ceiling(ncol / 2)))
  }

  cex4 <- 1
  if(max(abs(XPred$Est), na.rm = TRUE) > 9999 | max(abs(XPred$Est), na.rm = TRUE) < 0.05){
    cex4 <- 0.7
  }
  if(centerMap != "Europe"){
    longitudes <- longitudesPac[order(longitudesPac)]
    latitudes <- latitudes[order(latitudes, longitudesPac)]

    XPredPlot <- data.frame(XPredPac[order(XPredPac$Latitude, XPredPac$Longitude),],
                            Est = XPred$Est[order(XPredPac$Latitude, XPredPac$Longitude)])
    rangex <- rangexEU
  } else {
    XPredPlot <- XPred
  }
  if(!showModel){
    XPredPlot$Est <- rep(NA, length(XPredPlot$Est))
  }


  if(titleMain){
    main = ""
  } else {
    if(setAxisLabels){
      main = mainLabel
    } else {
      main = independent
    }
  }

  if(titleScale){
    mainS = ""
  } else {
    if(setAxisLabels){
      mainS = scLabel
    } else {
      mainS = independent
    }
  }

  if(setAxisLabels){
    xlab = xLabel
    ylab = yLabel

  } else {
    xlab = "Longitude"
    ylab = "Latitude"
  }


  filled.contour2(longitudes, latitudes, z = matrix(XPredPlot$Est, ncol = resolution),
                  contourType = contourType,
                  xlim = rangex, ylim = rangey, levels = levels, zlim = rangez,
                  col = colors,
                  showScale = showScale,
                  cex.axis = 1.5, cex.main = 1.5, cex.lab = 1.5,
                  asp = 1, key.axes = axis(side = 4, at = levelsLegend, tick = FALSE, cex.axis = cex4),
                  key.title = title(main = mainS, cex.main = 0.8),
                  plot.title = {title(cex.lab = AxisSize, xlab = xlab, ylab = ylab, main = main)},
                  plot.axes = {
                    par(fg = "black", col="black");
                    if (terrestrial == 1){
                      if(centerMap != "Europe"){
                        sp::plot(Maps$ocean160, add = T, col = "lightblue", lwd = 1, border = NA);
                        sp::plot(Maps$ocean200, add = T, col = "lightblue", lwd = 1, border = NA);
                      } else {
                        sp::plot(Maps$ocean, add = T, col = "lightblue", lwd = 1);
                      }
                    }
                    if (terrestrial == -1){
                      if(centerMap != "Europe"){
                        sp::plot(Maps$land160, add = T, lwd = 1, col = "grey96", border = NA);
                        sp::plot(Maps$land200, add = T, lwd = 1, col = "grey96", border = NA);
                      } else {
                        sp::plot(Maps$land, add = T, lwd = 1, col = "grey96", border = NA);
                      }
                    }
                    if(centerMap != "Europe"){
                      sp::plot(Maps$coast160, add = T, lwd = 1);
                      sp::plot(Maps$coast200, add = T, lwd = 1);
                    } else {
                      sp::plot(Maps$coast, add = T, lwd = 1);
                    }
                    if (grid == TRUE){
                      if(centerMap != "Europe"){
                        sp::plot(Maps$grids160, add = T, col = "grey", lty = 2, xlim = c(0, 1));
                        sp::plot(Maps$grids200, add = T, col = "grey", lty = 2, xlim = c(0, 1));
                      } else {
                        sp::plot(Maps$grids, add = T, col = "grey", lty = 2, xlim = c(0, 1));
                      }

                    }
                    if(centerMap != "Europe"){
                      sp::plot(Maps$borders160, add = T, col = "darkgrey", lwd = 1);
                      sp::plot(Maps$borders200, add = T, col = "darkgrey", lwd = 1);

                    } else {
                      sp::plot(Maps$borders, add = T, col = "darkgrey", lwd = 1);
                    }
                    if (points == TRUE){
                      if(!is.null(pointLabels)){
                        pointLabels <- as.numeric(pointLabels)
                        pointLabels <- pointLabels - min(pointLabels, na.rm = TRUE)
                        pointLabels <- pointLabels / max(pointLabels, na.rm = TRUE)
                        pointSize <- (pointSize * pointLabels + 0.1) * 2
                      }
                      if(!is.null(pointColLabels)){
                        pointColLabels <- as.numeric(pointColLabels)
                        pointColLabels <- pointColLabels - min(pointColLabels, na.rm = TRUE)
                        pointColLabels <- pointColLabels / max(pointColLabels, na.rm = TRUE)
                        pColor <- (colorRampPalette(brewer.pal(9, colorsP))(101))[round(pointColLabels, 2) * 100]
                      }
                      if(cluster & !is.null(data$spatial_cluster)){
                        pColor <- colorRampPalette(brewer.pal(8, clusterCol))(max(data$spatial_cluster, na.rm=TRUE))[data$spatial_cluster]
                        if((!"long_temporal_group_reference_point" %in% names(data))){
                          data_names <- c("spatial_cluster", "long_centroid_spatial_cluster", "lat_centroid_spatial_cluster")
                        } else {
                          data_names <- c("temporal_group", "long_temporal_group_reference_point", "lat_temporal_group_reference_point")
                        }
                        centroids <- unique(data[, data_names])
                        centroids <- centroids[order(centroids[,1]), ]
                        if(centerMap != "Europe"){
                          centroids2 <- centroids
                          centroids2[,2][centroids[,2] < -20] <- centroids2[,2][centroids[,2] < -20] + 200
                          centroids2[,2][centroids[,2] >= -20] <- (- 180 + centroids2[,2][centroids[,2] >= -20])
                          centroids <- centroids2
                        }
                      }

                      if(centerMap != "Europe"){
                        points(dataPac$Latitude ~ dataPac$Longitude,
                               col = pColor, lwd = 2,
                               pch = pointShape, cex = pointSize);
                      } else {
                        points(data$Latitude ~ data$Longitude,
                               col = pColor, lwd = 2,
                               pch = pointShape, cex = pointSize);
                      }
                      if(cluster & !is.null(data$spatial_cluster)){
                        points(centroids[, 2:3], lwd = 2,
                               pch = pointShape, cex = pointSize * 2.5, col = colorRampPalette(brewer.pal(8, clusterCol))(max(data$spatial_cluster, na.rm=TRUE)))
                        text(centroids[, 2:3], labels = paste0("Cluster_", centroids[,1]), pos = 4,
                             cex = fontSize * 1.5, col = fontCol, family = fontType)
                      }
                    }
                    if(mapType == "Minima/Maxima"){
                      points(dataMinPlot$Latitude ~ dataMinPlot$Longitude, lwd = 2, col = pColor,
                             pch = pointShape, cex = pointSize * 2.5)
                      text(dataMinPlot$Latitude ~ dataMinPlot$Longitude, labels = paste0(MinMax, "ima", 1:nMin), pos = 4,
                           cex = fontSize * 1.5, col = fontCol, family = fontType)
                    }
                    if(!is.null(textLabels)){
                      if(centerMap != "Europe"){
                        text(dataPac$Latitude ~ dataPac$Longitude,
                             labels = as.character(textLabels), pos = 4,
                             cex = fontSize, col = fontCol, family = fontType)
                      } else {
                        text(data$Latitude ~ data$Longitude,
                             labels = as.character(textLabels), pos = 4,
                             cex = fontSize, col = fontCol, family = fontType)
                      }
                    }
                    if(!is.null(pointDat) & NROW(pointDat) > 0){
                      points(x = pointDat$x, y = pointDat$y, cex = pointDat$pointSize, col = pointDat$pointColor, pch = 16)
                      text(pointDat$y ~ pointDat$x, labels = pointDat$label, pos = 4, cex = 1.75)
                    }
                    if(centerMap != "Europe"){
                      lab <- pretty(rangex)
                      lab[pretty(rangex) >= 20] <- lab[pretty(rangex) >= 20] - 200
                      lab[pretty(rangex) < 20] <- lab[pretty(rangex) < 20] + 180
                      axis(1, at = pretty(rangex), labels = lab, cex.axis = AxisLSize);
                    } else{
                      axis(1, cex.axis = AxisLSize);
                    }
                    axis(2, cex.axis = AxisLSize);
                  }
  )
  # }  else {
  #   plot(1:10, xlim = range(data$Longitude), ylim = range(data$Latitude),
  #        xlab = "Longitude", ylab = "Latitude")
  #   plot(Maps$ocean, add = T, col = "lightblue", lwd = 1)
  #   sp::plot(Maps$grids, add = T, col = "grey", lty = 2, xlim = c(0, 1))
  #   sp::plot(Maps$borders, add = T, col = "darkgrey", lwd = 1)
  #   text(mean(data$Longitude),mean(data$Latitude), cex = 1,
  #        "No estimates to plot, please adjust \"Display up to max standard error\" slider",
  #        col = "red")
  # }
  if (arrow == TRUE){
    if(showScale == TRUE){
      north.arrow(rangex[1] + diff(rangey) * NorthX, rangey[1] + diff(rangey) * NorthY,
                  diff(rangey) * 0.04 * northSize * 5,
                  1 * ((2 * diff(rangey)) / diff(rangex)) ^ 0.3* northSize * 5, c(0.5, - 0.25))
    } else {
      north.arrow(rangex[1] + diff(rangey) * NorthX, rangey[1] + diff(rangey) * NorthY,
                  diff(rangey) * 0.04 * northSize * 5,
                  1 * ((2 * diff(rangey)) / diff(rangex)) ^ 0.3* northSize * 5, c(0.5, - 0.25))
    }
  }
  if (scale == TRUE){
    if(showScale == TRUE){
      maps::map.scale(x = rangex[1] + diff(rangex) * scaleX,
                      y = rangey[1] + diff(rangey) * scaleY,
                      rel = scalSize * ((2 * diff(rangey)) / diff(rangex)) ^ 0.15,
                      cex = 0.75 * ((2 * diff(rangey)) / diff(rangex)) ^ -0.2, ratio = FALSE)
    } else {
      maps::map.scale(x = rangex[1] + diff(rangex) * scaleX,
                      y = rangey[1] + diff(rangey) * scaleY,
                      rel = scalSize * ((2 * diff(rangey)) / diff(rangex)) ^ 0.15,
                      cex = 0.75 * ((2 * diff(rangey)) / diff(rangex)) ^ -0.2, ratio = FALSE)

    }
  }

  return(list(XPred = XPred))
}

#' Plots time slice map of a spatio-temporal model from estimateMap3D() function
#'
#' @param model return object of a spatio-temporal model from estimateMap3D() function
#' @param IndSelect for categorical model: selected category
#' @param time time slice value for map
#' @param arrow display north arrow TRUE/FALSE
#' @param scale display scale TRUE/FALSE
#' @param terrestrial show only estimates on land masses (1), oceans (-1) or all (0)
#' @param resolution spatial grid resolution of displayed (higher is slower but better quality)
#' @param interior show only convex hull 0 "none", 1 "3D", 2 "time sliced 2D"
#' @param addU numeric: years of added uncertainty for time sliced 2D-convex hull
#' @param grid show coordinate grid TRUE/FALSE
#' @param ncol number of colors for estimates
#' @param pColor color of location marks drawn
#' @param colorsP color scale of location marks
#' @param pointShape pch shape of location marks
#' @param colors color scheme of estimates from RColorBrewer. defaults to "RdYlGn"
#' @param reverseColors inverse colour scheme
#' @param estType one of "Mean", "SE" and "Quantile". defaults to "Mean"
#' @param estQuantile quantile (only applicable if estType = "Quantile")
#' @param points show points on map TRUE/FALSE
#' @param pointSize point size (if points = TRUE)
#' @param StdErr maximum standard error to which estimates are displayed
#' @param rangex range of longitude values (x axis limits)
#' @param rangey range of latitude values (y axis limits)
#' @param mask boolean: Show output within range of points
#' @param maskRadius numeric: Show output within range of points in km
#' @param limitz restrict range of z (possible values "0-1", "0-100")
#' @param rangez range of estimated values (z axis limits)
#' @param centerMap center of map, one of "Europe" and "Pacific"
#' @param dataCenter optional data.frame with three columns (longitude / latitude / time)
#' @param RadiusBatch radius of batch spatial point estimates
#' @param textLabels text labels
#' @param pointLabels point size labels
#' @param pointColLabels point colour labels
#' @param fontSize font size
#' @param showModel show model
#' @param fontType font type
#' @param fontCol font color
#' @param titleMain show main title
#' @param titleScale show scale title
#' @param setAxisLabels show axis/main/scale labels
#' @param mainLabel main label
#' @param yLabel y lab label
#' @param xLabel y lab label
#' @param northSize size of north arrow
#' @param scalSize size of scale
#' @param scaleX scale x orientation
#' @param scaleY scale y orientation
#' @param NorthX north arrow x orientation
#' @param NorthY north arrow y orientation
#' @param scLabel scale lab label
#' @param AxisSize axis title font size
#' @param AxisLSize axis label font size
#' @param cluster show clusters
#' @param clusterAll show all cluster points
#' @param clusterResults temporal groups or spatial clusters
#' @param clusterCol Cluster colors
#' @param pointDat data frame of points to add to plot
#' @param plotRetNull return predictions
#' @inheritParams filled.contour2
#'
#' @export
plotMap3D <- function(model,
                      time,
                      IndSelect = NULL,
                      estType = "Mean", estQuantile = 0.9,
                      points = TRUE, pointSize = 1, StdErr = Inf,
                      rangex = range(model$data$Longitude, na.rm = TRUE),
                      rangey = range(model$data$Latitude, na.rm = TRUE),
                      rangez = NULL,
                      contourType = c("filled.contour", "contour"),
                      limitz = "none",
                      centerMap = "Europe",
                      addU = 0,
                      pointShape = 4,
                      pColor = "#000000",
                      colorsP = NULL,
                      showModel = TRUE,
                      textLabels = NULL,
                      pointLabels = NULL,
                      pointColLabels = NULL,
                      fontSize = 11,
                      mask = FALSE,
                      maskRadius = 100,
                      showScale = TRUE,
                      fontType = "sans",
                      fontCol = "black",
                      resolution = 100, interior = TRUE,
                      ncol = 10, colors = "RdYlGn", reverseColors = FALSE,
                      dataCenter = NULL,
                      RadiusBatch = 100, terrestrial = 1,
                      grid = TRUE, arrow = TRUE, scale = TRUE,
                      titleMain = TRUE,
                      titleScale = TRUE,
                      setAxisLabels = FALSE,
                      mainLabel = "",
                      yLabel =  "Latitude",
                      xLabel =  "Longitude",
                      scLabel =  "",
                      northSize = 0.2,
                      scalSize = 0.1,
                      scaleX = 0,
                      scaleY = 0.1,
                      NorthX = 0.025,
                      NorthY = 0.925,
                      AxisSize = 1,
                      AxisLSize = 1,
                      cluster = FALSE,
                      clusterAll = "0",
                      clusterResults = 0,
                      clusterCol = "Set1",
                      pointDat = NULL,
                      plotRetNull = FALSE){
  options(scipen = 999)
  contourType <- match.arg(contourType)
  minRangeFactor <- 0.75
  if((diff(rangex) / diff(rangey)) < minRangeFactor){
    rangex[1] <- max(-180, mean(rangex) - minRangeFactor / 2 * diff(rangey))
    rangex[2] <- min(180, mean(rangex) + minRangeFactor / 2 * diff(rangey))
  }
  minRangeFactor <- 0.5
  if((diff(rangey) / diff(rangex)) < minRangeFactor){
    rangey[1] <- max(-90, mean(rangey) - minRangeFactor / 2 * diff(rangex))
    rangey[2] <- min(90, mean(rangey) + minRangeFactor / 2 * diff(rangex))
  }
  independent <- model$independent

  if(!is.null(model$IndependentType) && model$IndependentType != "numeric"){
    if(is.null(IndSelect) || IndSelect == ""){
      return(NULL)
    }
    model$model <- model$model[[IndSelect]]
  }

  if(is.null(rangez)){
    rangez = range(model$data[, independent], na.rm = TRUE)
  }


  Bayes = TRUE
  GAM = FALSE
  if("gamm" %in% class(model$model)){
    Bayes = FALSE
  }
  if("kde" %in% class(model$model)){
    GAM = TRUE
    independent = "Density"
  }

  Maps <- loadMaps()
  data <- model$data
  sc <- model$sc

  if(!is.null(textLabels)){
    textLabels <- textLabels[as.numeric(rownames(data)),1]
  }
  if(!is.null(pointLabels)){
    pointLabels <- pointLabels[as.numeric(rownames(data)),1]
  }
  if(!is.null(pointColLabels)){
    pointColLabels <- pointColLabels[as.numeric(rownames(data)),1]
  }

  if (is.null(data)) return(NULL)

  RadiusBatch <-  RadiusBatch / 111
  maskRadius <- maskRadius / 300

  longitudes <- seq(max(-180, rangex[1] - 0.1 * diff(rangex)),
                    min(180, rangex[2] + 0.1 * diff(rangex)), length.out = resolution)
  latitudes <- seq(max(-90, rangey[1] - 0.1 * diff(rangey)),
                   min(90, rangey[2] + 0.1 * diff(rangey)), length.out = resolution)
  XPred <- cbind(expand.grid(Longitude2 = (longitudes - mean(data$Longitude)) / sd(data$Longitude),
                             Latitude2 = (latitudes - mean(data$Latitude)) / sd(data$Latitude)),
                 expand.grid(Longitude = longitudes, Latitude = latitudes), Date = time)

  if(centerMap != "Europe"){
    rangexEU <- rangex
    # rangex[rangexEU < 20] <- rangex[rangexEU < 20] + 160
    # rangex[rangexEU >= 20] <- (- 200 + rangex[rangexEU >= 20])
    rangex <- shiftLongitudesToPacific(rangex)
    rangex <- rangex[order(rangex)]
    longitudesPac <- longitudes
    # longitudes[longitudesPac < 20] <- longitudes[longitudesPac < 20] + 160
    # longitudes[longitudesPac >= 20] <- (- 200 + longitudes[longitudesPac >= 20])
    longitudes <- shiftLongitudesToPacific(longitudes)
    XPredPac <- XPred
    # XPred$Longitude[XPredPac$Longitude < 20] <- XPred$Longitude[XPredPac$Longitude < 20] + 160
    # XPred$Longitude[XPredPac$Longitude >= 20] <- (- 200 + XPred$Longitude[XPredPac$Longitude >= 20])
    XPred$Longitude <- shiftLongitudesToPacific(XPred$Longitude)
    XPred$Longitude2 = (XPred$Longitude - mean(data$Longitude)) / sd(data$Longitude)
    dataPac <- data %>%
      centerPlotData(centerMap = centerMap)
    dataTPac <- dataPac %>%
      filterT(addU = addU, time = time)
  }


  if (interior > 0){
    if(centerMap != "Europe"){
      if(interior == 1){
        cData <- rbind(data.frame(Date = dataPac$Date + 2 * dataPac$Uncertainty, dataPac[, c("Longitude", "Latitude")]),
                       data.frame(Date = dataPac$Date - 2 * dataPac$Uncertainty, dataPac[, c("Longitude", "Latitude")]))
        if(nrow(cData) > 0){
          convHull <- convhulln(cData)
          draw <- inhulln(convHull, as.matrix(XPredPac[, c(5,3,4)]))
        } else {
          draw <- rep(0, nrow(XPred))
        }
      } else {
        cData <- dataTPac[, c("Longitude", "Latitude")]
        if(nrow(cData) > 2){
          convHull <- convhulln(cData)
          draw <- inhulln(convHull, as.matrix(XPredPac[, c(3,4)]))
        } else {
          draw <- rep(0, nrow(XPred))
        }
      }

    } else {
      if(interior == 1){
        cData <- rbind(data.frame(Date = data$Date + 2 * data$Uncertainty, data[, c("Longitude", "Latitude")]),
                       data.frame(Date = data$Date - 2 * data$Uncertainty, data[, c("Longitude", "Latitude")]))
        if(nrow(unique(cData)) > 3){
          convHull <- convhulln(cData)
          draw <- inhulln(convHull, as.matrix(XPred[, c(5,3,4)]))
        } else {
          draw <- rep(0, nrow(XPred))
        }

      } else {
        cData <- filterT(data, addU = addU, time = time)[, c("Longitude", "Latitude")]
        if(nrow(unique(cData)) > 3){
          convHull <- convhulln(cData)
          draw <- inhulln(convHull, as.matrix(XPred[, c(3,4)]))
        } else {
          draw <- rep(0, nrow(XPred))
        }
      }
    }
  }

  if(mask == TRUE){
    if(exists("cData")){
      maskData <- unique(cData[, c("Longitude", "Latitude")])
    } else {
      maskData <- data
    }

    maskDraw <- extractMaskDraw(dat = maskData,
                                maskRadius = maskRadius,
                                XPred = centerXPred(XPred = XPred,
                                                    XPredPac = XPredPac,
                                                    centerMap = centerMap))
  }

  if (!is.null(dataCenter)){
    # this is batch mode
    XPred <- do.call("rbind", lapply(1:nrow(dataCenter), function(x) {
      longitudes <- seq(dataCenter[x, 1] - RadiusBatch,
                        dataCenter[x, 1] + RadiusBatch, length.out = 4)
      latitudes <- seq(dataCenter[x, 2] - RadiusBatch,
                       dataCenter[x, 2] + RadiusBatch, length.out = 4)
      XPred <- expand.grid(Longitude2 = (longitudes - mean(data$Longitude)) / sd(data$Longitude),
                           Latitude2 = (latitudes - mean(data$Latitude)) / sd(data$Latitude),
                           time = dataCenter[x, 3])
      XPred$Longitude <- XPred$Longitude2 * sd(data$Longitude) + mean(data$Longitude)
      XPred$Latitude <- XPred$Latitude2 * sd(data$Latitude) + mean(data$Latitude)
      cbind(XPred, id = x)
    }))
  }

  if (Bayes == TRUE & GAM == FALSE){
    PredMatr <- Predict.matrix(sc,
                               data = data.frame(XPred,
                                                 Date2 = (time - mean(data$Date)) /
                                                   sd(data$Date)))

    betas <- model$model$beta
    betaSigma <- model$model$betaSigma

    Predictions <-
      sapply(1:nrow(betas), function(x)
        PredMatr %*% betas[x, ] * model$sRe + model$mRe)

    if(!is.null(model$IndependentType) && model$IndependentType != "numeric"){
      Predictions <- invLogit(Predictions)
    }

    if(!is.null(betaSigma)){
      PredMatrV <- Predict.matrix(model$scV,
                                  data = data.frame(XPred,
                                                    Date2 = (time - mean(data$Date)) /
                                                      sd(data$Date)))
      PredictionsSigma <-
        rowMeans(sqrt(sapply(1:nrow(betaSigma), function(x)
          exp((PredMatrV %*% betaSigma[x, ])) / model$model$sigma[x]) * model$sRe^2))
    } else {
      if(!is.null(model$IndependentType) && model$IndependentType != "numeric"){
        PredictionsSigma <- sqrt(Predictions * (1-Predictions))
      } else {
        PredictionsSigma <- sqrt(mean(model$model$sigma) * model$sRe^2)
      }
    }
    if(estType == "Mean"){
      Est <- rowMeans(Predictions)
    }
    if(estType == "1 SE"){
      Est <- apply(Predictions, 1, sd)
    }
    if(estType == "2 SE"){
      Est <- apply(Predictions, 1, sd) * 2
    }
    if(estType == "1 SETOTAL"){
      Est <- sqrt(PredictionsSigma^2 + apply(Predictions, 1, sd)^2)
    }
    if(estType == "2 SETOTAL"){
      Est <- sqrt(PredictionsSigma^2 + apply(Predictions, 1, sd)^2) * 2
    }
    if(estType == "1 SD Population"){
      Est <- PredictionsSigma * 1
    }
    if(estType == "2 SD Population"){
      Est <- PredictionsSigma * 2
    }
    if(estType == "Quantile"){
      Est <- apply(Predictions, 1, quantile, estQuantile)
    }
    if(estType == "QuantileTOTAL"){
      Est <- rowMeans(Predictions +  qnorm(estQuantile) * sqrt(PredictionsSigma^2 + apply(Predictions, 1, sd)^2))
    }

    XPred <-
      data.frame(XPred,
                 Est = Est,
                 Sd = apply(Predictions, 1, sd),
                 SDPop = PredictionsSigma,
                 SdTotal = sqrt(PredictionsSigma^2 + apply(Predictions, 1, sd)^2),
                 IntLower = apply(Predictions, 1, quantile, 0.025),
                 IntUpper = apply(Predictions, 1, quantile, 0.975),
                 IntLowerTotal = apply(Predictions + sqrt(PredictionsSigma^2 + apply(Predictions, 1, sd)^2), 1, quantile, 0.025),
                 IntUpperTotal = apply(Predictions + sqrt(PredictionsSigma^2 + apply(Predictions, 1, sd)^2), 1, quantile, 0.975),
                 resError = sqrt(mean(model$model$sigma + model$model$tau) * model$sRe^2))
  }
  if (Bayes == FALSE & GAM == FALSE){
    Est <- predict(model$model$gam,
                   data.frame(XPred,
                              Date2 = (time - mean(data$Date)) /
                                sd(data$Date)), se.fit = TRUE, type = "response")
    if(estType == "1 SE"){
      Est$fit <- Est$se.fit
    }
    if(estType == "2 SE"){
      Est$fit <- Est$se.fit * 2
    }
    if(!is.null(model$IndependentType) && model$IndependentType != "numeric"){
      varM = Est$fit * (1-Est$fit)
    }  else {
      varM = var(residuals(model$model$gam))
    }
    if(estType == "1 SETOTAL"){
      Est$fit <- sqrt(Est$se.fit^2 + varM)
    }
    if(estType == "2 SETOTAL"){
      Est$fit <- sqrt(Est$se.fit^2 + varM) * 2
    }
    if(estType == "1 SD Population"){
      Est$fit <- sqrt(varM) * 1
    }
    if(estType == "2 SD Population"){
      Est$fit <- sqrt(varM) * 2
    }
    if(estType == "Quantile"){
      Est$fit <- Est$fit + qnorm(estQuantile) * Est$se.fit
    }
    if(estType == "QuantileTOTAL"){
      Est$fit <- Est$fit + qnorm(estQuantile) *
        sqrt(Est$se.fit^2 + varM)
    }
    XPred <- data.frame(XPred,
                        Est = Est$fit, Sd = Est$se.fit,
                        SDPop = sqrt(varM),
                        SdTotal = sqrt(Est$se.fit^2 + varM),
                        IntLower = Est$fit - 1.96 * Est$se.fit,
                        IntUpper = Est$fit + 1.96 * Est$se.fit,
                        IntLowerTotal = Est$fit - 1.96 *
                          sqrt(Est$se.fit^2 + varM),
                        IntUpperTotal = Est$fit + 1.96 *
                          sqrt(Est$se.fit^2 + varM),
                        resError = sqrt(mean((predict(model$model$gam, type = "response") - model$model$gam$y)^2)))
  }
  if(GAM == TRUE){
    Predictions <- sapply(1:length(model$model),
                          function(x) predict(model$model[[x]],
                                              x =  cbind(XPred$Longitude, XPred$Latitude,
                                                         Date2 = (time - mean(data$Date)) / sd(data$Date))))

    if(estType == "Mean"){
      Est <- rowMeans(Predictions)
    }
    if(estType == "1 SE"){
      Est <- apply(Predictions, 1, sd)
    }
    if(estType == "2 SE"){
      Est <- apply(Predictions, 1, sd) * 2
    }

    if(estType == "Quantile"){
      Est <- apply(Predictions, 1, quantile, estQuantile)
    }

    XPred <- data.frame(XPred,
                        Est = Est,
                        Sd = apply(Predictions, 1, sd),
                        IntLower = apply(Predictions, 1, quantile, 0.025),
                        IntUpper = apply(Predictions, 1, quantile, 0.975))
  }
  if (estType != "1 SE" && estType != "1 SETOTAL" && estType != "2 SE" &&
      estType != "2 SETOTAL" &&
      !is.null(limitz) && !missing(limitz)){
    if (limitz == "0-1"){
      XPred$Est <- pmin(1, XPred$Est)
      XPred$Est <- pmax(0, XPred$Est)
    }
    if (limitz == "0-100"){
      XPred$Est <- pmin(100, XPred$Est)
      XPred$Est <- pmax(0, XPred$Est)
    }
  }

  if (!is.null(dataCenter)){
    # this is batch mode
    XPredCenter <- lapply(1:nrow(dataCenter), function(x){
      XPred %>%
        extractXPredCenter(centerX = dataCenter[x, 1],
                           centerY = dataCenter[x, 2],
                           Radius = RadiusBatch,
                           batch = TRUE,
                           isThreeD = TRUE,
                           data = data,
                           time = dataCenter[x, 3])
    })

    centerEstimates <- lapply(1:nrow(dataCenter), function(x){
      XPredCenter[[x]] %>%
        extractCenterEstimates(batch = FALSE) # why is sd calculated here differently than in plotMap batch??
    })
    meanCenter <- sapply(1:nrow(dataCenter), function(x) centerEstimates[[x]]$mean)
    sdCenter <- sapply(1:nrow(dataCenter), function(x) centerEstimates[[x]]$sd)

    IntLower <- sapply(1:nrow(dataCenter),
                       function(x) signif(min(XPredCenter[[x]]$IntLower), 5))
    IntUpper <- sapply(1:nrow(dataCenter),
                       function(x) signif(max(XPredCenter[[x]]$IntUpper), 5))

    if(is.null(XPredCenter[[1]]$SDPop)) {
      dataCenter <- cbind(dataCenter, time = time, mean = meanCenter, sd = sdCenter,
                          IntLower = IntLower, IntUpper = IntUpper)
      } else {
        SDPop <- sapply(1:nrow(dataCenter),
                        function(x) signif(sqrt(mean(XPredCenter[[x]]$SDPop, na.rm = TRUE)^2), 5))

        sdCenterTotal <- sapply(1:nrow(dataCenter),
                                function(x) signif(sqrt(sum(c(sd(XPredCenter[[x]]$Est)^2,
                                                              mean(XPredCenter[[x]]$Sd^2),
                                                              mean(XPredCenter[[x]]$SdTotal)^2), na.rm = TRUE)), 5))
        IntLowerTotal <- sapply(1:nrow(dataCenter),
                                function(x) signif(min(XPredCenter[[x]]$IntLowerTotal), 5))
        IntUpperTotal <- sapply(1:nrow(dataCenter),
                                function(x) signif(max(XPredCenter[[x]]$IntUpperTotal), 5))

        dataCenter <- cbind(dataCenter, time = time, mean = meanCenter, sd = sdCenter,
                            SDPop = SDPop,
                            sdTotal = sdCenterTotal,
                            IntLower = IntLower, IntUpper = IntUpper,
                            IntLowerTotal = IntLowerTotal, IntUpperTotal = IntUpperTotal)
      }
    return(dataCenter)
  }

  # keep $Est for later calculation of mean and sd for center
  XPred$EstForCenter <- XPred$Est

  if (interior > 0){
    # this can remove all $Est
    XPred$Est[draw == 0] <- NA
  }
  if (mask == TRUE){
    XPred$Est[maskDraw == 0] <- NA
  }

  if(GAM == TRUE){
    XPred$Est[is.na(XPred$Est) | XPred$Est < 0] <- 0
  }

  #if (!all(is.na(XPred$Est))){
  levels <- pretty(c(rangez[1], rangez[2]), n = ncol)

  if(GAM == TRUE){
    colors <- c("#FFFFFF", colorRampPalette(brewer.pal(9, colors))(length(levels) - 2))
  } else{
    colors <- colorRampPalette(brewer.pal(9, colors))(length(levels) - 1)
  }

  if(reverseColors){
    colors <- rev(colors)
  }

  levelsLegend <- levels
  if(length(levels) > 25){
    par(fg = NA, col="black")
    levelsLegend <- pretty(c(rangez[1], rangez[2]),
                           n = pmin(20, ceiling(ncol / 2)))
  }
  cex4 <- 1
  if(!all(is.na(XPred$Est)) &&
     (max(abs(XPred$Est), na.rm = TRUE) > 9999 | max(abs(XPred$Est), na.rm = TRUE) < 0.05)){
    cex4 <- 0.7
  }

  if(centerMap != "Europe"){
    longitudes <- longitudesPac[order(longitudesPac)]
    latitudes <- latitudes[order(latitudes, longitudesPac)]

    XPredPlot <- data.frame(XPredPac[order(XPredPac$Latitude, XPredPac$Longitude),],
                            Est = XPred$Est[order(XPredPac$Latitude, XPredPac$Longitude)])
    rangex <- rangexEU
  } else {
    XPredPlot <- XPred
  }

  if(!showModel){
    XPredPlot$Est <- rep(NA, length(XPredPlot$Est))
  }

  if(titleMain){
    main = ""
  } else {
    if(setAxisLabels){
      main = mainLabel
    } else {
      main = independent
    }
  }

  if(titleScale){
    mainS = ""
  } else {
    if(setAxisLabels){
      mainS = scLabel
    } else {
      mainS = paste("time:", time)
    }
  }

  if(setAxisLabels){
    xlab = xLabel
    ylab = yLabel

  } else {
    xlab = "Longitude"
    ylab = "Latitude"
  }

  filled.contour2(longitudes, latitudes, z = matrix(XPredPlot$Est, ncol = resolution),
                  contourType = contourType,
                  xlim = rangex, ylim = rangey, levels = levels,
                  col = colors,
                  showScale = showScale,
                  cex.axis = 1.5, cex.main = 1.5, cex.lab = 1.5,
                  asp = 1, xaxp = c(0, 100, 10), yaxp = c(0, 100, 10),
                  key.axes = axis(side = 4, at = levelsLegend, cex.axis = cex4),
                  key.title = title(main = main, cex.main = 0.8),
                  plot.title = {title(cex.lab = AxisSize, xlab = xlab, ylab = ylab, main = mainS)},
                  plot.axes = {
                    par(fg = "black", col="black");
                    if (terrestrial == 1){
                      if(centerMap != "Europe"){
                        sp::plot(Maps$ocean160, add = T, col = "lightblue", lwd = 1, border = NA);
                        sp::plot(Maps$ocean200, add = T, col = "lightblue", lwd = 1, border = NA);
                      } else {
                        sp::plot(Maps$ocean, add = T, col = "lightblue", lwd = 1);
                      }
                    }
                    if (terrestrial == -1){
                      if(centerMap != "Europe"){
                        sp::plot(Maps$land160, add = T, lwd = 1, col = "grey96", border = NA);
                        sp::plot(Maps$land200, add = T, lwd = 1, col = "grey96", border = NA);
                      } else {
                        sp::plot(Maps$land, add = T, lwd = 1, col = "grey96", border = NA);
                      }
                    }
                    if(centerMap != "Europe"){
                      sp::plot(Maps$coast160, add = T, lwd = 1);
                      sp::plot(Maps$coast200, add = T, lwd = 1);
                    } else {
                      sp::plot(Maps$coast, add = T, lwd = 1);
                    }
                    if (grid == TRUE){
                      if(centerMap != "Europe"){
                        sp::plot(Maps$grids160, add = T, col = "grey", lty = 2, xlim = c(0, 1));
                        sp::plot(Maps$grids200, add = T, col = "grey", lty = 2, xlim = c(0, 1));
                      } else {
                        sp::plot(Maps$grids, add = T, col = "grey", lty = 2, xlim = c(0, 1));
                      }

                    }
                    if(centerMap != "Europe"){
                      sp::plot(Maps$borders160, add = T, col = "darkgrey", lwd = 1);
                      sp::plot(Maps$borders200, add = T, col = "darkgrey", lwd = 1);

                    } else {
                      sp::plot(Maps$borders, add = T, col = "darkgrey", lwd = 1);
                    }
                    # add points and/or centroids ----
                    if (points == TRUE){
                      if(!is.null(pointLabels)){
                        pointLabels <- as.numeric(pointLabels)
                        pointLabels <- pointLabels - min(pointLabels, na.rm = TRUE)
                        pointLabels <- pointLabels / max(pointLabels, na.rm = TRUE)
                        pointSize <- (pointSize * pointLabels + 0.1) * 2
                      }
                      if(!is.null(pointColLabels)){
                        pointColLabels <- as.numeric(pointColLabels)
                        pointColLabels <- pointColLabels - min(pointColLabels, na.rm = TRUE)
                        pointColLabels <- pointColLabels / max(pointColLabels, na.rm = TRUE)
                        pColor <- (colorRampPalette(brewer.pal(9, colorsP))(101))[round(pointColLabels, 2) * 100]
                      }
                      if(cluster & !is.null(data$spatial_cluster)){
                        data <- data %>%
                          selectClusterGrouping(cluster, clusterResults)

                        # get centroids
                        if(clusterResults == 1){
                          data_names <- c("spatial_cluster", "long_centroid_spatial_cluster", "lat_centroid_spatial_cluster", "cluster")
                          centroids <- unique(data[, data_names])
                          centroids <- na.omit(centroids)
                        } else {
                          data_names <- c("temporal_group", "long_temporal_group_reference_point", "lat_temporal_group_reference_point", "cluster")
                          centroids <- unique(data[, data_names])
                        }
                        centroids <- centroids[order(centroids[,1]), ]
                        if(centerMap != "Europe"){
                          # centroidsPac <- centroids
                          # centroidsPac[,2][centroids[,2] < -20] <- centroidsPac[,2][centroids[,2] < -20] + 200
                          # centroidsPac[,2][centroids[,2] >= -20] <- (- 160 + centroidsPac[,2][centroids[,2] >= -20])
                          # centroids <- centroidsPac
                          centroids <- shiftLongitudesToPacific(centroids)
                        }
                      }

                      # add points (location marks) ----
                      ## if there is no clustering, or
                      ## if not show only centroids
                      if (!cluster || clusterAll != "-1") {
                        dataPlot <- data %>%
                          centerPlotData(centerMap = centerMap)

                        ## if there is no clustering, or
                        ## if not "Show points for all times"
                        if (!cluster || clusterAll == "1") {
                          dataPlot <- dataPlot %>%
                            filterT(addU = addU, time = time)
                        }

                        dataPlot <- dataPlot %>%
                          selectClusterGrouping(cluster, clusterResults)

                        points(dataPlot$Latitude ~ dataPlot$Longitude,
                               col = getPColor(dataPlot, cluster, palName = clusterCol, pColor = pColor),
                               lwd = 2,
                               pch = pointShape,
                               cex = pointSize)
                      }

                      # add centroids ----
                      if (cluster & !is.null(data$spatial_cluster)) {
                        points(centroids[, 2:3],
                               col = getPColor(centroids, cluster, palName = clusterCol, pColor = pColor),
                               lwd = 2,
                               pch = pointShape,
                               cex = pointSize * 2.5)
                        text(centroids[, 2:3],
                             labels = sprintf("%s_%s",
                               ifelse(clusterResults == "0", "Group", "Cluster"),
                               centroids$cluster),
                             pos = 4,
                             cex = fontSize * 1.5,
                             col = fontCol,
                             family = fontType)
                      }
                    }
                    # add custom labels ----
                    if (!is.null(textLabels)) {
                      if (centerMap != "Europe") {
                        text(dataPac$Latitude ~ dataPac$Longitude,
                             labels = as.character(textLabels), pos = 4,
                             cex = fontSize, col = fontCol, family = fontType)
                      } else {
                        text(data$Latitude ~ data$Longitude,
                             labels = as.character(textLabels), pos = 4,
                             cex = fontSize, col = fontCol, family = fontType)
                      }
                    }
                    # add custom points ----
                    if (!is.null(pointDat) & NROW(pointDat) > 0) {
                      points(x = pointDat$x, y = pointDat$y, cex = pointDat$pointSize, col = pointDat$pointColor, pch = 16)
                      text(pointDat$y ~ pointDat$x, labels = pointDat$label, pos = 4, cex = 1.75)
                    }
                    # update axis if center Pacific
                    if (centerMap != "Europe") {
                      lab <- pretty(rangex)
                      # lab[pretty(rangex) >= 20] <- lab[pretty(rangex) >= 20] - 200
                      # lab[pretty(rangex) < 20] <- lab[pretty(rangex) < 20] + 160
                      lab <- shiftLongitudesToPacific(pretty(rangex))
                      axis(1, at = pretty(rangex), labels = lab, cex.axis = AxisLSize);
                    } else{
                      axis(1, cex.axis = AxisLSize);
                    }
                    axis(2, cex.axis = AxisLSize);

                  }
  )
  # }  else {
  #   plot(1:10, xlim = range(data$Longitude), ylim = range(data$Latitude),
  #        xlab = "Longitude", ylab = "Latitude")
  #   plot(Maps$ocean, add = T, col = "lightblue", lwd = 1)
  #   sp::plot(Maps$grids, add = T, col = "grey", lty = 2, xlim = c(0, 1))
  #   sp::plot(Maps$borders, add = T, col = "darkgrey", lwd = 1)
  #   text(mean(data$Longitude), mean(data$Latitude), cex = 1,
  #        "No estimates to plot, please adjust
  #         \"Display up to max standard error\" slider", col = "red")
  # }
  if (arrow == TRUE){
    if(showScale == TRUE){
      north.arrow(rangex[1] + diff(rangey) * NorthX, rangey[1] + diff(rangey) * NorthY,
                  diff(rangey) * 0.04 * northSize * 5,
                  1 * ((2 * diff(rangey)) / diff(rangex)) ^ 0.3* northSize * 5, c(0.5, - 0.25))
    } else {
      north.arrow(rangex[1] + diff(rangey) * NorthX, rangey[1] + diff(rangey) * NorthY,
                  diff(rangey) * 0.04 * northSize * 5,
                  1 * ((2 * diff(rangey)) / diff(rangex)) ^ 0.3* northSize * 5, c(0.5, - 0.25))
    }
  }
  if (scale == TRUE){
    if(showScale == TRUE){
      maps::map.scale(x = rangex[1] + diff(rangex) * scaleX,
                      y = rangey[1] + diff(rangey) * scaleY,
                      rel = scalSize * ((2 * diff(rangey)) / diff(rangex)) ^ 0.15,
                      cex = 0.75 * ((2 * diff(rangey)) / diff(rangex)) ^ -0.2, ratio = FALSE)
    } else {
      maps::map.scale(x = rangex[1] + diff(rangex) * scaleX,
                      y = rangey[1] + diff(rangey) * scaleY,
                      rel = scalSize * ((2 * diff(rangey)) / diff(rangex)) ^ 0.15,
                      cex = 0.75 * ((2 * diff(rangey)) / diff(rangex)) ^ -0.2, ratio = FALSE)

    }
  }

  if(plotRetNull){
    return(NULL)
  } else {
    return(list(XPred = XPred))
  }
}

#' Plots difference or similarity map
#'
#' @param XPred return object of similarityMap or createDifferenceMap functions
#' @param estType one of "Mean", "SE" and "Quantile". defaults to "Mean"
#' @param estQuantile quantile (only applicable if estType = "Quantile")
#' @param type one of "similarity" or "difference" determining the type of map
#' @param independent name of independent variable shown in plot
#' @param arrow display north arrow TRUE/FALSE
#' @param scale display scale TRUE/FALSE
#' @param terrestrial show only estimates on land masses (1), oceans (-1) or all (0)
#' @param grid show coordinate grid TRUE/FALSE
#' @param ncol number of colors for estimates
#' @param colors color scheme of estimates from RColorBrewer. defaults to "RdYlGn"
#' @param reverseColors inverse colour scheme
#' @param rangex range of longitude values (x axis limits)
#' @param rangey range of latitude values (y axis limits)
#' @param rangez range of estimated values (z axis limits)
#' @param centerMap center of map, one of "Europe" and "Pacific"
#' @param showValues boolean show values in plot?
#' @param simValues if showValues: list of simulated values
#' @param dataCenter data for batch estimation
#' @param RadiusBatch radius
#' @param showModel show model
#' @param titleMain show main title
#' @param titleScale show scale title
#' @param setAxisLabels show axis/main/scale labels
#' @param mainLabel main label
#' @param yLabel y lab label
#' @param xLabel y lab label
#' @param scLabel scale lab labels
#' @param northSize size of north arrow
#' @param scalSize size of scale
#' @param scaleX scale x orientation
#' @param scaleY scale y orientation
#' @param NorthX north arrow x orientation
#' @param NorthY north arrow y orientation
#' @param AxisSize axis title font size
#' @param AxisLSize axis label font size
#' @param pointDat data frame of points to add to plot
#' @inheritParams filled.contour2
#'
#' @export
plotDS <- function(XPred,
                   estType = "mean",
                   estQuantile = 0.95,
                   type = "similarity",
                   independent = "",
                   rangex = range(XPred$Longitude, na.rm = TRUE),
                   rangey = range(XPred$Latitude, na.rm = TRUE),
                   rangez = range(XPred$Est, na.rm = TRUE),
                   contourType = c("filled.contour", "contour"),
                   showModel = TRUE,
                   showScale = TRUE,
                   ncol = 10, colors = "RdYlGn",
                   centerMap = "Europe",
                   reverseColors = FALSE, terrestrial = 1, grid = TRUE,
                   arrow = TRUE, scale = TRUE,
                   simValues = NULL,
                   showValues = FALSE,
                   dataCenter = NULL, RadiusBatch = NULL,
                   titleMain = TRUE,
                   titleScale = TRUE,
                   setAxisLabels = FALSE,
                   mainLabel = "",
                   yLabel =  "Latitude",
                   xLabel =  "Longitude",
                   scLabel =  "",
                   northSize = 0.2,
                   scalSize = 0.1,
                   scaleX = 0,
                   scaleY = 0.1,
                   NorthX = 0.025,
                   NorthY = 0.925,
                   AxisSize = 1,
                   AxisLSize = 1,
                   pointDat = NULL){
  options(scipen = 999)
  contourType <- match.arg(contourType)
  RadiusBatch <-  RadiusBatch / 111
  minRangeFactor <- 0.75
  if ((diff(rangex) / diff(rangey)) < minRangeFactor) {
    rangex[1] <- max(-180, mean(rangex) - minRangeFactor / 2 * diff(rangey))
    rangex[2] <- min(180, mean(rangex) + minRangeFactor / 2 * diff(rangey))
  }
  minRangeFactor <- 0.5
  if((diff(rangey) / diff(rangex)) < minRangeFactor){
    rangey[1] <- max(-90, mean(rangey) - minRangeFactor / 2 * diff(rangex))
    rangey[2] <- min(90, mean(rangey) + minRangeFactor / 2 * diff(rangex))
  }

  if (!is.null(dataCenter)){
    # this is batch mode
    XPred2 <- do.call("rbind", lapply(1:nrow(dataCenter), function(x) {
      longitudes <- seq(dataCenter[x, 1] - RadiusBatch,
                        dataCenter[x, 1] + RadiusBatch, length.out = 4)
      latitudes <- seq(dataCenter[x, 2] - RadiusBatch,
                       dataCenter[x, 2] + RadiusBatch, length.out = 4)
      XPred2 <- expand.grid(Longitude = longitudes, Latitude = latitudes)
      cbind(XPred2, id = x)
    }))
    closestMean <- sapply(1:nrow(XPred2), function(x)
      XPred$Est[which.min((XPred2$Longitude[x] - XPred$Longitude) ^ 2 +
                            (XPred2$Latitude[x] - XPred$Latitude) ^ 2
      )])
    closestSD <- sapply(1:nrow(XPred2), function(x)
      XPred$Sd[which.min((XPred2$Longitude[x] - XPred$Longitude) ^ 2 +
                           (XPred2$Latitude[x] - XPred$Latitude) ^ 2
      )])

    closestDist <- sapply(1:nrow(XPred2), function(x)
      min((XPred2$Longitude[x] - XPred$Longitude) ^ 2 +
            (XPred2$Latitude[x] - XPred$Latitude) ^ 2
      ))

    closestMean[closestDist > RadiusBatch / 5] <- NA
    closestSD[closestDist > RadiusBatch / 5] <- NA

    XPred2$Mean <- closestMean
    XPred2$SD <- closestSD
    XPred2 <- do.call("rbind", lapply(1:max(XPred2$id), function(x) {
      tmp <- XPred2[XPred2$id == x,]
      data.frame(
        Longitude = tmp$Longitude[1],
        Latitude = tmp$Latitude[1],
        mean = signif(mean(tmp$Mean, na.rm = TRUE), 3),
        sd = signif(mean(tmp$SD, na.rm = TRUE) +
                      sd(tmp$Mean, na.rm = TRUE), 3)
      )
    })
    )
    XPred2$IntLower <- XPred2$mean - 1.96 * XPred2$sd
    XPred2$IntUpper <- XPred2$mean + 1.96 * XPred2$sd
    return(XPred2)
  }

  if(estType == "1 SE"){
    XPred$Est <- XPred$Sd
  }
  if(estType == "2 SE"){
    XPred$Est <- 2 * XPred$Sd
  }
  if(estType == "Significance (p-value)"){
    XPred$Est <- pnorm(-abs(XPred$Est), 0, XPred$Sd) * 2
  }
  if(estType == "Significance (z-value)"){
    XPred$Est <- XPred$Est / XPred$Sd
  }

  if(estType == "Quantile"){
    XPred$Est <- XPred$Est + qnorm(estQuantile) * XPred$Sd
    if(type == "similarity"){
      XPred$Est <- pmax(0, XPred$Est)
    }
  }
  #if (!all(is.na(XPred$Est))){
  if(type == "similarity"){
    z <- matrix(XPred$Est, ncol = length(unique(XPred$Latitude)))
  }
  if(type == "difference"){
    z <- matrix(XPred$Est, ncol = length(unique(XPred$Latitude)))
  }

  Maps <- loadMaps()
  levels <- pretty(c(rangez[1], rangez[2]), n = ncol)

  colors <- colorRampPalette(brewer.pal(9, colors))(length(levels) - 1)

  if(reverseColors){
    colors <- rev(colors)
  }

  levelsLegend <- levels
  if(length(levels) > 25){
    par(fg = NA, col="black")
    levelsLegend <- pretty(c(rangez[1], rangez[2]),
                           n = pmin(20, ceiling(ncol / 2)))
  }

  # keep $Est for later calculation of mean and sd for center
  XPred$EstForCenter <- XPred$Est

  if(centerMap != "Europe"){
    XPredPac <- XPred
    # XPredPac$Longitude[XPred$Longitude < -20] <- XPredPac$Longitude[XPred$Longitude < -20] + 200
    # XPredPac$Longitude[XPred$Longitude >= -20] <- (- 160 + XPredPac$Longitude[XPred$Longitude >= -20])
    XPredPac$Longitude <- shiftLongitudesToPacific(XPred$Longitude)
    XPredPlot <- data.frame(XPredPac[order(XPredPac$Latitude, XPredPac$Longitude),])
    z <- matrix(XPredPlot$Est, ncol = length(unique(XPredPlot$Latitude)))
  } else {
    XPredPlot <- XPred
  }

  if(!showModel){
    z <- matrix(NA, ncol = length(unique(XPredPlot$Latitude)), nrow = length(unique(XPredPlot$Longitude)))
  }

  cex4 <- 1
  if(max(abs(z), na.rm = TRUE) > 9999 | max(abs(z), na.rm = TRUE) < 0.05){
    cex4 <- 0.7
  }

  if(titleMain){
    main = ""
  } else {
    if(setAxisLabels){
      main = mainLabel
    } else {
      main = independent
    }
  }

  if(titleScale){
    mainS = ""
  } else {
    if(setAxisLabels){
      mainS = scLabel
    } else {
      mainS = independent
    }
  }

  if(setAxisLabels){
    xlab = xLabel
    ylab = yLabel

  } else {
    xlab = "Longitude"
    ylab = "Latitude"
  }


  if (any(unique(XPredPlot$Longitude) != sort(unique(XPredPlot$Longitude))) |
      any(unique(XPredPlot$Latitude) != sort(unique(XPredPlot$Latitude)))){
    return("The meridian should match the corresponding region. Spherical coordinates should also match. Try to adjust the map centering option.")
  }

  filled.contour2(unique(XPredPlot$Longitude), unique(XPredPlot$Latitude),
                  z = z,
                  contourType = contourType,
                  xlim = rangex, ylim = rangey, levels = levels,
                  col = colors,
                  showScale = showScale,
                  cex.axis = 1.5, cex.main = 1.5, cex.lab = 1.5,
                  asp = 1, key.axes = axis(side = 4, at = levelsLegend, cex.axis = cex4),
                  key.title = title(main = main, cex.main = 0.8),
                  plot.title = {title(cex.lab = AxisSize, xlab = xlab, ylab = ylab, main = mainS)},
                  plot.axes = {
                    par(fg = "black", col="black");
                    if (terrestrial == 1){
                      if(centerMap != "Europe"){
                        sp::plot(Maps$ocean160, add = T, col = "lightblue", lwd = 1, border = NA);
                        sp::plot(Maps$ocean200, add = T, col = "lightblue", lwd = 1, border = NA);
                      } else {
                        sp::plot(Maps$ocean, add = T, col = "lightblue", lwd = 1);
                      }
                    }
                    if (terrestrial == -1){
                      if(centerMap != "Europe"){
                        sp::plot(Maps$land160, add = T, lwd = 1, col = "grey96", border = NA);
                        sp::plot(Maps$land200, add = T, lwd = 1, col = "grey96", border = NA);
                      } else {
                        sp::plot(Maps$land, add = T, lwd = 1, col = "grey96", border = NA);
                      }
                    }
                    if(centerMap != "Europe"){
                      sp::plot(Maps$coast160, add = T, lwd = 1);
                      sp::plot(Maps$coast200, add = T, lwd = 1);
                    } else {
                      sp::plot(Maps$coast, add = T, lwd = 1);
                    }
                    if (grid == TRUE){
                      if(centerMap != "Europe"){
                        sp::plot(Maps$grids160, add = T, col = "grey", lty = 2, xlim = c(0, 1));
                        sp::plot(Maps$grids200, add = T, col = "grey", lty = 2, xlim = c(0, 1));
                      } else {
                        sp::plot(Maps$grids, add = T, col = "grey", lty = 2, xlim = c(0, 1));
                      }

                    }
                    if(centerMap != "Europe"){
                      sp::plot(Maps$borders160, add = T, col = "darkgrey", lwd = 1);
                      sp::plot(Maps$borders200, add = T, col = "darkgrey", lwd = 1);

                    } else {
                      sp::plot(Maps$borders, add = T, col = "darkgrey", lwd = 1);
                    }
                    if(centerMap != "Europe"){
                      lab <- pretty(rangex)
                      # lab[pretty(rangex) >= 20] <- lab[pretty(rangex) >= 20] - 200
                      # lab[pretty(rangex) < 20] <- lab[pretty(rangex) < 20] + 160
                      lab <- shiftLongitudesToPacific(pretty(rangex))
                      axis(1, at = pretty(rangex), labels = lab, cex.axis = AxisLSize);
                    } else{
                      axis(1, cex.axis = AxisLSize);
                    }
                    axis(2, cex.axis = AxisLSize);
                    if(!is.null(pointDat) & NROW(pointDat) > 0){
                      points(x = pointDat$x, y = pointDat$y, cex = pointDat$pointSize, col = pointDat$pointColor, pch = 16)
                      text(pointDat$y ~ pointDat$x, labels = pointDat$label, pos = 4, cex = 1.75)
                    }
                    if(showValues == TRUE){
                      op <- par(family = "mono")
                      titel = paste(capture.output(do.call("rbind", simValues)), collapse='\n')
                      legend("bottomright", legend = c(""), pch = 26, title= titel ,
                             text.width = strwidth(titel)[1]/2, bty = "n", cex = 2, text.font = 2)
                      par(op)
                    }
                  }
  )
  # }  else {
  #   plot(1:10, xlim = range(XPred$Longitude), ylim = range(XPred$Latitude),
  #        xlab = "Longitude", ylab = "Latitude")
  #   plot(Maps$ocean, add = T, col = "lightblue", lwd = 1)
  #   sp::plot(Maps$grids, add = T, col = "grey", lty = 2, xlim = c(0, 1))
  #   sp::plot(Maps$borders, add = T, col = "darkgrey", lwd = 1)
  #   text(mean(XPred$Longitude), mean(XPred$Latitude), cex = 1,
  #        "No estimates to plot, please adjust \"Display up to max standard error\" slider",
  #        col = "red")
  # }
  if (arrow == TRUE){
    if(showScale == TRUE){
      north.arrow(rangex[1] + diff(rangey) * NorthX, rangey[1] + diff(rangey) * NorthY,
                  diff(rangey) * 0.04 * northSize * 5,
                  1 * ((2 * diff(rangey)) / diff(rangex)) ^ 0.3* northSize * 5, c(0.5, - 0.25))
    } else {
      north.arrow(rangex[1] + diff(rangey) * NorthX, rangey[1] + diff(rangey) * NorthY,
                  diff(rangey) * 0.04 * northSize * 5,
                  1 * ((2 * diff(rangey)) / diff(rangex)) ^ 0.3* northSize * 5, c(0.5, - 0.25))
    }
  }
  if (scale == TRUE){
    if(showScale == TRUE){
      maps::map.scale(x = rangex[1] + diff(rangex) * scaleX,
                      y = rangey[1] + diff(rangey) * scaleY,
                      rel = scalSize * ((2 * diff(rangey)) / diff(rangex)) ^ 0.15,
                      cex = 0.75 * ((2 * diff(rangey)) / diff(rangex)) ^ -0.2, ratio = FALSE)
    } else {
      maps::map.scale(x = rangex[1] + diff(rangex) * scaleX,
                      y = rangey[1] + diff(rangey) * scaleY,
                      rel = scalSize * ((2 * diff(rangey)) / diff(rangex)) ^ 0.15,
                      cex = 0.75 * ((2 * diff(rangey)) / diff(rangex)) ^ -0.2, ratio = FALSE)

    }
  }
  return(list(XPred = XPred))
}

#' Filled Countour 2
#' Wrapper for contour plot
#'
#' @param x x values
#' @param y y values
#' @param z z values
#' @param contourType one of "filled.contour" or "contour"
#' @param xlim x limits
#' @param ylim y limits
#' @param zlim z limits
#' @param levels levels
#' @param nlevels number of levels
#' @param showScale show colour scale
#' @param color.palette color palette
#' @param col colors
#' @param plot.title plot title
#' @param plot.axes plot axes
#' @param key.title key title
#' @param key.axes key axes
#' @param asp aspect ratio
#' @param xaxs x axis style
#' @param yaxs y axis style
#' @param las label style
#' @param axes show axes
#' @param frame.plot frame plot
#' @param ... additional arguments
filled.contour2 <- function(x = seq(0, 1, length.out = nrow(z)),
                            y = seq(0, 1, length.out = ncol(z)),
                            z,
                            contourType = c("filled.contour", "contour"),
                            xlim = range(x, finite = TRUE),
                            ylim = range(y, finite = TRUE), zlim = range(z, finite = TRUE),
                            levels = pretty(zlim, nlevels), nlevels = 20, showScale = TRUE,
                            color.palette = cm.colors, col = color.palette(length(levels) - 1),
                            plot.title, plot.axes, key.title, key.axes, asp = NA, xaxs = "i",
                            yaxs = "i", las = 1, axes = TRUE, frame.plot = axes, ...)
{
  contourType <- match.arg(contourType)

  if (missing(z)) {
    if (!missing(x)) {
      if (is.list(x)) {
        z <- x$z
        y <- x$y
        x <- x$x
      }
      else {
        z <- x
        x <- seq.int(0, 1, length.out = nrow(z))
      }
    }
    else stop("no 'z' matrix specified")
  }
  else if (is.list(x)) {
    y <- x$y
    x <- x$x
  }
  if (any(diff(x) <= 0) || any(diff(y) <= 0))
    stop("increasing 'x' and 'y' values expected")

  # setup layout
  mar.orig <- (par.orig <- par(c("mar", "las", "mfrow")))$mar
  on.exit(par(par.orig))

  par(las = las)
  pin1 <- par("pin")

  ratioLim <- abs(diff(ylim)) / abs(diff(xlim))

  if (showScale && contourType == "filled.contour") {
    # set up colour legend
    w <- (3 + mar.orig[2L]) * par("csi") * 2.54
    layout(matrix(c(2, 1), ncol = 2L), widths = c(1, lcm(w)))

  mar <- mar.orig
  mar[4L] <- mar[2L]
  mar[2L] <- 1
  par(mar = mar)

  a = (pin1[1] + par("mai")[2] + par("mai")[4])
  b = (pin1[2] + par("mai")[1] + par("mai")[3])
  ratioXY <- (a / b)

  if ((1 / ratioLim) >= ratioXY) {
    par(plt = getPlotRegion(bottom = 0.5, ratioLim = ratioLim, ratioXY = ratioXY))
  }
  if ((1 / ratioLim) < ratioXY) {
    par(plt = c(0.15, 0.5, 0.15, 0.9))
  }

  # create colour legend
  plot.new()
  plot.window(xlim = c(0, 1), ylim = range(levels), xaxs = "i",
              yaxs = "i")

  rect(0, levels[-length(levels)], 1, levels[-1L], col = col)

  if (missing(key.axes)) {
    if (axes)
      if (max(abs(z), na.rm = TRUE) < 10000) {
        axis(4)
      } else {
        axis(4, cex.axis = 0.7)
      }
  }
  else key.axes

  box()

  if (!missing(key.title))
    key.title
  }

  # set up (filled.)contour
  mar <- mar.orig
  mar[4L] <- 1
  par(mar = mar)

  a = (pin1[1] + par("mai")[2] + par("mai")[4])
  b = (pin1[2] + par("mai")[1] + par("mai")[3])
  ratioXY <- (a / b)

  if ((1 / ratioLim) >= ratioXY) {
    par(plt = getPlotRegion(bottom = 0.975, ratioLim = ratioLim, ratioXY = ratioXY))
  }
  if ((1 / ratioLim) < ratioXY) {
    add <- 1 / ratioXY / 2 * 0.75 / ratioLim
    par(plt = c(0.975 - 2 * add, 0.975, 0.15, 0.9))
  }

  # create (filled.)contour
  plot.new()
  if (contourType == "contour") {
    # contour plot ----
    contour(x = x, y = y, z = z, nlevels = length(levels), col = col,
            xlim = xlim, ylim = ylim, xaxs = xaxs, yaxs = yaxs, asp = asp)
  } else {
    # filled.contour plot ----
    plot.window(xlim, ylim, "", xaxs = xaxs, yaxs = yaxs, asp = asp)
    .filled.contour(x, y, z, levels, col)
  }

  # set plot axis and background
  if (missing(plot.axes)) {
    if (axes) {
      title(main = "", xlab = "", ylab = "")
      Axis(x, side = 1)
      Axis(y, side = 2)
    }
  }
  else plot.axes

  if (frame.plot)
    box()

  if (missing(plot.title))
    title(...)
  else plot.title

  invisible()
}

getPlotRegion <- function(left = 0.1, bottom = 0.975, ratioLim = 1, ratioXY = 1) {
  c(left, bottom, 0.525 - ratioLim * ratioXY / 2 * 0.875, 0.525 + ratioLim * ratioXY / 2 * 0.875)
}

north.arrow = function(x, y, h, c, adj) {
  polygon(c(x, x, x + h/2), c(y - h, y, y - (1 + sqrt(3)/2) * h),
          col = "black", border = NA)
  polygon(c(x, x + h/2, x, x - h/2), c(y - h, y - (1 + sqrt(3)/2) *
                                         h, y, y - (1 + sqrt(3)/2) * h))
  text(x, y, "N", adj = adj, cex = c)
}

#' Plots time course map of a spatio-temporal model from estimateMap3D() function
#'
#' @param model return object of a spatio-temporal model from estimateMap3D() function
#' @param IndSelect for categorical model: selected category
#' @param independent name of independent variable shown in plot
#' @param trange range of longitude values (x axis limits)
#' @param resolution temporal grid resolution of displayed (higher is slower but better quality)
#' @param centerX longitude value to display time course plot for
#' @param centerY latitude value to display time course plot for
#' @param rangey 2-element vector of time interval to show
#' @param seType setype
#' @param pointDat add points/lines to plot
#' @param pointsTime should nearby points be plotted?
#' @param returnPred should the prediction be returned?
#' @param intTime should uncertainty intervals of nearby points be plotted?
#' @param rangePointsTime range of nearby points in km
#' @param limitz z limit range
#' @param formatTimeCourse parameters for the plot format, e.g. axesDecPlace, nLabelsX, nLabelsY
#'
#' @export
plotTimeCourse <- function(model, IndSelect = NULL,
                           independent = "", trange = range(model$data$Date),
                           resolution = 500, centerX = NA,
                           centerY = NA, rangey = NULL,
                           seType = "2",
                           pointDat = NULL,
                           pointsTime = FALSE,
                           returnPred = FALSE,
                           intTime = FALSE,
                           rangePointsTime = 500,
                           limitz = NULL,
                           formatTimeCourse = NULL){
  sdValue <- 1
  if(as.numeric(seType) > 5){
    sdValue <- 2
  }
  minVal <- -Inf
  maxVal <- Inf
  if(!is.null(limitz) && limitz == "0-100"){
    minVal <- 0
    maxVal <- 100
  }
  if(!is.null(limitz) && limitz == "0-1"){
    minVal <- 0
    maxVal <- 1
  }

  if(!is.null(model$IndependentType) && model$IndependentType != "numeric"){
    if(IndSelect == "" | is.null(IndSelect)){
      return(NULL)
    }
    model$model <- model$model[[IndSelect]]
    minVal <- max(minVal, 0)
    maxVal <- min(maxVal, 1)
  }

  Bayes = TRUE
  GAM = FALSE
  if("gamm" %in% class(model$model)){
    Bayes = FALSE
  }
  if("kde" %in% class(model$model)){
    GAM = TRUE
  }
  data <- model$data

  if (is.null(data)) return(NULL)
  if (is.na(centerX)){centerX <- signif(mean(data$Longitude, na.rm = TRUE), 3)}
  if (is.na(centerY)){centerY <- signif(mean(data$Latitude, na.rm = TRUE), 3)}
  time <- seq(trange[1], trange[2], length.out = resolution)

  XPred <- data.frame(time,
                      Date2 = (time - mean(data$Date)) /
                        sd(data$Date),
                      Longitude2 = (centerX - mean(data$Longitude)) / sd(data$Longitude),
                      Latitude2 = (centerY - mean(data$Latitude)) / sd(data$Latitude),
                      Longitude = centerX,
                      Latitude = centerY)

  if (Bayes == TRUE & GAM == FALSE){
    sc <- model$sc
    PredMatr <- Predict.matrix(sc, data = XPred)

    betas <- model$model$beta
    betaSigma <- model$model$betaSigma

    Predictions <-
      sapply(1:nrow(betas), function(x)
        PredMatr %*% betas[x, ] * model$sRe + model$mRe)

    if(!is.null(model$IndependentType) && model$IndependentType != "numeric"){
      Predictions <- invLogit(Predictions)
    }

    if(!is.null(betaSigma)){
      PredMatrV <- Predict.matrix(model$scV, data = XPred)
      PredictionsSigma <-
        rowMeans(sqrt(sapply(1:nrow(betaSigma), function(x)
          exp((PredMatrV %*% betaSigma[x, ])) / model$model$sigma[x]) * model$sRe^2))
    } else {
      if(!is.null(model$IndependentType) && model$IndependentType != "numeric"){
        PredictionsSigma <- sqrt(Predictions * (1-Predictions))
      } else {
        PredictionsSigma <- sqrt(mean(model$model$sigma) * model$sRe^2)
      }
    }

    if(!is.null(betaSigma)){
      SdTotal <- sqrt((PredictionsSigma)^2 + apply(Predictions, 1, sd)^2)
    } else {
      SdTotal <- sqrt(PredictionsSigma^2 + apply(Predictions, 1, sd)^2)
    }

    XPred <-
      data.frame(XPred,
                 Est = rowMeans(Predictions),
                 Sd = apply(Predictions, 1, sd),
                 SdTotal = SdTotal,
                 PredictionsSigma = PredictionsSigma,
                 IntLower = pmax(minVal, pmin(maxVal, rowMeans(Predictions) - sdValue * apply(Predictions, 1, sd))),
                 IntUpper = pmax(minVal, pmin(maxVal, rowMeans(Predictions) + sdValue * apply(Predictions, 1, sd))))
    mainlab <- paste0("Estimate of ", independent, " in time course at coordinates ",
                      "(", centerY,",", centerX,")" ," with credible intervals")
  }
  if (Bayes == FALSE & GAM == FALSE){
    Est <- predict(model$model$gam, XPred, se.fit = TRUE, type = "response")
    XPred <- data.frame(XPred, Est = Est$fit, Sd = Est$se.fit,
                        SdTotal = sqrt(Est$se.fit^2 + mean(residuals(model$model$gam)^2)),
                        PredictionsSigma = sd(residuals(model$model$gam)),
                        IntLower = pmax(minVal, pmin(maxVal, Est$fit - sdValue * Est$se.fit)),
                        IntUpper = pmax(minVal, pmin(maxVal, Est$fit + sdValue * Est$se.fit))
                        # IntLowerTotal = Est$fit - 1.96 * sqrt(Est$se.fit^2 + mean(residuals(model$model$gam)^2)),
                        # IntUpperTotal = Est$fit + 1.96 * sqrt(Est$se.fit^2 + mean(residuals(model$model$gam)^2))
    )
    mainlab <- paste0("Estimate of ", independent, " in time course at coordinates ",
                      "(", centerY,",", centerX,")" ," with confidence intervals")
  }
  if (GAM == TRUE){
    EstTemp <- sapply(1:length(model$model), function(x) predict(model$model[[x]],
                                                                 x =  cbind(XPred$Longitude, XPred$Latitude, XPred$Date2)))

    estQuantile <-
    XPred <- data.frame(XPred,
                        Est = rowMeans(EstTemp),
                        Sd = apply(EstTemp, 1, sd),
                        IntLower = pmax(0, apply(EstTemp, 1, quantile, 1 - pnorm(sdValue))),
                        IntUpper = pmax(0, apply(EstTemp, 1, quantile, pnorm(sdValue))))
    mainlab <- paste0("Density estimate in time course at coordinates ",
                      "(", centerY,",", centerX,").")

  }
  ylims <- c(min(XPred$Est - sdValue * XPred$Sd), max(XPred$Est + sdValue * XPred$Sd))

  if(!is.null(rangey)){
    ylims[!is.na(rangey)] = rangey[!is.na(rangey)]
  }

  plot(
    pmax(minVal, pmin(maxVal, XPred$Est)) ~ time,
    type = "l",
    ylim = ylims,
    xlim = c(min(time), max(time)),
    ylab = independent,
    xlab = "time",
    main = mainlab,
    xaxt = 'n',
    yaxt = 'n'
  )

  if (!is.null(formatTimeCourse)) {
    addFormattedAxis(
      axis = "x",
      min = min(time),
      max = max(time),
      nLabels = formatTimeCourse$nLabelsX,
      decPlace = formatTimeCourse$axesDecPlace
    )

    addFormattedAxis(
      axis = "y",
      min = ylims[1],
      max = ylims[2],
      nLabels = formatTimeCourse$nLabelsY,
      decPlace = formatTimeCourse$axesDecPlace
    )
  }

  if(seType %in% c("2", "4", "6", "9")){
    polygon(c(rev(time), time), pmax(minVal, pmin(maxVal, c(rev(XPred$IntUpper), XPred$IntLower))),
            col = 'grey90', border = NA)
    lines(pmax(minVal, pmin(maxVal, (XPred$Est))) ~ (time), lty = 1)
    lines(pmax(minVal, pmin(maxVal, (XPred$IntUpper))) ~ (time), lty = 2)
    lines(pmax(minVal, pmin(maxVal, (XPred$IntLower))) ~ (time), lty = 2)
  }
  if(seType %in% c("5", "7")){
    polygon(c(rev(time), time), pmax(minVal, pmin(maxVal,
                                                  c(rev(XPred$Est - sdValue * XPred$PredictionsSigma),
                                                    XPred$Est + sdValue * XPred$PredictionsSigma))),
            col = 'grey90', border = NA)
    lines(pmax(minVal, pmin(maxVal, (XPred$Est))) ~ (time), lty = 1)
    lines(pmax(minVal, pmin(maxVal, (XPred$Est - sdValue * XPred$PredictionsSigma))) ~ (time), lty = 2)
    lines(pmax(minVal, pmin(maxVal, (XPred$Est + sdValue * XPred$PredictionsSigma))) ~ (time), lty = 2)
  }

  if(seType %in% c("3", "4", "8", "9")){
    lines(pmax(minVal, pmin(maxVal,(XPred$Est + sdValue * XPred$SdTotal))) ~ (time), lty = 3)
    lines(pmax(minVal, pmin(maxVal,(XPred$Est - sdValue * XPred$SdTotal))) ~ (time), lty = 3)
  }
  if(!is.null(pointDat) & NROW(pointDat) > 0){
    for(i in 1:nrow(pointDat)){
      if(!is.na(pointDat$y[i]) & !is.na(pointDat$x[i])){
        points(pointDat$y[i] ~ pointDat$x[i], col = pointDat$pointColor[i],
               cex = pointDat$pointSize[i], pch = 19)
        if(!is.na(pointDat$ymin[i]) & !is.na(pointDat$ymax[i])){
          lines(x = rep(pointDat$x[i], 2), y = c(pointDat$ymin[i], pointDat$ymax[i]),
                lwd = pointDat$pointSize[i])
        }
        if(!is.na(pointDat$xmin[i]) & !is.na(pointDat$xmax[i])){
          lines(y = rep(pointDat$y[i], 2), x = c(pointDat$xmin[i], pointDat$xmax[i]),
                lwd = pointDat$pointSize[i])
        }
      }
    }
  }
  if(returnPred){
    return(XPred[, !(names(XPred) %in% c("Date2", "Longitude2", "Latitude2"))])
  }
  if(pointsTime){
    pointPlotData <- model$data[(sqrt((model$data$Longitude - centerX)^2 +
                                        (model$data$Latitude - centerY)^2) * 111) < rangePointsTime, ]
    if(NROW(pointPlotData) > 0){
      if(model$independent == ""){
        pointPlotData$ind <- 0
        ind <- "ind"
      } else {
        if(!is.null(model$IndependentType) && model$IndependentType == "numeric"){
          ind <- model$independent
        } else {
          if(IndSelect == "" | is.null(IndSelect)){
            ind <- model$independent
          } else {
            ind <- IndSelect
          }
        }
      }
      points(pointPlotData[, ind] ~ pointPlotData$Date)
      if(intTime){
        for(i in 1:nrow(pointPlotData)){
          lines(c((pointPlotData$Date[i] - 2 * pointPlotData$Uncertainty[i]),
                  (pointPlotData$Date[i] + 2 * pointPlotData$Uncertainty[i])),
                c(pointPlotData[, ind][i], pointPlotData[, ind][i]))
        }
        pointPlotData <- pointPlotData[(pointPlotData$Date + 2 * pointPlotData$Uncertainty > trange[1]) &
                                         (pointPlotData$Date - 2 * pointPlotData$Uncertainty < trange[2]), ]
      } else {
        pointPlotData <- pointPlotData[(pointPlotData$Date > trange[1]) &
                                         (pointPlotData$Date < trange[2]), ]
        }
    }
    return(pointPlotData)
  }
  return(NULL)
}

#' Add Formatted Axis
#'
#' @param axis (character) axis to add, either "x" or "y"
#' @param min (numeric) position of 1st label
#' @param max (numeric) position of last label
#' @param nLabels (numeric) number of displayed labels
#' @param decPlace (numeric) number of the label's decimal places
addFormattedAxis <- function(axis, min, max, nLabels = 7, decPlace = 0) {
  labelPositions <- seq(min, max, length.out = nLabels)

  axisType <- switch(axis,
                     "x" = 1,
                     "y" = 2)

  axis(axisType,
       at = labelPositions,
       labels = sprintf(paste('%1.', decPlace, 'f', sep = ""), labelPositions))

  return(NULL)
}

plotTimeIntervals <- function(Model,
                              trange = c(0, 1000),
                              AxisSize = 1,
                              AxisLSize = 1,
                              cluster = FALSE,
                              clusterCol = "Set1",
                              clusterResults = 0
){
  dat <- Model$data
  dat <- dat %>%
    selectClusterGrouping(cluster = cluster,
                          clusterResults = clusterResults)
  if(!is.null(dat$spatial_cluster)){
    dat$cluster_color <- colorRampPalette(brewer.pal(8, clusterCol))(max(dat$cluster, na.rm=TRUE))[dat$cluster]
    dat <- dat[!is.na(dat$cluster),]
    dat$cluster <- factor(dat$cluster)
    g <- ggplot(dat, aes_(~Date, ~cluster)) + theme_light() + coord_cartesian(xlim = trange) +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            axis.text=element_text(size=12 * AxisLSize),
            axis.title=element_text(size=14 * AxisSize), legend.position = "none") +
      geom_point(color=dat$cluster_color, position = position_dodge(0.3), alpha = 0.3) +
      geom_errorbar(
        aes_(xmin = ~ Date-2*Uncertainty, xmax = ~ Date + 2*Uncertainty),
        color=dat$cluster_color,
        position = position_dodge(0.3), width = 0.1, alpha = 0.3) +
      ylab(ifelse(clusterResults == "0", "temporal_group", "spatial_cluster"))
    print(g)
  } else {
    plot(1, cex = 0.1)
    text(x = 1, y= 1, "Please run KernelTimeR with Cluster Analysis (left Panel)")
  }
}

roundUpNice <- function(x, nice=c(1,2,5,10)) {
  y <- x
  if(y < 0){
    x <- -x
  }
  if(length(x) != 1) stop("'x' must be of length 1")
  x <- 10^floor(log10(x)) * nice[[which(x <= 10^floor(log10(x)) * nice)[[1]]]]
  if(y < 0){
    x <- -x
  }
  x
}

createSimilarityMap <- function(XPredList, pointList,
                                includeUncertainty = TRUE,
                                normalize = FALSE,
                                normalType = "1",
                                weightProb = FALSE,
                                weightMap = NULL,
                                negZero = TRUE,
                                invWeight = FALSE,
                                weightDecay = 1){

  XPredList <- lapply(1:length(XPredList), function(x){
    similarityMap(XPredList[[x]], pointList[[x]], includeUncertainty)
  })
  combineSimilarityMaps(XPredList,
                        normalize = normalize,
                        normalType = normalType,
                        weightProb = weightProb,
                        weightMap = weightMap,
                        negZero = negZero,
                        invWeight = invWeight,
                        weightDecay = weightDecay)
}

similarityMap <- function(XPred, point, includeUncertainty = TRUE) {
  if (includeUncertainty == TRUE) {
    density <- lapply(1:nrow(point), function(x)
      log(rowMeans(sapply(1:1000,
                          function(y)
                            dnorm(
                              rnorm(1, point$mean[x], point$sd[x]),
                              mean = XPred$Est,
                              sd = sqrt(XPred$Sd ^ 2 + XPred$resError ^ 2),
                              log = FALSE
                            )))))
    densitySD <- lapply(1:nrow(point), function(x)
      apply((sapply(1:1000,
                    function(y)
                      dnorm(
                        rnorm(1, point$mean[x], point$sd[x]),
                        mean = XPred$Est,
                        sd = sqrt(XPred$Sd ^ 2 + XPred$resError ^ 2),
                        log = FALSE
                      ))), 1, sd))
    XPred$densitySd <- Reduce("*", densitySD)

  } else {
    density <- lapply(1:nrow(point), function(x)
      dnorm(
        point$mean[x],
        mean = XPred$Est,
        sd = sqrt(XPred$Sd ^ 2 + XPred$resError ^ 2),
        log = TRUE)
    )
    XPred$densitySd <- 0
  }
  XPred$density <- exp(Reduce("+", density))

  XPred$Est <- XPred$density
  XPred$Sd <- XPred$densitySd
  XPred$IntLower <- pmax(0, XPred$Est - 1.96 * XPred$Sd)
  XPred$IntUpper <- XPred$Est + 1.96 * XPred$Sd

  return(XPred)
}

combineSimilarityMaps <- function(XPredList,
                                  normalize = FALSE,
                                  normalType = "1",
                                  weightProb = FALSE,
                                  weightMap = NULL,
                                  negZero = TRUE,
                                  invWeight = FALSE,
                                  weightDecay = 1) {
  XPred <- XPredList[[1]]
  XPred$density <- log(XPred$density)

  if (length(XPredList) > 1) {
    for (j in 2:length(XPredList)) {
      closest <- sapply(1:nrow(XPred), function(x)
        XPredList[[j]]$density[which.min((XPred$Longitude[x] - XPredList[[j]]$Longitude) ^ 2 +
                                           (XPred$Latitude[x] - XPredList[[j]]$Latitude) ^ 2
        )])
      closestSD <- sapply(1:nrow(XPred), function(x)
        XPredList[[j]]$densitySd[which.min((XPred$Longitude[x] - XPredList[[j]]$Longitude) ^ 2 +
                                             (XPred$Latitude[x] - XPredList[[j]]$Latitude) ^ 2
        )])

      closestDist <- sapply(1:nrow(XPred), function(x)
        min((XPred$Longitude[x] - XPredList[[j]]$Longitude) ^ 2 +
              (XPred$Latitude[x] - XPredList[[j]]$Latitude) ^ 2
        ))

      closest[closestDist > sqrt(100)] <- NA
      closestSD[closestDist > sqrt(100)] <- NA

      XPred$densitySd <- sqrt((XPred$densitySd ^ 2 + exp(XPred$density) ^ 2) * (closestSD ^ 2 + closest ^ 2) -
                                (exp(XPred$density) ^ 2) * (closest ^ 2))

      XPred$density <- XPred$density + log(closest)
    }
  }

  XPred$density <- exp(XPred$density)
  if(!is.null(weightMap) & weightProb == TRUE){
    closestWeights <- sapply(1:nrow(XPred), function(x)
              weightMap$predictions$Est[which.min((XPred$Longitude[x] - weightMap$predictions$Longitude) ^ 2 +
                                         (XPred$Latitude[x] - weightMap$predictions$Latitude) ^ 2
      )])
    closestWeightsOG <- closestWeights
    if(!negZero){
      closestWeights[closestWeightsOG<0] <- 0
    }

    if(invWeight){
      closestWeights <- exp(-log(2)/weightDecay * closestWeights)
    }
    if(negZero){
      closestWeights[closestWeightsOG<0] <- 0
    }

    XPred$density <- XPred$density * closestWeights
    XPred$densitySd <- XPred$densitySd * sqrt(closestWeights)
  }
  XPred$Est <- XPred$density

  if(normalize == TRUE){
    if(normalType == "1"){
      constant <- max(XPred$Est, na.rm = TRUE)
    } else {
      constant <- sum(XPred$Est, na.rm = TRUE) /
        (diff(sort(unique(XPred$Longitude))[1:2]) *
           diff(sort(unique(XPred$Latitude))[1:2]))
    }
  } else {
    constant <- 1
  }

  XPred$Est <- XPred$Est / constant
  XPred$Sd <- XPred$densitySd / sqrt(constant)
  XPred$IntLower <- pmax(0, XPred$Est - 1.96 * XPred$Sd)
  XPred$IntUpper <- XPred$Est + 1.96 * XPred$Sd

  return(XPred)
}

createDifferenceMap <- function(XPred1, XPred2, operation = "-") {
  if (inherits(XPred2, "numeric") && !inherits(XPred1, "numeric")) {
    XPredNew <- XPred1
    XPredNew$Est <- XPred2[1]
    XPredNew$Sd <- XPred2[2]
    XPred2 <- XPredNew
  }

  if (inherits(XPred1, "numeric") && !inherits(XPred2, "numeric")) {
    XPredNew <- XPred2
    XPredNew$Est <- XPred1[1]
    XPredNew$Sd <- XPred1[2]
    XPred1 <- XPredNew
  }

  if (inherits(XPred1, "numeric") && inherits(XPred2, "numeric")) {
    lo <- seq(-180, 180, by = 0.5)
    la <- seq(-90, 90, by = 0.5)
    coord <- expand.grid(lo, la)
    XPred1 <- data.frame(Est = XPred1[1], Sd = XPred1[2],
                         Longitude = coord[,1], Latitude = coord[,2])
    XPred2 <- data.frame(Est = XPred2[1], Sd = XPred2[2],
                         Longitude = coord[,1], Latitude = coord[,2])
  }
  closestMean <- sapply(1:nrow(XPred1), function(x)
    XPred2$Est[which.min((XPred1$Longitude[x] - XPred2$Longitude) ^ 2 +
                           (XPred1$Latitude[x] - XPred2$Latitude) ^ 2)])
  closestSd <- sapply(1:nrow(XPred1), function(x)
    XPred2$Sd[which.min((XPred1$Longitude[x] - XPred2$Longitude) ^ 2 +
                          (XPred1$Latitude[x] - XPred2$Latitude) ^ 2)])

  closestDist <- sapply(1:nrow(XPred1), function(x)
    min((XPred1$Longitude[x] - XPred2$Longitude) ^ 2 +
          (XPred1$Latitude[x] - XPred2$Latitude) ^ 2
    ))
  closestMean[closestDist > sqrt(2)] <- NA
  closestSd[closestDist > sqrt(2)] <- NA

  XPred <- XPred1
  if(operation == "-"){
    XPred$Est <- XPred1$Est - closestMean
    XPred$Sd <- sqrt(XPred1$Sd ^ 2 + closestSd ^ 2)
  }
  if(operation == "+"){
    XPred$Est <- XPred1$Est + closestMean
    XPred$Sd <- sqrt(XPred1$Sd ^ 2 + closestSd ^ 2)
  }
  if(operation == "*"){
    XPred$Est <- XPred1$Est * closestMean
    XPred$Sd <- sqrt(XPred1$Sd ^ 2 * closestSd ^ 2 +
                       XPred1$Sd ^ 2 * closestMean ^ 2 +
                       closestSd ^ 2 * XPred1$Est ^ 2)
  }
  if(operation == "/"){
    XPred$Est <- XPred1$Est / closestMean
    XPred$Sd <- sqrt(((XPred1$Est ^ 2) / (closestMean ^ 2)) *
                       ((XPred1$Sd ^ 2) / (XPred1$Est ^ 2) + (closestMean ^ 2) / (closestSd ^ 2)))
  }
  if(operation == "mean"){
    XPred$Est <- (XPred1$Est + closestMean) / 2
    XPred$Sd <- sqrt(XPred1$Sd ^ 2 + closestSd ^ 2) / sqrt(2)
  }
  if(operation == "weightedMean"){
    VAR <- 1 / (1 / (XPred1$Sd ^ 2) + 1 / (closestSd ^ 2))
    XPred$Est <- VAR * (XPred1$Est / (XPred1$Sd ^ 2)  + closestMean / (closestSd ^ 2))
    XPred$Sd <- sqrt(VAR)
  }
  if(operation == "weight"){
    constant <- mean(closestMean)
    closestMean <- closestMean / constant
    closestSd <- closestSd / constant
    XPred$Est <- XPred1$Est * closestMean
    XPred$Sd <- sqrt(XPred1$Sd ^ 2 * closestSd ^ 2 +
                       XPred1$Sd ^ 2 * closestMean ^ 2 +
                       closestSd ^ 2 * XPred1$Est ^ 2)
  }
  if(operation == "pMin"){
    XPred$Est <- pmin(XPred1$Est, closestMean, na.rm = T)
    XPred1$Est[is.na(XPred1$Est)] <- -Inf
    XPred$Sd <- sapply(1:length(XPred1$Sd), function(l) c(XPred1$Sd[l], closestSd[l])[which.min(c(XPred1$Est[l], closestMean[l]))])
    XPred$Sd[is.na(XPred$Est)] <- NA
  }
  if(operation == "pMax"){
    XPred$Est <- pmax(XPred1$Est, closestMean, na.rm = T)
    XPred1$Est[is.na(XPred1$Est)] <- Inf
    XPred$Sd <- sapply(1:length(XPred1$Sd), function(l) c(XPred1$Sd[l], closestSd[l])[which.max(c(XPred1$Est[l], closestMean[l]))])
    XPred$Sd[is.na(XPred$Est)] <- NA
  }

  XPred$IntLower <- XPred$Est - 1.96 * XPred$Sd
  XPred$IntUpper <- XPred$Est + 1.96 * XPred$Sd
  return(XPred)
}

getMinima <- function(XPredPlot, nMin = 3, minDist = 250, minima = "Min"){
  if(minima == "Max"){
    XPredPlot$Est <- - 1* XPredPlot$Est
  }
  mins <- rep(NA, nMin)
  mins[1] <- which.min(XPredPlot$Est)

  y = 1
  dist <- which(sqrt((XPredPlot$Longitude[mins[y]] - XPredPlot$Longitude) ^ 2 +
                       (XPredPlot$Latitude[mins[y]] - XPredPlot$Latitude) ^ 2) * 111 > minDist)

  while(y < nMin &  y < 9){
    newMin <- which.min(XPredPlot$Est[dist])
    newMinVal <- min(XPredPlot$Est[dist], na.rm = TRUE)
    if(length(na.omit(newMin)) == 0){
      return(na.omit(mins))
    }

    nearest <-
      sqrt((XPredPlot$Longitude[dist][newMin] - XPredPlot$Longitude[dist]) ^ 2 +
             (XPredPlot$Latitude[dist][newMin] - XPredPlot$Latitude[dist]) ^ 2)
    neighbours <- XPredPlot$Est[dist][which(nearest <= sort(nearest)[5])]
    if(newMinVal == min(neighbours, na.rm = TRUE)){
      y = y +1
      mins[y] = which(newMinVal == XPredPlot$Est)
      dist <- intersect(dist, which(sqrt((XPredPlot$Longitude[mins[y]] - XPredPlot$Longitude) ^ 2 +
                                           (XPredPlot$Latitude[mins[y]] - XPredPlot$Latitude) ^ 2) * 111 > minDist))
    } else {
      dist <- setdiff(dist, which(newMinVal == XPredPlot$Est))
    }
    if(length(na.omit(dist)) == 0){
      return(na.omit(mins))
    }
  }
  return(na.omit(mins))
}

#' Select Cluster Grouping
#'
#' @param data (data.frame) data if \code{cluster == TRUE} it must contain columns "spatial_cluster"
#'  and "temporal_group"
#' @param cluster (logical) if TRUE, a column "cluster" is added to data
#' @param clusterResults (integer) either \code{0} or \code{1}, using "temporal_group" or
#'  "spatial_cluster" for the column "cluster", respectively.
#'
#' @return (data.frame) data containing the column "cluster" if \code{cluster == TRUE}, else
#'  just the input \code{data}
selectClusterGrouping <- function(data, cluster, clusterResults) {
  if(cluster & !is.null(data$spatial_cluster)){
    if(clusterResults == 0){
      data$cluster <- data$temporal_group
    } else {
      data$cluster <- data$spatial_cluster
    }
  }

  return(data)
}

#' Get Point Color
#'
#' @param data (data.frame) if cluster, different ids in the column "cluster" will receive a different colour
#' @param cluster (logical) if TRUE, return color for cluster
#' @param palName (character) name of the color palette
#' @param pColor single color or vector of colors that is returned if \code{cluster == FALSE}
#'
#' @return single color or vector of colors

getPColor <- function(data, cluster, palName, pColor) {
  if (!cluster) return(pColor)

  colorRampPalette(brewer.pal(8, palName))(max(data$cluster))[data$cluster]
}
