# Collection of helper modules for the modelling tabs ----

## Formatting of decimal places ----

#' Center Estimate UI
#'
#' UI function of center estimate module
#'
#' @param id namespace
#' @param title title in tab
centerEstimateUI <- function(id, title = "") {
  ns <- NS(id)

  tagList(
    numericInput(
      inputId = ns("centerY"),
      label = "Center point latitude",
      min = -180,
      max = 180,
      value = c(),
      step = 0.5,
      width = "100%"
    ),
    numericInput(
      inputId = ns("centerX"),
      label = "Center point longitude",
      min = -90,
      max = 90,
      value = c(),
      step = 0.5,
      width = "100%"
    ),
    conditionalPanel(
      condition =
        "input.centerY != null && input.centerY != '' && input.centerX != null && input.centerX != '' && output.isCenterEstimateMap == true",
      numericInput(
        inputId = ns("decimalPlace"),
        label = "Decimal places for Mean/Error at the Center point",
        min = 0,
        max = 10,
        value = 2,
        step = 1,
        width = "100%"
      ),
      ns = ns
    ),
    sliderInput(
      inputId = ns("Radius"),
      label = "Radius (km)",
      min = 10,
      max = 300,
      value = 100,
      step = 10,
      width = "100%"
    )
  )
}


#' Center Estimate Server
#'
#' Backend for center estimate module
#'
#' @param id namespace id
#' @param predictions (reactive) all predictions
#' @param mapType (reactive) type of plot, either "Map", "Time course", "Time intervals by temporal group or cluster",
#'  "Spread", "Speed", "Minima/Maxima"
centerEstimateServer <-
  function(id, predictions, mapType = reactiveVal("Map")) {
    moduleServer(id,
                 function(input, output, session) {
                   gridLength <- reactiveVal(NULL)
                   meanCenter <- reactiveVal(NULL)
                   sdCenter <- reactiveVal(NULL)
                   centerEstimateText <- reactiveVal("")

                   centerEstimateMaps <- c("Map", "Spread")
                   # check which maps have the text in the previous version:
                   # currently not included: "Time course", "Time intervals by temporal group or cluster",
                   # SpreadR tab: "Speed" (a map but no meanCenter, sdCenter in the output available), "Minima/Maxima"

                   output$isCenterEstimateMap <- reactive({
                     mapType() %in% centerEstimateMaps
                   })
                   outputOptions(output, "isCenterEstimateMap", suspendWhenHidden = FALSE)

                   observe({
                     if (is.null(predictions())) {
                       # reset
                       gridLength(NULL)
                       meanCenter(NULL)
                       sdCenter(NULL)
                       return()
                     }

                     gridL <-
                       extractGridLength(
                         latitude = predictions()$Latitude,
                         longitude = predictions()$Longitude,
                         digits = 10
                       ) * 111
                     gridLength(gridL)

                     transformedRadius <- input$Radius / 111
                     predictionsCenter <- predictions() %>%
                       extractXPredCenter(
                         centerX = input$centerX,
                         centerY = input$centerY,
                         Radius = transformedRadius
                       )

                     centerEstimates <-
                       extractCenterEstimates(predictionsCenter, digits = 10)
                     meanCenter(centerEstimates$mean)
                     sdCenter(centerEstimates$sd)
                   })

                   observeEvent(list(meanCenter(), sdCenter(), input$decimalPlace), {
                     # meanCenter(), sdCenter() depend on input$centerX, input$centerY
                     # -> do not extra observe on input$centerX, input$centerY
                     if (is.na(input$centerY) |
                         is.na(input$centerX) |
                         is.na(input$Radius) |
                         !(mapType() %in% centerEstimateMaps) |
                         is.null(meanCenter()) |
                         is.null(sdCenter()) |
                         is.null(gridLength())) {
                       centerEstimateText("")
                     }

                     req(
                       input$centerX,
                       input$centerY,
                       input$Radius,
                       (mapType() %in% centerEstimateMaps)
                     )

                     if (is.na(meanCenter()) | is.na(sdCenter())) {
                       centerEstimateText(
                         "Cannot compute mean and sd at your provided coordinates. Please raise the
                       plot resolution or radius such that estimates within the radius are available."
                       )
                     }

                     req(meanCenter(), sdCenter())
                     centerEstimateText(
                       HTML(paste0(
                         "Mean: ",
                         round(meanCenter(), digits = input$decimalPlace),
                         ", Standard error of the mean: ",
                         round(sdCenter(), digits = input$decimalPlace),
                         "  at coordinates ",
                         "(",
                         input$centerY,
                         "\u00B0, " ,
                         input$centerX,
                         "\u00B0) for a ",
                         round(input$Radius, 3),
                         " km radius",
                         "<br/>",
                         "Grid length (plot resolution) for latitude, longitude: ",
                         paste(round(gridLength(), digits = input$decimalPlace), collapse = " km, "),
                         " km"
                       )
                     ))
                   })

                   list(
                     centerX = reactive(input$centerX),
                     centerY = reactive(input$centerY),
                     radius = reactive(input$Radius),
                     text = centerEstimateText
                   )
                 })
  }

#' Extract Grid Length
#'
#' @param latitude (numeric) latitude
#' @param longitude (numeric) longitude
#' @param digits (numeric) significant digits
extractGridLength <- function(latitude, longitude, digits = 5) {
  getGridLength <- function(x) {
    x[order(x)] %>%
      diff() %>%
      max() %>%
      signif(digits = digits)
  }

  c("latitude" = getGridLength(latitude),
    "longitude" = getGridLength(longitude))
}

#' Extract XPred Center
#'
#' @param XPred (data.frame) data.frame containing all estimates
#' @param centerX (numeric) center for longitude
#' @param centerY (numeric) center for latitude
#' @param Radius (numeric) radius of the area around the center
#' @param batch (logical) set TRUE if used for batch estimates
#' @param isThreeD (logical) set TRUE if used in plotMap3D
#' @param data (data.frame) data.frame with input data for the model
#' @param time (numeric) center of time
extractXPredCenter <-
  function(XPred,
           centerX,
           centerY,
           Radius,
           batch = FALSE,
           isThreeD = FALSE,
           data = NULL,
           time = NULL) {
    if (!isThreeD || is.null(data)) {
      # this is used in plotMap
      XPredCenter <- XPred[sqrt((XPred$Longitude - centerX) ^ 2 +
                                  (XPred$Latitude - centerY) ^ 2) < Radius,]
    } else if (!batch) {
      # this is used in plotMap3D without batch mode
      XPredCenter <-
        XPred[sqrt((((XPred$Longitude2 * sd(data$Longitude)) + mean(data$Longitude)
        ) - centerX) ^ 2 +
          (((XPred$Latitude2 * sd(data$Latitude)) +
              mean(data$Latitude)
          ) - centerY) ^ 2) < Radius,]
    } else {
      # this is used in plotMap3D with batch mode
      if (is.null(time))
        stop("Missing time variable in 3D estimation of batch mode.")
      XPredCenter <-
        XPred[sqrt(((
          XPred$Longitude2 * sd(data$Longitude) + mean(data$Longitude)
        ) - centerX) ^ 2 +
          ((
            XPred$Latitude2 * sd(data$Latitude) +
              mean(data$Latitude)
          ) - centerY) ^ 2) < Radius &
          XPred$time == time,]
    }

    XPredCenter
  }

#' Extract Center Estimates
#'
#' @param XPredCenter (data.frame) data.frame containing estimates around a center
#' @param digits (numeric) significant digits
#' @param batch (logical) set TRUE if used for batch estimates
extractCenterEstimates <-
  function(XPredCenter,
           digits = 5,
           batch = FALSE) {
    meanCenter <- signif(mean(XPredCenter$EstForCenter), digits = digits)

    if (!batch) {
      # this is used in plotMap without batch mode
      sdCenter <-
        signif(sd(XPredCenter$EstForCenter) + mean(XPredCenter$Sd), digits = digits)
    } else {
      # this is used in plotMap with batch mode
      sdCenter <-
        signif(sqrt(sum(
          sd(XPredCenter$EstForCenter) ^ 2, mean(XPredCenter$Sd) ^ 2, na.rm = TRUE
        )),
        digits = digits)
    }

    list(mean = meanCenter,
         sd = sdCenter)
  }


#' Format Time Course UI
#'
#' UI function for formatting of the time course plot
#'
#' @param id namespace
#' @param title title in tab
formatTimeCourseUI <- function(id, title = "") {
  ns <- NS(id)

  tagList(
    numericInput(
      inputId = ns("axesDecPlace"),
      label = "Decimal places for axes",
      min = 0,
      max = 10,
      value = 0,
      step = 1,
      width = "100%"
    ),
    fluidRow(
      column(
        width = 6,
        numericInput(
          inputId = ns("nLabelsX"),
          label = "N labels of x axis",
          min = 0,
          max = 20,
          value = 7,
          step = 1,
          width = "100%"
        )
      ),
      column(
        width = 6,
        numericInput(
          inputId = ns("nLabelsY"),
          label = "N labels of y axis",
          min = 0,
          max = 20,
          value = 7,
          step = 1,
          width = "100%"
        )
      )
    )
  )
}


#' Format Time Course Server
#'
#' Backend for formatting of the time course plot
#'
#' @param id namespace id
formatTimeCourseServer <-
  function(id) {
    moduleServer(id,
                 function(input, output, session) {
                   reactive(
                     list(
                       axesDecPlace = input$axesDecPlace,
                       nLabelsX = input$nLabelsX,
                       nLabelsY = input$nLabelsY
                     )
                   )
                 })
  }


## Time and Map Section ----


#' Time And Map Section UI
#'
#' UI of the module
#'
#' @param id id of module
#' @param label label
timeAndMapSectionUI <- function(id, label) {
  ns <- NS(id)
  tagList(
    tags$hr(),
    tags$h4(label),
    sliderAndNumericInputUI(
      ns("timeExtended"),
      label = "Time",
      min = 0,
      max = 15000,
      value = 5000,
      step = 100
    ),
    mapSectionUI(ns("mapSection")),
    fluidRow(
      column(
        width = 4,
        offset = 8,
        style = "margin-top: -60px;",
        align = "right",
        actionButton(ns("set"), "Set Time and Map Section")
      )
    ),
    tags$hr(),
  )
}


#' Time And Map Section Server
#'
#' Server function of the module
#' @param id id of module
#' @param dateMin (reactive) min date
#' @param dateMax (reactive) max date
#' @param dateValue (reactive) value date
#' @param dateStep (reactive) step date
#' @param zoomValue (reactive) default zoom given by model output
timeAndMapSectionServer <- function(id,
                                    dateMin,
                                    dateMax,
                                    dateValue,
                                    dateStep,
                                    zoomValue) {
  moduleServer(id,
               function(input, output, session) {
                 mapAndTimeSettings <- reactiveValues(
                   time = 5000,
                   upperLeftLongitude = NA,
                   upperLeftLatitude = NA,
                   zoom = 50,
                   set = 0
                 )

                 userInputTime <-
                   sliderAndNumericInputServer(
                     "timeExtended",
                     value = dateValue,
                     min = dateMin,
                     max = dateMax,
                     step = dateStep
                   )

                 mapSectionParams <-
                   mapSectionServer("mapSection", zoomValue = zoomValue)

                 # default values depend on model output
                 observeEvent(list(dateValue(),
                                   zoomValue()), {
                                     mapAndTimeSettings$time <- dateValue()
                                     mapAndTimeSettings$zoom <-
                                       zoomValue()
                                     mapAndTimeSettings$upperLeftLatitude <-
                                       mapSectionParams$upperLeftLatitude
                                     mapAndTimeSettings$upperLeftLongitude <-
                                       mapSectionParams$upperLeftLongitude
                                   })

                 # values given by the user pressing button
                 observeEvent(input$set, {
                   mapAndTimeSettings$time <- userInputTime()
                   mapAndTimeSettings$zoom <- mapSectionParams$zoom
                   mapAndTimeSettings$upperLeftLatitude <-
                     mapSectionParams$upperLeftLatitude
                   mapAndTimeSettings$upperLeftLongitude <-
                     mapSectionParams$upperLeftLongitude
                   mapAndTimeSettings$set <- input$set
                 })

                 return(mapAndTimeSettings)
               })
}


#' Map Section UI
#'
#' UI of the module
#'
#' @param id id of module
mapSectionUI <- function(id) {
  ns <- NS(id)
  tagList(
    sliderAndNumericInputUI(
      ns("zoom"),
      label = "Zoom/x-Range in degrees Longitude",
      min = 0.1,
      max = 360,
      value = 50,
      step = 1
    ),
    fluidRow(
      column(
        width = 3,
        numericInput(
          inputId = ns("upperLeftLatitude"),
          label = "Latitude of upper left corner",
          min = -90,
          max = 90,
          value = c()
        )
      ),
      column(
        width = 3,
        numericInput(
          inputId = ns("upperLeftLongitude"),
          label = "Longitude of upper left corner",
          min = -180,
          max = 180,
          value = c()
        )
      )
    )
  )
}


#' Map Section Server
#'
#' Server function of the module
#' @param id id of module
#' @param zoomValue (reactive) default zoom given by model output
mapSectionServer <- function(id,
                             zoomValue) {
  moduleServer(id,
               function(input, output, session) {
                 # there is a bug with default values after switching centers, zoom value is too small afterwards
                 mapSettings <- reactiveValues(
                   upperLeftLongitude = NA,
                   upperLeftLatitude = NA,
                   zoom = 50
                 )

                 zoomInput <- sliderAndNumericInputServer(
                   "zoom",
                   value = zoomValue,
                   min = reactive(0.1),
                   max = reactive(360),
                   step = reactive(1)
                 )

                 # update upperLeftLatitude/upperLeftLongitude if values$up/... change ----

                 observe({
                   mapSettings$zoom <- zoomInput()
                   mapSettings$upperLeftLatitude <-
                     input$upperLeftLatitude
                   mapSettings$upperLeftLongitude <-
                     input$upperLeftLongitude
                 })

                 return(mapSettings)
               })
}

## Z-scale settings ----

#' Z Scale UI
#'
#' UI of the module
#'
#' @param id id of module
zScaleUI <-
  function(id) {
    ns <- NS(id)
    tagList(
      selectInput(
        inputId = ns("estType"),
        label = "Estimation type",
        choices = NULL,
      ),
      conditionalPanel(
        ns = ns,
        condition = "input.estType == 'Quantile' || input.estType == 'QuantileTOTAL'",
        sliderInput(
          inputId = ns("Quantile"),
          label = "Estimation quantile",
          min = 0.01,
          max = 0.99,
          value = c(0.9),
          width = "100%"
        )
      ),
      checkboxInput(
        inputId = ns("showModel"),
        label = "Show model estimates",
        value = TRUE
      ),
      htmlOutput(ns("titleScaleInput"), style = "font-weight: bold"),
      numericInput(
        inputId = ns("max"),
        label = "Max range",
        value = 10
      ),
      numericInput(
        inputId = ns("min"),
        label = "Min range",
        value = 0
      ),
      conditionalPanel(
        ns = ns,
        condition = "output.restrictOption == 'show'",
        selectInput(
          inputId = ns("limit"),
          label = "Range restriction",
          choices = list(
            "No restriction" = "No restriction",
            "0-1" = "0-1",
            "0-100" = "0-100"
          )
        )
      )
    )
  }

#' Z Scale Server
#'
#' Server function of the module
#' @param id id of module
#' @param Model (reactive) model output
#' @param fixCol (reactive) user input if columns should be fixed, TRUE or FALSE
#' @param estimationTypeChoices (reactive) named characters of choices of estimation types
#' @param restrictOption (reactive) either "hide" or "show". If "show" than add user input to
#' restrict the z scale.
#' @param zValuesFun (reactive) function to extract zValues, either getZValues or getZValuesKernel
#' @param mapType (reactive) type of map, either "Map" or "Time course"; "Spread", "Speed" or
#'  "Minima/Maxima"
#' @param zValuesFactor (numeric) factor applied to zValues
#' @param mapType (character)
#' @param IndSelect (character) select category in case of categorical model
zScaleServer <- function(id,
                         Model,
                         fixCol,
                         estimationTypeChoices,
                         restrictOption,
                         zValuesFun,
                         zValuesFactor,
                         mapType = reactive("Map"),
                         IndSelect = NULL) {
  moduleServer(id,
               function(input, output, session) {
                 zModelValues <- reactiveVal(NULL)

                 values <- reactiveValues(
                   estType = NULL,
                   Quantile = NULL,
                   showModel = TRUE,
                   range = NULL,
                   limit = NULL
                 )

                 output$titleScaleInput <- renderText({
                   switch(
                     mapType(),
                     "Time course" = "Range of y axis:",
                     "Minima/Maxima" = "Range of y axis:",
                     "Range of estimates:"
                   )
                 })

                 observeEvent(estimationTypeChoices(), {
                   updateSelectInput(session,
                                     "estType",
                                     choices = estimationTypeChoices(),
                                     selected = "Mean")
                 })

                 observeEvent(list(input$estType, Model(), mapType()), {
                   req(input$estType)
                   validate(validInput(Model()))

                   if (!fixCol())
                     zModelValues(
                       zValuesFun(
                         estimationType = input$estType,
                         model = Model(),
                         mapType = mapType(),
                         factor = zValuesFactor,
                         IndSelect = IndSelect
                       )
                     )
                   else
                     zModelValues(NULL)

                   req(zModelValues())
                   values$estType <- input$estType
                   values$range <-
                     c(zModelValues()$minInput$value,
                       zModelValues()$maxInput$value)

                   updateNumericInput(
                     session,
                     "min",
                     value = zModelValues()$minInput$value,
                     min = zModelValues()$minInput$min,
                     max = zModelValues()$minInput$max
                   )

                   updateNumericInput(
                     session,
                     "max",
                     value = zModelValues()$maxInput$value,
                     min = zModelValues()$maxInput$min,
                     max = zModelValues()$maxInput$max
                   )

                   # reset Quantile
                   values$Quantile <- NULL

                   req(input$estType %in% c("Quantile", "QuantileTOTAL"))
                   values$Quantile <- input$Quantile
                 })

                 output$restrictOption <-
                   renderText({
                     restrictOption()
                   })
                 outputOptions(output, "restrictOption", suspendWhenHidden = FALSE)

                 # react slower on user input
                 zRange <- reactive({
                   if (input$estType == "Significance (p-value)") {
                     pmax(0, pmin(1, c(input$min, input$max)))
                   } else {
                     c(input$min, input$max)
                   }
                 })

                 zRange_d <- zRange %>% debounce(1000)

                 observeEvent(zRange_d(), {
                   req(zModelValues(),
                       (
                         values$range[1] != zRange_d()[1] |
                           values$range[2] != zRange_d()[2]
                       ))
                   values$range <- zRange_d()
                 })

                 observeEvent(input$limit, {
                   req(restrictOption() == "show", zModelValues())

                   rangez <-
                     c(zModelValues()$minInput$value,
                       zModelValues()$maxInput$value)

                   if (identical(input$limit, "0-1")) {
                     rangez <- pmax(0, pmin(1, rangez))
                     if (rangez[1] == rangez[2]) {
                       rangez <- c(0, 1)
                     }
                   }

                   if (identical(input$limit, "0-100")) {
                     rangez <- pmax(0, pmin(100, rangez))
                     if (rangez[1] == rangez[2]) {
                       rangez <- c(0, 100)
                     }
                   }

                   values$range <- c(min(rangez), max(rangez))
                   values$limit <- input$limit

                   updateNumericInput(
                     session,
                     "min",
                     value = min(rangez),
                     min = min(rangez),
                     max = max(rangez)
                   )
                   updateNumericInput(
                     session,
                     "max",
                     value = max(rangez),
                     min = min(rangez),
                     max = max(rangez)
                   )
                 })

                 observeEvent(input$Quantile, {
                   values$estType <- input$estType
                   values$Quantile <- input$Quantile
                 })

                 observeEvent(input$showModel, {
                   values$showModel <- input$showModel
                 })

                 values
               })
}

#' Get Z Values Map Sim
#'
#' @inheritParams getZvalues
getZValuesMapSim <-
  function(estimationType,
           model,
           mapType,
           factor = 1,
           IndSelect = NULL) {
    if (is.null(model))
      return(NULL)

    zValues <- list(
      minInput = list(value = 0, min = 0, max = 10),
      maxInput = list(value = 10, min = 0, max = 10)
    )

    if (estimationType %in% c("Mean", "Quantile")) {
      zRange <- model$Est
      minValue <- 0
      maxValue <- signif(max(zRange, na.rm = TRUE) * factor, 2)

      zValues$minInput <-
        list(value = minValue,
             min = minValue,
             max = maxValue)
      zValues$maxInput <-
        list(value = maxValue,
             min = minValue,
             max = maxValue)
      return(zValues)
    }

    if (estimationType %in% c("1 SE", "2 SE", "SE")) {
      sdVal <- ifelse(grepl("2", estimationType), 2, 1)
      zRange <- model$Sd
      maxValue <- signif(max(zRange, na.rm = TRUE) * sdVal, 2)

      zValues$minInput <-
        list(value = 0,
             min = 0,
             max = maxValue)
      zValues$maxInput <-
        list(value = maxValue,
             min = 0,
             max = maxValue)
      return(zValues)
    }
  }


#' Get Z Values Map Diff
#'
#' @inheritParams getZvalues
getZValuesMapDiff <-
  function(estimationType,
           model,
           mapType,
           factor = 1,
           IndSelect = NULL) {
    if (is.null(model))
      return(NULL)

    zValues <- list(
      minInput = list(value = 0, min = 0, max = 10),
      maxInput = list(value = 10, min = 0, max = 10)
    )

    if (estimationType %in% c("Mean",
                              "Quantile",
                              "Significance (p-value)",
                              "Significance (z-value)")) {
      zRange <- model$Est
      minValue <- min(zRange, na.rm = TRUE)
      maxValue <- max(zRange, na.rm = TRUE) * factor

      minValue <- signif(minValue,
                         which(round(abs(
                           diff(zRange) / minValue * 10 ^ (0:10)
                         ), 0) > 1)[1])
      maxValue <- signif(maxValue,
                         which(round(abs(
                           diff(zRange) / maxValue * 10 ^ (0:10)
                         ), 0) > 1)[1])

      minValue[is.na(minValue)] <- 0
      maxValue[is.na(maxValue)] <- 0

      zValues$minInput <-
        list(value = minValue,
             min = minValue,
             max = maxValue)
      zValues$maxInput <-
        list(value = maxValue,
             min = minValue,
             max = maxValue)
      return(zValues)
    }

    if (estimationType %in% c("1 SE", "2 SE", "SE")) {
      sdVal <- ifelse(grepl("2", estimationType), 2, 1)
      zRange <- model$Sd
      maxValue <- signif(max(zRange, na.rm = TRUE) * sdVal, 2)

      zValues$minInput <-
        list(value = 0,
             min = 0,
             max = maxValue)
      zValues$maxInput <-
        list(value = maxValue,
             min = 0,
             max = maxValue)
      return(zValues)
    }
  }


#' Get Z Values Kernel
#'
#' @inheritParams getZvalues
getZValuesKernel <-
  function(estimationType,
           model,
           mapType,
           factor = 1.25,
           IndSelect = NULL) {
    model <- model$model
    if (is.null(model))
      return(NULL)

    zValues <- list(
      minInput = list(value = 0, min = 0, max = 10),
      maxInput = list(value = 10, min = 0, max = 10)
    )

    if (estimationType %in% c("1 SE", "2 SE")) {
      sdVal <- ifelse(grepl("2", estimationType), 2, 1)
      zRange <-
        as.vector(apply(sapply(1:length(model), function(x)
          model[[x]]$estimate), 1, sd)) * sdVal
    } else {
      zRange <-
        as.vector(rowMeans(sapply(1:length(model), function(x)
          model[[x]]$estimate))) * factor
    }

    maxValue <- signif(max(zRange, na.rm = TRUE), 2)

    zValues$minInput <-
      list(value = 0,        min = 0, max = maxValue)
    zValues$maxInput <-
      list(value = maxValue, min = 0, max = maxValue)
    return(zValues)
  }


#' Get Z Values
#'
#' @param estimationType (character) type of estimate
#' @param model (list) model output
#' @param mapType (character) type of map, either "Map" or "Time course"
#' @param factor (numeric) factor applied to estimates
#' @param IndSelect (character) select category in case of categorical model
getZvalues <-
  function(estimationType,
           model,
           mapType,
           factor = 3,
           IndSelect = NULL) {
    zValues <-
      getZValuesInitial(IndependentType = model$IndependentType,
                        IndSelect = IndSelect)
    if (!is.null(model$IndependentType) &&
        model$IndependentType != "numeric" &&
        (is.null(IndSelect) || IndSelect == "")) {
      return(zValues)
    }

    model <- getModel(model = model, IndSelect = IndSelect)
    if (is.null(model))
      return(NULL)

    if (mapType == "Speed") {
      maxValue <- signif(50000 / diff(model$range$mean), 1)

      zValues$minInput <-
        list(value = 1,
             min = 0,
             max = maxValue)
      zValues$maxInput <-
        list(value = maxValue,
             min = 0,
             max = maxValue * 100)
      return(zValues)
    }

    if (mapType == "Time course" ||
        estimationType %in% c("Mean", "Quantile", "QuantileTOTAL")) {
      defaultMin <- getDefaultZMin(model$range$mean)
      defaultMax <- getDefaultZMax(model$range$mean)
      zValues$minInput <-
        list(value = defaultMin,
             min = defaultMin,
             max = defaultMax)
      zValues$maxInput <-
        list(value = defaultMax,
             min = defaultMin,
             max = defaultMax)
      return(zValues)
    }

    val <- 10   # default value
    if (estimationType %in% c("1 SETOTAL", "2 SETOTAL", "1 SD Population", "2 SD Population") &&
        mapType != "Time course") {
      val <- getDefaultZError(estimationType, model$range$seTotal)
    }

    if (estimationType %in% c("1 SE", "2 SE")  &&
        mapType != "Time course") {
      val <- getDefaultZError(estimationType, model$range$se)
    }

    zValues$minInput <- list(value = 0,
                             min = 0,
                             max = val * factor)
    zValues$maxInput <- list(value = val,
                             min = 0,
                             max = val * factor)
    return(zValues)
  }


getZValuesInitial <- function(IndependentType, IndSelect) {
  if (is.null(IndependentType) ||
      IndependentType == "numeric" ||
      (!is.null(IndSelect) && IndSelect != "")) {
    list(
      minInput = list(
        value = 0,
        min = 0,
        max = 10
      ),
      maxInput = list(
        value = 10,
        min = 0,
        max = 10
      )
    )
  } else {
    list(
      minInput = list(
        value = 0,
        min = 0,
        max = 1
      ),
      maxInput = list(
        value = 1,
        min = 0,
        max = 1
      )
    )
  }
}

getModel <- function(model, IndSelect) {
  if (!is.null(model$IndependentType) &&
      model$IndependentType != "numeric" &&
      !is.null(IndSelect) && IndSelect != "") {
    model$model[[IndSelect]]
  } else {
    model$model
  }
}

#' Get Default Z Error
#'
#' @param estType (character) type of estimate
#' @param range (numeric) range from model output
getDefaultZError <- function(estType, range) {
  sdVal <- ifelse(grepl("2", estType), 2, 1)
  3 * signif(1.1 * max(range) * sdVal, 2)
}

#' Get Default Z Min
#'
#' @param mean mean from model output
getDefaultZMin <- function(mean) {
  shift <- 0.1 * diff(mean)
  if (shift == 0) shift <- 0.0001 * mean[1]

  defaultMin <- mean[1] - shift

  digits <- which(round(abs(diff(mean) / mean[1] * 10 ^ (0:10)), 0) > 1)[1]
  if (is.na(digits)) return(defaultMin)

  signif(defaultMin, digits)
}

#' Get Default Z Max
#'
#' @param mean mean from model output
getDefaultZMax <- function(mean) {
  shift <- 0.1 * diff(mean)
  if (shift == 0) shift <- 0.0001 * mean[2]

  defaultMax <- mean[2] + shift

  digits <- which(round(abs(diff(mean) / mean[2] * 10 ^ (0:10)), 0) > 1)[1]
  if (is.na(digits)) return(defaultMax)

  signif(defaultMax, digits)
}


# Helper functions for the modelling tabs ----

#' Extract Zoom From Long Range
#'
#' @param rangeLongitude (numeric) range of longitude vector
#' @param mapCentering (character) centering of the map, either "Europe" or "Pacific"
extractZoomFromLongRange <- function(rangeLongitude, mapCentering) {
  if (mapCentering == "Europe") {
    rangeLong <- diff(range(rangeLongitude, na.rm = TRUE) + c(-1, 1))
  } else {
    longRange <- list(Longitude = rangeLongitude) %>%
      centerData(center = mapCentering) %>%
      range()
    longRange[rangeLongitude < -20] <-
      longRange[rangeLongitude < -20] + 200
    longRange[rangeLongitude >= -20] <-
      (-160 + longRange[rangeLongitude >= -20])
    rangeLong <- diff(range(longRange, na.rm = TRUE) + c(-1, 1))
  }

  pmin(360, pmax(0, rangeLong, na.rm = TRUE)) %>% round()
}
