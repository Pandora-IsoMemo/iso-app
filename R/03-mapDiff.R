
#' ui function of modelResultsDiffSim module
#'
#' @param id namespace
#' @param title title in tab
#'
#' @export
modelResultsDiffUI <- function(id, title = ""){
  ns <- NS(id)
  tabPanel(
    title,
    id = id,
    value = id,
    fluidRow(
      class = "modeling-content",
      sidebarPanel(
        width = 2,
        selectInput(ns("dataSource"),
                    "Data source",
                    choices = c("Create map" = "create",
                                "Load OperatoR map" = "model"),
                    selected = "db"),
        conditionalPanel(
          condition = "input.dataSource == 'model'",
        selectInput(ns("savedModel"),
                    "Select Map",
                    choices = c(""),
                    selected = ""),
        actionButton( ns("load"), "load"),
        ns = ns
        ),
        conditionalPanel(
          condition = "input.dataSource == 'create'",
        selectInput(inputId = ns("targetMap1"),
                    label = "Map1:",
                    choices = NULL),
        selectInput(inputId = ns("targetMap2"),
                    label = "Map2:",
                    choices = NULL),
        selectInput(inputId = ns("operation"),
                    label = "Operation",
                    choices = c("Difference" = "-", "Sum" = "+",
                                "Multiply" = "*", "Ratio" = "/",
                                "Mean" = "mean", "Min" = "pMin",
                                "Max" = "pMax",
                                "Weight" = "weight",
                                "Weighted mean" = "weightedMean")),
        actionButton( ns("createDiffMap"), "Run"),
        conditionalPanel(
          condition = conditionPlot(ns("DistMap")),
          checkboxInput(inputId = ns("fixCol"),
                        label = "Fix colours and ranges ",
                        value = FALSE, width = "100%")
        ),
        ns = ns
        )
      ),
      mainPanel(
        width = 8,
        div(class = "aspect-16-9", div(
          plotOutput(outputId = ns("DistMap"), width = "100%", height = "100%")
        )),
        conditionalPanel(
          condition = conditionPlot(ns("DistMap")),
          textOutput(ns("centerEstimate"), container = function(...) div(..., style = "text-align:center;")),
          div(
            style = "display:flex;",
            div(
              class = "zoom-map",
              sliderInput(inputId = ns("zoom"),
                          label = "Zoom/x-Range in degrees Longitude",
                          min = 0.1, max = 360, value = 50, width = "100%")
            ),
            div(
              class = "move-map",
              uiOutput(ns("move"))
            )),
          numericInput(inputId = ns("upperLeftLatitude"),
                       label = "Set Latitude of upper left corner",
                       min = -90, max = 90, value = c(), width = "20%"),
          numericInput(inputId = ns("upperLeftLongitude"),
                       label = "Set Longitude of upper left corner",
                       min = -180, max = 180, value = c(), width = "20%"),
          numericInput(inputId = ns("zoomSet"),
                      label = "Zoom/x-Range in degrees Longitude (click set button for apply)",
                      min = 0.1, max = 360, value = 50, width = "20%"),
          actionButton( ns("set"), "Set"),
          div(
            div(
              style = 'display:inline-block',
              class = "save-plot-container",
              textInput(ns("saveMapName"), NULL, placeholder = "Name for Map"),
              actionButton(ns("saveMap"), "Save map")
            ),
            div(style = 'display:inline-block', plotExportButton(ns("export")))
          ),
          actionButton(ns("add_btn2D"), "Add data point"),
          actionButton(ns("rm_btn2D"), "Remove data point"),
          actionButton(ns("ok"), "Ok"),
          uiOutput(ns("pointInput2D"))
        )
      ),
        sidebarPanel(
          width = 2,
          radioButtons(inputId = ns("Centering"),
                       label = "Map Centering",
                       choices = c("0th meridian" = "Europe", "160th meridian" = "Pacific")),
          radioButtons(inputId = ns("estType"), label = "Estimation type", inline = TRUE,
                       choices = c("Mean", "1 SE", "2 SE", "Quantile", "Significance (p-value)", "Significance (z-value)"),
                       selected = "Mean"),
          conditionalPanel(
            ns = ns,
            condition = "input.estType == 'Quantile'",
            sliderInput(inputId = ns("Quantile"),
                        label = "Estimation quantile",
                        min = 0.01, max = 0.99, value = c(0.9), width = "100%")
          ),
          checkboxInput(inputId = ns("showModel"), label = "Show model estimates", value = T),
          numericInput(ns("rangezMin"), "Min value of range dependent variable", value = 0),
          numericInput(ns("rangezMax"), "Max value of range dependent variable", value = 10),
          radioButtons(inputId = ns("terrestrial"), label = "", inline = TRUE,
                       choices = list("Terrestrial " = 1, "All" = 3, "Aquatic" = -1),
                       selected = 1),
          checkboxInput(inputId = ns("grid"),
                        label = "Show map grid",
                        value = TRUE, width = "100%"),
          checkboxInput(inputId = ns("scale"),
                        label = "Show map scale",
                        value = TRUE, width = "100%"),
          checkboxInput(inputId = ns("arrow"),
                        label = "Show north arrow",
                        value = TRUE, width = "100%"),
          checkboxInput(inputId = ns("titleMain"),
                        label = "Show plot title",
                        value = TRUE),
          checkboxInput(inputId = ns("titleScale"),
                        label = "Show colour scale title",
                        value = TRUE),
          checkboxInput(inputId = ns("showScale"),
                        label = "Show colour scale",
                        value = TRUE),
          checkboxInput(inputId = ns("setAxisLabels"),
                        label = "Set axis labels",
                        value = FALSE),
          conditionalPanel(
            condition = "input.setAxisLabels == true",
            textInput(ns("mainLabel"), NULL, placeholder = "main title"),
            textInput(ns("yLabel"), NULL, placeholder = "y-axis"),
            textInput(ns("xLabel"), NULL, placeholder = "x-axis"),
            textInput(ns("scLabel"), NULL, placeholder = "colour scale title"), ns = ns
          ),
          checkboxInput(inputId = ns("setNorth"),
                        label = "Set north arrow and scale size and position",
                        value = FALSE),
          conditionalPanel(
            condition = "input.setNorth == true",
            sliderInput(ns("northSize"), "Size north arrow", min = 0, max = 1, value = 0.2),
            sliderInput(ns("scalSize"), "Size scale", min = 0, max = 1, value = 0.1),
            sliderInput(ns("scaleX"), "Scale x orientation", min = 0, max = 1, value = 0),
            sliderInput(ns("scaleY"), "Scale y orientation", min = 0, max = 1, value = 0.1),
            sliderInput(ns("NorthX"), "North arrow x orientation", min = 0, max = 1, value = 0.025),
            sliderInput(ns("NorthY"), "North arrow y orientation", min = 0, max = 1, value = 0.925),
            ns = ns
          ),
          selectInput(inputId = ns("Colours"), label = "Colour palette",
                      choices = list("Red-Yellow-Green" = "RdYlGn",
                                     "Yellow-Green-Blue" = "YlGnBu",
                                     "Purple-Orange" = "PuOr",
                                     "Pink-Yellow-Green" = "PiYG",
                                     "Red-Yellow-Blue" = "RdYlBu",
                                     "Yellow-Brown" = "YlOrBr",
                                     "Brown-Turquoise" = "BrBG"),
                      selected = "RdYlGn"),
          checkboxInput(inputId = ns("reverseCols"),
                        label = "Reverse colors",
                        value = FALSE, width = "100%"),
          checkboxInput(inputId = ns("smoothCols"),
                        label = "Smooth color transition",
                        value = FALSE, width = "100%"),
          sliderInput(inputId = ns("ncol"),
                      label = "Approximate number of colour levels",
                      min = 4, max = 50, value = 20, step = 2, width = "100%"),
          numericInput(inputId = ns("centerY"),
                       label = "Center point latitude",
                       min = -180, max = 180, value = c(), step = 0.5, width = "100%"),
          numericInput(inputId = ns("centerX"),
                       label = "Center point longitude",
                       min = -90, max = 90, value = c(), step = 0.5, width = "100%"),
          sliderInput(inputId = ns("Radius"),
                      label = "Radius (km)",
                      min = 10, max = 300, value = 100, step = 10, width = "100%"),
          sliderInput(inputId = ns("AxisSize"),
                      label = "Axis title font size",
                      min = 0.1, max = 3, value = 1, step = 0.1, width = "100%"),
          sliderInput(inputId = ns("AxisLSize"),
                      label = "Axis label font size",
                      min = 0.1, max = 3, value = 1, step = 0.1, width = "100%"),
          batchPointEstimatesUI(ns("batch"))
        )
      )
  )
}


#' server function of model Results module
#'
#' @param input input
#' @param output output
#' @param session session
#' @param savedMaps saved Maps
#' @param fruitsData data for export to FRUITS
#'
#' @export
mapDiff <- function(input, output, session, savedMaps, fruitsData){

  observeEvent(savedMaps(), {
    choices <- getMapChoices(savedMaps(), "difference")

    updateSelectInput(session, "savedModel", choices = choices)
  })

  observeEvent(savedMaps(), {
    choices <- getMapChoices(savedMaps(), c("localAvg", "temporalAvg", "spread", "difference", "similarity", "kernel2d", "kernel3d", "user"))

    updateSelectInput(session, "targetMap1", choices = choices)
    updateSelectInput(session, "targetMap2", choices = choices)
  })

  observeEvent(input$saveMap, {
    mapName <- trimws(input$saveMapName)
    if (mapName == ""){
      alert("Please provide a map name")
      return()
    }

    map <- createSavedMap(
      model = MapDiff(),
      predictions = values$predictions,
      plot = values$plot,
      type = "difference",
      name = mapName
    )

    maps <- savedMaps()
    maps[[length(maps) + 1]] <- map
    savedMaps(maps)

    alert(paste0("Map '", mapName, "' was saved"))
    updateTextInput(session, "saveMapName", value = "")
  })

  MapDiff <- reactiveVal(NULL)

  observeEvent(input$createDiffMap, {
    if (!is.null(input$targetMap1) & !is.null(input$targetMap2)) {
      withProgress(
        MapDiff(createDifferenceMap(savedMaps()[[as.numeric(input$targetMap1)]]$predictions,
                                    savedMaps()[[as.numeric(input$targetMap2)]]$predictions,
                                    operation = input$operation)),
        value = 0,
        message = "Generating map"
      )
    }
  })

  observeEvent(input$load, {
    MapDiff(savedMaps()[[as.numeric(input$savedModel)]]$model)
  })


  observe({
    validate(validInput(MapDiff()))
    if(input$fixCol == FALSE){
      val <- signif(max(MapDiff()$Sd, na.rm = TRUE), 2)
      updateSliderInput(session, "StdErr", value = signif(val * 5, 2),
                        min = 0, max = signif(val * 5, 2),
                        step = signif(roundUpNice(val, nice = c(1,10)) / 1000, 1))
      if(input$Centering == "Europe"){
        rangeLong <- diff(range(MapDiff()$Longitude, na.rm = TRUE) + c(-1, 1))

        updateSliderInput(session, "zoom",
                          value = pmin(360, pmax(0, rangeLong, na.rm = TRUE)))
      } else {
        longRange <- MapDiff()$Longitude
        longRange[MapDiff()$Longitude < -20] <- longRange[MapDiff()$Longitude < -20] + 200
        longRange[MapDiff()$Longitude >= -20] <- (- 160 + longRange[MapDiff()$Longitude >= -20])
        rangeLong <- diff(range(longRange, na.rm = TRUE) + c(-1, 1))
        updateSliderInput(session, "zoom",
                          value = pmin(360, pmax(0, rangeLong, na.rm = TRUE)))
      }
      values$up <- 0
      values$right <- 0
    }
  })

  output$move <- renderUI({
    moveButtons(ns = session$ns)
  })

  observeEvent(input$zoom, {
    zoom <- input$zoom
    values$zoom <- input$zoom
  })

  observeEvent(input$up, {
    if(values$set > 0){
      zoom <- values$zoom
    } else {
      zoom <- input$zoom
    }
    values$up <- values$up + zoom / 40
  })

  observeEvent(input$down, {
    if(values$set > 0){
      zoom <- values$zoom
    } else {
      zoom <- input$zoom
    }
    values$up <- values$up - zoom / 40
  })
  observeEvent(input$left, {
    if(values$set > 0){
      zoom <- values$zoom
    } else {
      zoom <- input$zoom
    }
    values$right <- values$right - zoom / 40
  })
  observeEvent(input$right, {
    if(values$set > 0){
      zoom <- values$zoom
    } else {
      zoom <- input$zoom
    }
    values$right <- values$right + zoom / 40
  })
  observeEvent(input$center, {
    values$up <- 0
    values$right <- 0
  })

  observeEvent(input$set, {
    values$set <- 1
    values$up <- 0
    values$right <- 0
    values$zoom <- input$zoomSet
    values$upperLeftLatitude <- input$upperLeftLatitude
    values$upperLeftLongitude <- input$upperLeftLongitude
  })
  ### Add Points
  pointDat2D <- reactiveVal({
    data.frame(
      index = numeric(0),
      y = numeric(0),
      x = numeric(0),
      label = character(0),
      pointSize = numeric(0),
      pointAlpha = numeric(0),
      pointColor = character(0)
    )
  })

  observeEvent(MapDiff(), ignoreNULL = FALSE, {
    pointDat2D(data.frame(
      index = numeric(0),
      y = numeric(0),
      x = numeric(0),
      label = character(0),
      pointSize = numeric(0),
      pointAlpha = numeric(0),
      pointColor = character(0)
    ))
  })

  addRow2D <- function(df) {
    rbind(df, data.frame(index = nrow(df) + 1, y = NA,
                         x = NA, label = "",
                         pointColor = "black", pointSize = 1,
                         pointAlpha = 0.5, stringsAsFactors = FALSE))
  }

  rmRow2D <- function(df) {
    if (nrow(df) > 0) df[- nrow(df), , drop = FALSE]
    else df
  }

  observeEvent(input$add_btn2D, {
    df <- pointDat2D()
    indices <- df$index
    lapply(indices, function(index) {
      yval <- input[[paste("y", index, sep = "_")]]
      xval <- input[[paste("x", index, sep = "_")]]
      labelVal <- input[[paste("label", index, sep = "_")]]
      pointColor <- input[[paste("pointColor", index, sep = "_")]]
      pointSize <- input[[paste("pointSize", index, sep = "_")]]
      pointAlpha <- input[[paste("pointAlpha", index, sep = "_")]]
      df[index, "pointColor"] <<-  if (is.null(pointColor)) "#000000" else pointColor
      df[index, "pointSize"] <<-   if (is.null(pointSize)) 1 else pointSize
      df[index, "pointAlpha"] <<-   if (is.null(pointAlpha)) 1 else pointAlpha
      df[index, "y"] <<- if (is.null(yval)) NA else yval
      df[index, "x"] <<- if (is.null(xval)) NA else xval
      df[index, "label"] <<- if (is.null(labelVal)) NA else labelVal
    })
    pointDat2D(df)
    pointDat2D(addRow2D(pointDat2D()))
  })

  observeEvent(input$rm_btn2D, {
    pointDat2D(rmRow2D(pointDat2D()))
  })

  inputGroup2D <- reactive({
    createPointInputGroup2D(pointDat2D(), ns = session$ns)
  })

  pointDatOK <- eventReactive(input$ok, ignoreNULL = FALSE, {
    df <- pointDat2D()
    indices <- df$index
    lapply(indices, function(index) {
      yval <- input[[paste("y", index, sep = "_")]]
      xval <- input[[paste("x", index, sep = "_")]]
      labelVal <- input[[paste("label", index, sep = "_")]]
      pointColor <- input[[paste("pointColor", index, sep = "_")]]
      pointSize <- input[[paste("pointSize", index, sep = "_")]]
      pointAlpha <- input[[paste("pointAlpha", index, sep = "_")]]
      df[index, "pointColor"] <<-  if (is.null(pointColor)) "#000000" else pointColor
      df[index, "pointSize"] <<-   if (is.null(pointSize)) 1 else pointSize
      df[index, "pointAlpha"] <<-   if (is.null(pointAlpha)) 1 else pointAlpha
      df[index, "y"] <<- if (is.null(yval)) NA else yval
      df[index, "x"] <<- if (is.null(xval)) NA else xval
      df[index, "label"] <<- if (is.null(labelVal)) NA else labelVal
    })
    pointDat2D(df)
    return(pointDat2D())
  })

  observe({
    validate(validInput(MapDiff()))
    if(input$fixCol == FALSE){
      zValues <- MapDiff()$Est
      minValue <- signif(min(zValues, na.rm = TRUE), which(round(abs(diff(zValues) / min(zValues, na.rm = TRUE) * 10^(0:10)), 0) > 1)[1])
      maxValue <- signif(max(zValues, na.rm = TRUE), which(round(abs(diff(zValues) / max(zValues, na.rm = TRUE) * 10^(0:10)), 0) > 1)[1])
      if(is.na(minValue)){
        minValue <- 0
      }
      if(is.na(maxValue)){
        maxValue <- 0
      }
      updateNumericInput(session, "rangezMin", value = minValue, min = minValue, max = maxValue)
      updateNumericInput(session, "rangezMax", value = maxValue, min = minValue, max = maxValue)
      if(input$estType %in% c("1 SE", "2 SE", "SE")){
        sdVal <- ifelse(grepl("2", input$estType), 2, 1)
        zValues <- MapDiff()$Sd
        maxValue <- signif(max(zValues, na.rm = TRUE) * sdVal, 2)
        updateNumericInput(session, "rangezMin", value = 0, min = 0, max = maxValue)
        updateNumericInput(session, "rangezMax", value = maxValue, min = 0, max = maxValue)
      }
    }
  })

  plotFun <-  reactive({
    validate(validInput(MapDiff()))
    pointDatOK = pointDatOK()

    if(input$fixCol == FALSE){
      if(values$set > 0){
        zoom <- values$zoom
      } else {
        zoom <- input$zoom
      }

      rangey <- - diff(range(MapDiff()$Latitude, na.rm = TRUE)) / 2 +
        max(MapDiff()$Latitude, na.rm = TRUE) + values$up
      if(!is.na(values$upperLeftLatitude) & values$set > 0){
        rangey <- values$upperLeftLatitude + c(- zoom / 2 , 0) + values$up
      } else {
        rangey <- rangey + c( - zoom / 4, zoom / 4)
      }
      if(input$Centering == "Europe"){
        rangex <- - diff(range(MapDiff()$Longitude, na.rm = TRUE)) / 2 +
          max(MapDiff()$Longitude, na.rm = TRUE) + values$right
        if(!is.na(values$upperLeftLongitude) & values$set > 0){
          rangex <- values$upperLeftLongitude + values$right
          rangex <- rangex + c(0, zoom)
        } else {
          rangex <- rangex + c( - zoom / 2, zoom / 2)
        }
      } else{
        dataPac <- MapDiff()[, c("Longitude", "Latitude")]
        dataPac$Longitude[MapDiff()$Longitude < -20] <- dataPac$Longitude[MapDiff()$Longitude < -20] + 200
        dataPac$Longitude[MapDiff()$Longitude >= -20] <- (- 160 + dataPac$Longitude[MapDiff()$Longitude >= -20])
        rangex <- - diff(range(dataPac$Longitude, na.rm = TRUE)) / 2 +
          max(dataPac$Longitude, na.rm = TRUE) + values$right
        if(!is.na(values$upperLeftLongitude) & values$set > 0){
          rangex <- values$upperLeftLongitude+ values$right
          if(rangex < -20) rangex <- rangex + 200
          if(rangex >= -20) rangex <- rangex - 160
          rangex <- rangex + c(0, zoom)
        } else {
          rangex <- rangex + c( - zoom / 2, zoom / 2)
        }
        }
      if(rangex[2] > 180){
        rangex <- c(180 - zoom, 180)
      }
      if(rangex[1] < -180){
        rangex <- c(-180, -180 + zoom)
      }
      if(rangey[2] > 90){
        coordDiff <- rangey[2] - 90
        rangey <- pmin(90, pmax(-90, rangey - coordDiff))
      }
      if(rangey[1] < -90){
        coordDiff <- rangey[1] + 90
        rangey <- pmin(90, pmax(-90, rangey - coordDiff))
      }
      values$rangex <- rangex
      values$rangey <- rangey
    }
    if(input$smoothCols){
      values$ncol <- 200
    } else {
      if(input$fixCol == FALSE){
        values$ncol <- input$ncol
      }
    }
    rangez <- c(input$rangezMin, input$rangezMax)
    if(input$estType == "Significance (p-value)"){
      rangez <- pmax(0, pmin(1, rangez))
    }

    function(...){
      plotDS(MapDiff(),
             estType = input$estType,
             estQuantile = input$Quantile,
             type = "difference", independent = "",
             rangex = values$rangex,
             rangey = values$rangey,
             rangez = rangez,
             colors = input$Colours,
             ncol = values$ncol,
             centerMap = input$Centering,
             reverseColors = input$reverseCols,
             terrestrial = input$terrestrial,
             grid = input$grid,
             arrow = input$arrow,
             scale = input$scale,
             centerX = input$centerX,
             centerY = input$centerY,
             Radius = input$Radius,
             titleMain = !input$titleMain,
             titleScale = !input$titleScale,
             showScale = input$showScale,
             setAxisLabels = input$setAxisLabels,
             mainLabel = input$mainLabel,
             yLabel =  input$yLabel,
             xLabel =  input$xLabel,
             scLabel =  input$scLabel,
             northSize = input$northSize,
             scalSize = input$scalSize,
             scaleX = input$scaleX,
             scaleY = input$scaleY,
             NorthX = input$NorthX,
             NorthY = input$NorthY,
             AxisSize = input$AxisSize,
             AxisLSize = input$AxisLSize,
             pointDat = pointDatOK,
             ...
      )
    }
  })

  output$DistMap <- renderPlot({
    validate(validInput(MapDiff()))
    res <- plotFun()()
    values$predictions <- res$XPred
    values$meanCenter <- res$meanCenter
    values$sdCenter <- res$sdCenter
    values$plot <- recordPlot()
  })

  values <- reactiveValues(plot = NULL, predictions = NULL, sdCenter = NA, meanCenter = NA,
                           set = 0,
                           upperLeftLongitude = NA,
                           upperLeftLatitude = NA,
                           zoom = 50)

  output$centerEstimate <- renderText({
    if (is.na(input$centerY) | is.na(input$centerX) | is.na(input$Radius)) return("")

    if (is.na(values$meanCenter) | is.na(values$sdCenter)) {
      return("Cannot compute mean and sd at your provided coordinates.
             Please raise the plot resolution or radius such that estimates within the radius are available.")
    }

    paste0("Mean: ", values$meanCenter,
           ", Standard error of the mean: ", values$sdCenter,
           "  at coordinates ",  "(",
           input$centerY, "\u00B0, " , input$centerX,
           "\u00B0) for a ", round(input$Radius, 3),
           " km radius")
  })


  output$pointInput2D <- renderUI(inputGroup2D())
  output$n2D <- reactive(nrow(pointDat2D()))
  outputOptions(output, "n2D", suspendWhenHidden = FALSE)

  callModule(plotExport, "export", reactive(values$plot), "similarity",
             reactive(values$predictions))
  callModule(batchPointEstimates, "batch", plotFun, fruitsData = fruitsData)

}
