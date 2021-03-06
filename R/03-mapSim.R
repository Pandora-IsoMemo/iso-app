
#' ui function of modelResultsDiffSim module
#'
#' @param id namespace
#' @param title title in tab
#'
#' @export
modelResultsSimUI <- function(id, title = ""){
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
                                "Saved map" = "model"),
                    selected = "db"),
        conditionalPanel(
          condition = "input.dataSource == 'model'",
        selectInput(ns("savedModel"),
                    "Select similarity Map",
                    choices = c(""),
                    selected = ""),
        actionButton( ns("load"), "Load"),
        ns = ns),
        conditionalPanel(
          condition = "input.dataSource == 'create'",
          pickerInput(
          inputId = ns("SimMapSelect"),
          label = "Select maps for similarity map:",
          choices = NULL,
          options = list(
            `actions-box` = FALSE,
            size = 10,
            `none-selected-text` = "No maps selected",
            `selected-text-format` = "count > 8"
          ),
          multiple = TRUE
        ),
        actionButton(ns("simDataImport"), "Provide data values"),
        tags$hr(),
        checkboxInput(ns("normalize"), "Normalize values"),
        conditionalPanel(
          condition = "input.normalize == true",
          radioButtons(ns("normalType"), "Type of normalisation",
                       choices = c("Max value equal to 1" = "1", "Volume equal to 1" = "2")),
          ns = ns),
        actionButton(ns("start"), "Create similarity map"),
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
                       choices = c("Mean", "1 SE", "2 SE", "Quantile"),
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
          checkboxInput(inputId = ns("showValues"),
                        label = "Show data values in plot",
                        value = TRUE, width = "100%"),
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
mapSim <- function(input, output, session, savedMaps, fruitsData){
  values <- reactiveValues(
    simDataList = list(),
    simDataTemp = list(),
    predictionList = list(),
    sdCenter = NA,
    meanCenter = NA,
    set = 0,
    upperLeftLongitude = NA,
    upperLeftLatitude = NA,
    zoom = 50
  )

  observeEvent(savedMaps(), {
    choices <- getMapChoices(savedMaps(), "similarity")

    updateSelectInput(session, "savedModel", choices = choices)
  })

  mapChoices <- reactive(
    getMapChoices(savedMaps(), c("localAvg", "temporalAvg"))
  )

  observe({
    updatePickerInput(session, "SimMapSelect", choices = mapChoices())
  })

  observeEvent(input$SimMapSelect, ignoreNULL = FALSE, {
    if (is.null(input$SimMapSelect) || length(input$SimMapSelect) == 0)
      shinyjs::disable("simDataImport")
    else
      shinyjs::enable("simDataImport")
  })

  observeEvent(input$simDataImport, {
    mapNames <- names(mapChoices())[match(input$SimMapSelect, mapChoices())]
    m <- listToDoubleMatrix(values$simDataList, mapNames)

    if (nrow(m) == 0) m <- rbind(m, NA)
    mode(m) <- "character"

    m[is.na(m)] <- ""

    showModal(modalDialog(
      title = "Data values",
      matrixInput(
        session$ns("simDataValues"),
        value = m,
        class = "numeric",
        cols = list(
          names = TRUE,
          createHeader = "MpiIsoApp.doubleHeader.create",
          updateHeader = "MpiIsoApp.doubleHeader.update",
          getHeader = "MpiIsoApp.doubleHeader.get"
        ),
        rows = list(extend = TRUE),
      ),
      footer = tagList(
        actionButton(session$ns("simDataImportCancel"), "Cancel"),
        tags$button("Submit", class = "btn btn-default", type = "button",
                    onClick = paste0("setTimeout(function(){Shiny.setInputValue('",
                                     session$ns("simDataImportSubmit"), "', Math.random())}, 300)"))
      )
    ))
  })

  observe({
    req(input$simDataValues)
    values$simDataTemp <- doubleMatrixToList(input$simDataValues)
  })

  observeEvent(input$SimMapSelect, {
    res <- lapply(input$SimMapSelect, function(m) {
      savedMaps()[[as.numeric(m)]]$predictions
    })
    values$predictionList <- res
    values$simDataList <- list()
    values$simDataListM <- list()
  })

  observeEvent(input$simDataImportCancel, removeModal())
  observeEvent(input$simDataImportSubmit, {
    values$simDataList <- values$simDataTemp
    removeModal()
  })

  Model <- reactiveVal(NULL)
  observeEvent(input$start, {
    values$simDataListM <- values$simDataList
    values$set <- 0

    if (length(values$simDataListM) == 0) return(NULL)
    if (any(is.na(do.call("rbind", values$simDataListM)[, 1]))) return(NULL)
    if (any(is.na(do.call("rbind", values$simDataListM)[, 2]))){
      withProgress(
        Model(createSimilarityMap(values$predictionList,
                                  values$simDataListM, includeUncertainty = FALSE,
                                  normalize = input$normalize,
                                  normalType = input$normalType)),
        value = 0,
        message = 'Creating similarity map ...'
      )
    } else {
      withProgress(
        Model(createSimilarityMap(values$predictionList,
                                  values$simDataListM,
                                  normalize = input$normalize,
                                  normalType = input$normalType)),
        value = 0,
        message = 'Creating similarity map ...'
      )
    }
  })

  observeEvent(input$load, {
    Model(savedMaps()[[as.numeric(input$savedModel)]]$model)
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

  observeEvent(Model(), ignoreNULL = FALSE, {
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


  plotFun <-  reactive({
    validate(validInput(Model()))
    pointDatOK = pointDatOK()

    if(input$fixCol == FALSE){
      if(values$set > 0){
        zoom <- values$zoom
      } else {
        zoom <- input$zoom
      }

      rangey <- - diff(range(Model()$Latitude, na.rm = TRUE)) / 2 +
        max(Model()$Latitude, na.rm = TRUE) + values$up
      if(!is.na(values$upperLeftLatitude) & values$set > 0){
        rangey <- values$upperLeftLatitude + c(- zoom / 2 , 0) + values$up
      } else {
        rangey <- rangey + c( - zoom / 4, zoom / 4)
      }
      if(input$Centering == "Europe"){
        rangex <- - diff(range(Model()$Longitude, na.rm = TRUE)) / 2 +
          max(Model()$Longitude, na.rm = TRUE) + values$right
        if(!is.na(values$upperLeftLongitude) & values$set > 0){
          rangex <- values$upperLeftLongitude + values$right
          rangex <- rangex + c(0, zoom)
        } else {
          rangex <- rangex + c( - zoom / 2, zoom / 2)
        }
      } else{
        dataPac <- Model()[, c("Longitude", "Latitude")]
        dataPac$Longitude[Model()$Longitude < -20] <- dataPac$Longitude[Model()$Longitude < -20] + 200
        dataPac$Longitude[Model()$Longitude >= -20] <- (- 160 + dataPac$Longitude[Model()$Longitude >= -20])
        rangex <- - diff(range(dataPac$Longitude, na.rm = TRUE)) / 2 +
          max(dataPac$Longitude, na.rm = TRUE) + values$right
        if(!is.na(values$upperLeftLongitude) & values$set > 0){
          rangex <- values$upperLeftLongitude + values$right
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

    function(...){
      plotDS(Model(),
             estType = input$estType,
             estQuantile = input$Quantile,
             type = "similarity", independent = "",
             rangex = values$rangex,
             rangey = values$rangey,
             rangez = c(input$rangezMin, input$rangezMax),
             colors = input$Colours,
             ncol = values$ncol,
             reverseColors = input$reverseCols,
             terrestrial = input$terrestrial,
             grid = input$grid,
             centerMap = input$Centering,
             arrow = input$arrow,
             scale = input$scale,
             centerX = input$centerX,
             centerY = input$centerY,
             Radius = input$Radius,
             simValues = values$simDataListM,
             showValues = input$showValues,
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

  observeEvent(input$saveMap, {
    mapName <- trimws(input$saveMapName)
    if (mapName == ""){
      alert("Please provide a map name")
      return()
    }

    map <- createSavedMap(
      model = Model(),
      predictions = values$predictions,
      plot = values$plot,
      type = "similarity",
      name = mapName
    )

    maps <- savedMaps()
    maps[[length(maps) + 1]] <- map
    savedMaps(maps)

    alert(paste0("Map '", mapName, "' was saved"))
    updateTextInput(session, "saveMapName", value = "")
  })



  output$DistMap <- renderPlot({
    validate(validInput(Model()))
    res <- plotFun()()
    if(class(res) == "character"){
      alert(res)
    } else {
    values$predictions <- res$XPred
    values$meanCenter <- res$meanCenter
    values$sdCenter <- res$sdCenter
    values$plot <- recordPlot()
    }
  })

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

  observe({
    validate(validInput(Model()))
    if(input$fixCol == FALSE){
      val <- signif(max(Model()$Sd, na.rm = TRUE), 2)
      updateSliderInput(session, "StdErr", value = signif(val * 8, 2),
                        min = 0, max = signif(val * 8, 2),
                        step = signif(roundUpNice(val, nice = c(1,10)) / 1000, 1))
      if(input$Centering == "Europe"){
        rangeLong <- diff(range(Model()$Longitude, na.rm = TRUE) + c(-1, 1))

        updateSliderInput(session, "zoom",
                          value = pmin(360, pmax(0, rangeLong, na.rm = TRUE)))
      } else {
        longRange <- Model()$Longitude
        longRange[Model()$Longitude < -20] <- longRange[Model()$Longitude < -20] + 200
        longRange[Model()$Longitude >= -20] <- (- 160 + longRange[Model()$Longitude >= -20])
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

  observe({
    validate(validInput(Model()))
    if(input$fixCol == FALSE){
      zValues <- Model()$Est
      minValue <- 0
      maxValue <- signif(max(zValues, na.rm = TRUE), 2)

      updateNumericInput(session, "rangezMin", value = minValue, min = minValue, max = maxValue)
      updateNumericInput(session, "rangezMax", value = maxValue, min = minValue, max = maxValue)
      if(input$estType %in% c("SE", "1 SE", "2 SE")){
        sdVal <- ifelse(grepl("2", input$estType), 2, 1)
        zValues <- Model()$Sd
        maxValue <- signif(max(zValues, na.rm = TRUE) * sdVal, 2)
        updateNumericInput(session, "rangezMin", value = 0, min = 0, max = maxValue)
        updateNumericInput(session, "rangezMax", value = maxValue, min = 0, max = maxValue)
      }
    }
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

  output$pointInput2D <- renderUI(inputGroup2D())
  output$n2D <- reactive(nrow(pointDat2D()))
  outputOptions(output, "n2D", suspendWhenHidden = FALSE)
  callModule(plotExport, "export", reactive(values$plot), "similarity",
             reactive(values$predictions))
  callModule(batchPointEstimates, "batch", plotFun, fruitsData = fruitsData)

}
