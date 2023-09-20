
#' ui function of modelResults2DKernel module
#'
#' @param id namespace
#' @param title title in tab
#' @param asFruitsTab show as part of the FRUITS app
#'
#' @export
modelResults2DKernelUI <- function(id, title = "", asFruitsTab = FALSE){
  ns <- NS(id)
  tabPanel(
    title,
    id = id,
    value = id,
    fluidRow(
      class = "modeling-content",
      # left sidebar ----
      sidebarPanel(
        width = 2,
        style = "position:fixed; width:14%; max-width:220px; overflow-y:auto; height:88%",
        downUploadButtonUI(ns("downUpload"), title = "Load a Model", label = "Upload / Download"),
        textAreaInput(ns("modelNotes"), label = NULL, placeholder = "Description ..."),
        tags$hr(),
        selectInput(ns("dataSource"),
                    "Data source",
                    choices = if (!asFruitsTab) c("Database" = "db", "Upload file" = "file", "Saved map" = "model") else c("Database" = "db"),
                    selected = "db"),
        conditionalPanel(
          condition = "input.dataSource == 'file'",
          importDataUI(ns("importData"), "Import Data"),
          tags$br(),
          tags$br(),
          radioButtons(
            inputId = ns("CoordType"),
            label = "Coordinate format",
            choiceNames = c("decimal degrees \n (e.g. \"40.446\" or \"79.982\")",
                            "degrees decimal minutes \n (e.g. \"40\u00B0 26.767\u2032 N\" or \"79\u00B0 58.933 W\")",
                            "degrees minutes seconds \n (e.g. \"40\u00B0 26\u2032 46\u2033 N\" or \"79\u00B0 58\u2032 56\u2033 W\")"),
            choiceValues = c("decimal degrees", "degrees decimal minutes", "degrees minutes seconds")
            ),
          tags$hr(),
          ns = ns
        ),
        conditionalPanel(
          condition = "input.dataSource == 'model'",
          ns = ns,
          selectInput(
            ns("savedModel"),
            "Saved Map",
            choices = NULL,
            selected = NULL
          )
        ),
        conditionalPanel(
          condition = "input.dataSource != 'model'",
          ns = ns,
          selectInput(inputId = ns("Longitude"),
                      label = "Longitude variable:",
                      choices = c("Longitude")),
          selectInput(inputId = ns("Latitude"),
                      label = "Latitude variable:",
                      choices = c("Latitude")),
          selectInput(inputId = ns("IndependentX"),
                      label = "Presence/Absence variable (optional):",
                      choices = c("")),
          selectInput(inputId = ns("Weighting"),
                      label = "Weighting variable (optional):",
                      choices = c("")),
          selectizeInput(inputId = ns("clusterMethod"),
                      label = "Cluster Method (optional):",
                      choices = c("kmeans","mclust"),
                      options = list(
                        placeholder = '',
                        onInitialize = I('function() { this.setValue(""); }')
                      )),
          conditionalPanel(
            condition = "input.clusterMethod == 'kmeans'",
            ns = ns,
            selectInput(inputId = ns("kMeansAlgo"),
                        label = "K-means algorithm:",
                        choices = c("Hartigan-Wong", "Lloyd", "Forgy",
                                    "MacQueen")),
            sliderInput(inputId = ns("nClust"),
                        label = "Number of clusters",
                        value = 5, min = 2, max = 15, step = 1)
          ),
          conditionalPanel(
            condition = "input.clusterMethod == 'mclust'",
            ns = ns,
            sliderInput(inputId = ns("nClustRange"),
                        label = "Possible range for clusters",
                        value = c(2,10), min = 2, max = 50, step = 1)
          ),
          checkboxInput(inputId = ns("modelArea"),
                        label = "Restrict model area",
                        value = FALSE, width = "100%"),
          sliderInput(inputId = ns("nSim"), label = "Number of simulations (for ci/se prediction)",
          min = 5, max = 100, step = 1, value = 10),
          conditionalPanel(
            condition = "input.modelArea == true",
            tags$strong("Latitude restriction:"),
            numericInput(inputId = ns("mALat1"),
                         label = "Lower",
                         min = -90, max = 90, value = c(-90), width = "80%"),
            numericInput(inputId = ns("mALat2"),
                         label = "Upper",
                         min = -90, max = 90, value = c(90), width = "80%"),
            tags$strong("Longitude restriction:"),
            numericInput(inputId = ns("mALong1"),
                         label = "Lower",
                         min = -180, max = 180, value = c(-180), width = "80%"),
            numericInput(inputId = ns("mALong2"),
                         label = "Upper",
                         min = -180, max = 180, value = c(180), width = "80%"),
            ns = ns
          )
        ),
        radioButtons(ns("kdeType"), "Bandwidth matrix type",
                     choices = c("Correlated" = "1", "Diagonal" = "2", "Diagonal + equal in longitudes and latitudes" = "3")),
        actionButton( ns("start"), "Start"),
        conditionalPanel(
          condition = conditionPlot(ns("DistMap")),
          checkboxInput(inputId = ns("fixCol"),
                        label = "Fix colours and ranges ",
                        value = FALSE, width = "100%")
        ),
        conditionalPanel(
          ns = ns,
          condition = "input.dataSource != 'model'",
          tags$hr(),
          tags$h4("Batch Modeling"),
          batchModelingUI(ns("batchModeling"))
        )
      ),
      # main panel ----
      mainPanel(
        width = 8,
        div(class = "aspect-16-9", div(
          plotOutput(outputId = ns("DistMap"), width = "100%", height = "100%")
        )),
        conditionalPanel(
          condition = conditionPlot(ns("DistMap")),
          htmlOutput(ns("centerEstimate"), container = function(...) div(..., style = "text-align:center;")),
          tags$br(),
          tags$br(),
          fluidRow(column(width = 3,
                          div(
                            class = "move-map",
                            uiOutput(ns("move"))
                          )
          ),
          column(width = 3,
                 offset = 6,
                 align = "right",
                 plotExportButton(ns("export"))
          )),
          tags$hr(),
          tags$h4("Map Section"),
          mapSectionUI(ns("mapSection")),
          fluidRow(
            column(
              width = 3,
              offset = 9,
              style = "margin-top: -60px;",
              align = "right",
              actionButton(ns("set"), "Set Map Section")
            )
          ),
          tags$hr(),
          div(
            if (!asFruitsTab) div(
              style = 'display:inline-block',
              class = "save-plot-container",
              textInput(ns("saveMapName"), NULL, placeholder = "Name for Map"),
              actionButton(ns("saveMap"), "Save map")
            ) else NULL,
            dataExportButton(ns("exportData"))
          ),
          actionButton(ns("add_btn2D"), "Add data point"),
          actionButton(ns("rm_btn2D"), "Remove data point"),
          actionButton(ns("ok"), "Ok"),
          uiOutput(ns("pointInput2D"))
        )
      ),
      # right sidebar ----
        sidebarPanel(
          width = 2,
          style = "position:fixed; width:14%; max-width:220px; overflow-y:auto; height:88%",
          radioButtons(inputId = ns("Centering"),
                       label = "Map Centering",
                       choices = c("0th meridian" = "Europe", "160th meridian" = "Pacific")),
          zScaleUI(ns("zScale")),
          radioButtons(inputId = ns("terrestrial"), label = "", inline = TRUE,
                       choices = list("Terrestrial " = 1, "All" = 3, "Aquatic" = -1),
                       selected = 1),
          checkboxInput(inputId = ns("points"),
                        label = "Show locations on map",
                        value = TRUE, width = "100%"),
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
          conditionalPanel(
            condition = "input.points == true",
            sliderInput(inputId = ns("pointSize"),
                        label = "Location mark size",
                        min = 0.1, max = 2, value = 1, width = "100%",
                        step = 0.1), ns = ns),
          checkboxInput(inputId = ns("colPSettings"),
                        label = "Set colour and shape of location marks",
                        value = FALSE, width = "100%"),
          conditionalPanel(
            condition = "input.colPSettings == true",
            radioButtons(inputId = ns("colFix"),
                         label = "Fixed or variable location mark colour?", choices = c("fixed", "variable")),
            conditionalPanel(
              condition = "input.colFix == 'fixed'",
              colourInput(inputId = ns("pointCol"),
                          label = "Colour of location marks",
                          value = "#2C2161"), ns = ns),
            conditionalPanel(
              condition = "input.colFix == 'variable'",
              selectInput(inputId = ns("pointLabelsVarCol"),
                          label = "Select point color variable",
                          choices = c("")),
              selectInput(inputId = ns("colorsP"), label = "Colour palette for points",
                          choices = list("Red-Yellow-Green" = "RdYlGn",
                                         "Yellow-Green-Blue" = "YlGnBu",
                                         "Purple-Orange" = "PuOr",
                                         "Pink-Yellow-Green" = "PiYG",
                                         "Red-Yellow-Blue" = "RdYlBu",
                                         "Yellow-Brown" = "YlOrBr",
                                         "Brown-Turquoise" = "BrBG"),
                          selected = "RdYlGn"),
              ns = ns),
            selectInput(inputId = ns("pointShape"), label = "Shape of location marks",
                        choices = pchChoices(), selected = 4),
            ns = ns),
          checkboxInput(inputId = ns("cluster"),
                        label = "Show Clustering",
                        value = FALSE, width = "100%"),
          conditionalPanel(
            condition = "input.cluster == true",
            selectInput(inputId = ns("clusterCol"), label = "Colour palette for points",
                        choices = list("Set1" = "Set1",
                                       "Set2" = "Set2",
                                       "Set3" = "Set3",
                                       "Pastel1" = "Pastel1",
                                       "Pastel2" = "Pastel2",
                                       "Paired" = "Paired",
                                       "Dark2" = "Dark2",
                                       "Accent" = "Accent"),
                        selected = "Set1"),
            ns = ns),
          checkboxInput(inputId = ns("interior"),
                        label = "Apply convex hull",
                        value = FALSE, width = "100%"),

          checkboxInput(inputId = ns("mask"),
                        label = "Mask / Show output within range of points",
                        value = FALSE, width = "100%"),
          conditionalPanel(
            condition = "input.mask == true",
            sliderInput(inputId = ns("maskRadius"),
                        label = "Mask radius in km",
                        min = 10, max = 2500, value = 500, width = "100%",
                        step = 10), ns = ns),
          selectInput(inputId = ns("Colours"), label = "Colour palette",
                      choices = list("Yellow-Red" = "YlOrRd",
                                     "Purple-Red" = "PuRd",
                                     "Red" = "Reds",
                                     "Purple" = "Purples",
                                     "Orange" = "Oranges",
                                     "Grey" = "Greys",
                                     "Blue" = "Blues",
                                     "Green" = "Greens",
                                     "Yellow-Green" = "YlGn",
                                     "Red-Purple" = "RdPu",
                                     "Orange-Red" = "OrRd",
                                     "Green-Blue" = "GnBu",
                                     "Blue-Green" = "BuGn",
                                     "Purple-Blue" = "PuBu"),
                      selected = "RdYlGn"),
          checkboxInput(inputId = ns("reverseCols"),
                        label = "Reverse colors",
                        value = FALSE, width = "100%"),
          sliderInput(inputId = ns("ncol"),
                      label = "Approximate number of colour levels",
                      min = 4, max = 50, value = 50, step = 2, width = "100%"),
          checkboxInput(inputId = ns("smoothCols"),
                        label = "Smooth color transition",
                        value = FALSE, width = "100%"),
          sliderInput(inputId = ns("resolution"),
                      label = "Plot resolution (px)",
                      min = 20, max = 500, value = 100, width = "100%",
                      step = 20),
          checkboxInput(inputId = ns("pointLabels"),
                        label = "Scale point size by variable",
                        value = FALSE, width = "100%"),
          conditionalPanel(
            condition = "input.pointLabels == true",
            selectInput(inputId = ns("pointLabelsVar"),
                        label = "Select point size variable",
                        choices = c("")), ns = ns
          ),
          checkboxInput(inputId = ns("textLabels"),
                        label = "Add location mark text labels",
                        value = FALSE, width = "100%"),
          conditionalPanel(
            condition = "input.textLabels == true",
            selectInput(inputId = ns("textLabelsVar"),
                        label = "Select text label variable",
                        choices = c("")),
            sliderInput(inputId = ns("fontSize"),
                        label = "Font size)",
                        min = 0.1, max = 3, value = 1, step = 0.1, width = "100%"),
            selectInput(inputId = ns("fontType"),
                        label = "Select font type",
                        choices = names(pdfFonts())),
            colourInput(inputId = ns("fontCol"),
                        label = "Colour of font",
                        value = "#2C2161"), ns = ns),
          centerEstimateUI(ns("centerEstimateParams")),
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
#' @param isoData data
#' @param savedMaps saved Maps
#' @param fruitsData data for export to FRUITS
#'
#' @export
modelResults2DKernel <- function(input, output, session, isoData, savedMaps, fruitsData){
  observeEvent(savedMaps(), {
    choices <- getMapChoices(savedMaps(), "kernel2d")

    updateSelectInput(session, "savedModel", choices = choices)
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
      type = "kernel2d",
      name = mapName
    )
    maps <- savedMaps()
    maps[[length(maps) + 1]] <- map
    savedMaps(maps)

    alert(paste0("Map '", mapName, "' was saved"))
    updateTextInput(session, "saveMapName", value = "")
  })

  data <- reactiveVal()
  observe({
    activeData <- switch(
      input$dataSource,
      db = isoData(),
      file = fileImport()
    )

    # reset model
    Model(NULL)
    data(activeData)
  })

  coordType <- reactive({
    # reset model
    Model(NULL)

    switch(
      input$dataSource,
      db = "decimal degrees",
      file = input$CoordType
    )
  })

  Model <- reactiveVal(NULL)

  # MODEL DOWN- / UPLOAD ----
  uploadedData <- downUploadButtonServer(
    "downUpload",
    dat = data,
    inputs = input,
    model = reactive(list(currentModel = Model(), savedMaps = savedMaps())),
    rPackageName = "MpiIsoApp",
    githubRepo = "iso-app",
    subFolder = "KernelR",
    helpHTML = getHelp(id = "model2DKernel"),
    modelNotes = reactive(input$modelNotes),
    compressionLevel = 1)

  observe(priority = 100, {
    # reset model
    Model(NULL)
    ## update data ----
    data(uploadedData$data)
  }) %>%
    bindEvent(uploadedData$data)

  observe(priority = 50, {
    ## reset input of model notes
    updateTextAreaInput(session, "modelNotes", value = "")

    ## update inputs ----
    inputIDs <- names(uploadedData$inputs)
    inputIDs <- inputIDs[inputIDs %in% names(input)]

    for (i in 1:length(inputIDs)) {
      session$sendInputMessage(inputIDs[i],  list(value = uploadedData$inputs[[inputIDs[i]]]) )
    }
  }) %>%
    bindEvent(uploadedData$inputs)

  observe(priority = 10, {
    ## update model ----
    Model(unpackModel(uploadedData$model))

    uploadedSavedMaps <- unpackSavedMaps(uploadedData$model, currentSavedMaps = savedMaps())
    savedMaps(c(savedMaps(), uploadedSavedMaps))
  }) %>%
    bindEvent(uploadedData$model)

  # RUN MODEL ----
  observeEvent(input$start, {
    if (input$dataSource == "model") {
      if (length(savedMaps()) == 0) return(NULL)

      Model(savedMaps()[[as.numeric(input$savedModel)]]$model)
      return()
    }
    if (input$Latitude == "" | input$Longitude == "") {
      Model(NULL)
      return()
    }

    if(input$modelArea){
      restriction <- c(input$mALat1, input$mALat2, input$mALong1, input$mALong2)
      restriction[is.na(restriction)] <- c(-90, 90, -180, 180)[is.na(restriction)]
    } else {
      restriction <- c(-90, 90, -180, 180)
    }

    data <- data()

    model <- withProgress(
      estimateMapKernel(data = data, independent = input$IndependentX,
                  Longitude = input$Longitude, Latitude = input$Latitude,
                  CoordType = coordType(),
                  Weighting = input$Weighting,
                  clusterMethod = input$clusterMethod,
                  nClust = input$nClust,
                  nClustRange = input$nClustRange,
                  kMeansAlgo = input$kMeansAlgo,
                  restriction = restriction,
                  nSim = input$nSim,
                  kdeType = input$kdeType),
      value = 0,
      message = "Generating local kernel density model"
    )
    Model(model)
  })

  zoomFromModel <- reactiveVal(50)

  observe({
    validate(validInput(Model()))
    if(input$fixCol == FALSE){
      newZoom <- extractZoomFromLongRange(
        rangeLongitude = range(Model()$data$Longitude, na.rm = TRUE),
        mapCentering = input$Centering
      )

      isolate({
        zoomFromModel(newZoom)
        values$zoom <- newZoom
        values$up <- 0
        values$right <- 0
      })
    }
  })

  output$move <- renderUI({
    moveButtons(ns = session$ns)
  })

  zSettings <- zScaleServer("zScale",
                            Model = Model,
                            fixCol = reactive(input$fixCol),
                            estimationTypeChoices =
                              reactive(c("Mean", "1 SE", "2 SE", "Quantile")),
                            restrictOption = reactive("hide"),
                            zValuesFun = getZValuesKernel,
                            zValuesFactor = 1.25
                            )

  mapSettings <- mapSectionServer("mapSection", zoomValue = zoomFromModel)

  observeEvent(input$up, {
    values$up <- values$up + values$zoom / 40
  })

  observeEvent(input$down, {
    values$up <- values$up - values$zoom / 40
  })
  observeEvent(input$left, {
    values$right <- values$right - values$zoom / 40
  })
  observeEvent(input$right, {
    values$right <- values$right + values$zoom / 40
  })
  observeEvent(input$center, {
    values$upperLeftLatitude <- NA
    values$upperLeftLongitude <- NA
    values$up <- 0
    values$right <- 0
  })
  observeEvent(input$set, {
    values$zoom <- mapSettings$zoom
    values$upperLeftLatitude <- mapSettings$upperLeftLatitude
    values$upperLeftLongitude <- mapSettings$upperLeftLongitude
    values$up <- 0
    values$right <- 0
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

  centerEstimate <- centerEstimateServer("centerEstimateParams",
                                         predictions = reactive(values$predictions))

  plotFun <- reactive({
    function (model, ...) {
      pointDatOK = pointDatOK()

      if(input$fixCol == FALSE){
        zoom <- values$zoom

        rangey <- - diff(range(Model()$data$Latitude, na.rm = TRUE)) / 2 +
          max(Model()$data$Latitude, na.rm = TRUE) + values$up
        if(!is.na(values$upperLeftLatitude)){
          rangey <- values$upperLeftLatitude + c(- zoom / 2 , 0) + values$up
        } else {
          rangey <- rangey + c( - zoom / 4, zoom / 4)
        }
        if(input$Centering == "Europe"){
          rangex <- - diff(range(Model()$data$Longitude, na.rm = TRUE)) / 2 +
            max(Model()$data$Longitude, na.rm = TRUE) + values$right
          if(!is.na(values$upperLeftLongitude)){
            rangex <- values$upperLeftLongitude + values$right
            rangex <- rangex + c(0, zoom)
          } else {
            rangex <- rangex + c( - zoom / 2, zoom / 2)
          }
        } else{
          dataPac <- Model()$data
          dataPac$Longitude[Model()$data$Longitude < -20] <- dataPac$Longitude[Model()$data$Longitude < -20] + 200
          dataPac$Longitude[Model()$data$Longitude >= -20] <- (- 160 + dataPac$Longitude[Model()$data$Longitude >= -20])
          rangex <- - diff(range(dataPac$Longitude, na.rm = TRUE)) / 2 +
            max(dataPac$Longitude, na.rm = TRUE) + values$right
          if(!is.na(values$upperLeftLongitude)){
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
      textLabels <- NULL
      if(input$textLabels & !is.null(input$textLabelsVar) & input$textLabelsVar != ""){
        textLabels <- (data())[, input$textLabelsVar, drop = FALSE]
        if(nrow(textLabels) == 0){
          textLabels = NULL
        }
      }

      pointLabels <- NULL
      if(input$pointLabels & !is.null(input$pointLabelsVar) & input$pointLabelsVar != ""){
        pointLabels <- (data())[, input$pointLabelsVar, drop = FALSE]
        if(nrow(pointLabels) == 0){
          pointLabels = NULL
        }
      }
      pointColLabels <- NULL
      if(input$colFix == "variable" & !is.null(input$pointLabelsVarCol) & input$pointLabelsVarCol != ""){
        pointColLabels <- (data())[, input$pointLabelsVarCol, drop = FALSE]
        if(nrow(pointColLabels) == 0){
          pointColLabels = NULL
        }
      }

      req(zSettings$estType)

      # PLOT MAP ----
      plotMap(
        model,
        points = input$points,
        pointSize = input$pointSize,
        rangex = values$rangex,
        rangey = values$rangey,
        estType = zSettings$estType,
        showModel = zSettings$showModel,
        rangez = zSettings$range,
        limitz = zSettings$limit,
        resolution = input$resolution,
        interior = input$interior,
        mask = input$mask,
        maskRadius = input$maskRadius,
        ncol = values$ncol,
        pColor = input$pointCol,
        pointShape = as.numeric(input$pointShape),
        textLabels = textLabels,
        pointLabels = pointLabels,
        pointColLabels = pointColLabels,
        colorsP = input$colorsP,
        fontSize = input$fontSize,
        fontType = input$fontType,
        fontCol = input$fontCol,
        centerMap = input$Centering,
        terrestrial = input$terrestrial,
        colors = input$Colours,
        reverseColors = input$reverseCols,
        arrow = input$arrow,
        grid = input$grid,
        scale = input$scale,
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
        cluster = input$cluster,
        clusterCol = input$clusterCol,
        pointDat = pointDatOK,
        ...
      )
    }
  })


  output$DistMap <- renderPlot({
    validate(validInput(Model()))
    withProgress({
      res <- plotFun()(Model())
    }, min = 0, max = 1, value = 0.8, message = "Plotting map ...")
    values$predictions <- res$XPred
    values$plot <- recordPlot()
  })

  values <- reactiveValues(
    plot = NULL,
    predictions = NULL,
    up = 0,
    right = 0,
    set = 0,
    upperLeftLongitude = NA,
    upperLeftLatitude = NA,
    zoom = 50
  )

  output$centerEstimate <- renderUI({
    centerEstimate$text()
  })

  observe(priority = 75, {
    numVars <- unlist(lapply(names(data()), function(x){
      if (
        (is.integer(data()[[x]]) | is.numeric(data()[[x]]) | sum(!is.na(as.numeric((data()[[x]])))) > 2) #&
        #!(x %in% c("Latitude", "Longitude"))
      )
        x
      else
        NULL
    }))
    selectedTextLabel <- NULL

    selectedIndependent <- NULL
    if (input$dataSource == "db" & ("mean" %in% names(data()))){
      selectedIndependent <- "mean"
    }

    selectedIndependentUnc <- NULL
    if (input$dataSource == "db" & ("sd" %in% names(data()))){
      selectedIndependentUnc <- "sd"
    }

    selectedLongitude <- NULL
    if (input$dataSource == "db" & ("longitude" %in% names(data()))){
      selectedLongitude <- "longitude"
    }
    selectedLatitude <- NULL
    if (input$dataSource == "db" & ("latitude" %in% names(data()))){
      selectedLatitude <- "latitude"
    }

    updateSelectInput(session, "IndependentX",  choices = c("", numVars),
                      selected = selectedIndependent)
    updateSelectInput(session, "Weighting", choices = c("", numVars))
    updateSelectInput(session, "Longitude", choices = c("", names(data())),
                      selected = selectedLongitude)
    updateSelectInput(session, "Latitude", choices = c("", names(data())),
                      selected = selectedLatitude)
    updateSelectInput(session, "textLabelsVar", choices = c("", names(data())),
                      selected = selectedTextLabel)
    updateSelectInput(session, "pointLabelsVar", choices = c("", names(data())),
                      selected = selectedTextLabel)
    updateSelectInput(session, "pointLabelsVarCol", choices = c("", names(data())),
                      selected = selectedTextLabel)
  }) %>%
    bindEvent(data())

  ## Import Data ----
  importedDat <- importDataServer("importData")

  fileImport <- reactiveVal(NULL)
  observe({
    # reset model
    Model(NULL)
    if (length(importedDat()) == 0 ||  is.null(importedDat()[[1]])) fileImport(NULL)

    req(length(importedDat()) > 0, !is.null(importedDat()[[1]]))
    data <- importedDat()[[1]]
    valid <- validateImport(data, showModal = TRUE)

    if (!valid){
      showNotification("Import is not valid.")
      fileImport(NULL)
    } else {
      fileImport(data)
    }
  }) %>% bindEvent(importedDat())

  dataFun <- reactive({
    req(Model())
    function() {
      if(!is.null(Model()$data$spatial_cluster)){
        allData <- data()
        allData$rNames <- rownames(allData)
        modelData <- Model()$data
        modelData$rNames <- rownames(modelData)
        modelData <- merge(modelData[, c("spatial_cluster", "long_centroid_spatial_cluster", "lat_centroid_spatial_cluster", "rNames")], allData, all.y = FALSE, sort = FALSE)
        modelData$rNames <- NULL
        return(modelData)
      } else {
        allData <- data()
        allData$rNames <- rownames(allData)
        modelData <- Model()$data
        modelData$rNames <- rownames(modelData)
        modelData <- merge(modelData[, c("rNames")], allData, all.y = FALSE, sort = FALSE)
        modelData$rNames <- NULL
        return(modelData)
      }
    }
  })

  output$pointInput2D <- renderUI(inputGroup2D())
  output$n2D <- reactive(nrow(pointDat2D()))
  outputOptions(output, "n2D", suspendWhenHidden = FALSE)

  callModule(dataExport, "exportData", data = dataFun, filename = "modelData")
  callModule(plotExport, "export", reactive(values$plot), "local-average",
             reactive(values$predictions))
  callModule(batchPointEstimates, "batch", plotFun, fruitsData = fruitsData, Model = Model)
  batchModel <- callModule(batchModeling, "batchModeling", data = data, plotFun = plotFun,
             modelParams = reactive(reactiveValuesToList(input)), type = "kernel2d",
             savedMaps = savedMaps, estimateWrapper = estimateMapKernelWrapper,
             variableNames = "Presence/Absence Variables")

  observeEvent(batchModel(), {
    Model(batchModel())
  })

}
