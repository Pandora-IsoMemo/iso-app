library(MpiIsoApp)

options(shiny.maxRequestSize = 200*1024^2)
options(scipen=999)

server <- function(input, output, session) {
#  savedMaps <- reactiveVal(readRDS("~/savedMaps.rds"))
  savedMaps <- reactiveVal(list())
  fruitsData <- reactiveVal(list(event = NULL, data = NULL))

  isoData <- dataExplorerServer("dataExplorer")
  callModule(interactiveMap, "interactivemap", isoData = isoData)
  callModule(modelResults2D, "model2D", isoData = isoData, savedMaps = savedMaps,
             fruitsData = fruitsData)
  callModule(modelResults3D, "model3D", isoData = isoData, savedMaps = savedMaps,
             fruitsData = fruitsData)
  callModule(modelResultsSpread, "spread", isoData = isoData, savedMaps = savedMaps,
             fruitsData = fruitsData)
  callModule(modelResults2DKernel, "model2DKernel", isoData = isoData, savedMaps = savedMaps,
             fruitsData = fruitsData)
  callModule(modelResults3DKernel, "model3DKernel", isoData = isoData, savedMaps = savedMaps,
             fruitsData = fruitsData)
  callModule(mapDiff, "difference", savedMaps = savedMaps,
             fruitsData = fruitsData)
  callModule(mapSim, "similarity", savedMaps = savedMaps,
             fruitsData = fruitsData)
  callModule(modelResultsAssign, "assign", isoData = isoData)

  callModule(savedMapsTab, "svmt", savedMaps = savedMaps)

  if (reSourcesInstalled()) {
    callModule(ReSources::fruitsTab, "fruits", isoMemoData = fruitsData)
    hideTab("tab", "fruits")
  }

  if (isOnlyDataSearchMode()) {
    hideTab("tab", "Modeling", session = session)
    appendTab(inputId = "tab",
              modelLinkUI("modelLink", title = "Modeling")
    )
  } else {
    appendTab(inputId = "tab",
              savedMapsTabUI("svmt", "Saved/Create maps")
    )
  }

  observeEvent(input$getHelp, {
    showModal(modalDialog(
      title = "Help",
      easyClose = TRUE,
      getHelp(input$tab)
    ))
  })

  observe({
    query <- session$clientData$url_search
    params <- shiny::parseQueryString(query)

    if (!is.null(params$skin) && params$skin %in% allowedSkins()) {
      setSkin(params$skin)
      updateRadioButtons(session, "skin", selected = params$skin)
    }
  })

  observeEvent(input$skin, {
    setSkin(input$skin)
  })
}
