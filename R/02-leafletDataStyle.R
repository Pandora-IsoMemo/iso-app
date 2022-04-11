#' ui function of leaflet settings module
#'
#' @param id namespace
#' @param title title in tab
leafletDataStyleUI <- function(id, title = "") {
  ns <- NS(id)

  tagList(
    tags$h2(title),
    selectInput(ns("groupingColumn"),
                "Column with Groups",
                choices = NULL),
    checkboxInput(ns("customizeMarkers"), "Customize Circles", value = FALSE),
    conditionalPanel(
      ns = ns,
      "input.customizeMarkers == true",
      # selectInput(
      #   ns("dataShapes"),
      #   "Fontawesome Icon(s)",
      #   choices = c(
      #     "circle",
      #     "diamond",
      #     "square",
      #     "leaf",
      #     "seedling",
      #     "tree",
      #     "water",
      #     "volcano",
      #     "mountain",
      #     "fire"
      #   ),
      #   multiple = TRUE,
      #   selected = "circle"
      # ),
      colourInput(ns("dataColour"), "Colour Palette", value = "red"),
      selectInput(ns("selectedGroup"), "Group", choices = NULL),
      sliderInput(
        ns("radiusKm"),
        "Radius in km",
        value = 20,
        min = 1,
        max = 100
      ),
      sliderInput(
        ns("opacity"),
        "Opacity",
        value = 0.2,
        min = 0,
        max = 1
      )
    )
  )
}


#' server funtion of leaflet settings module
#'
#' @param input input
#' @param output output
#' @param session session
#' @param isoData reactive isoData table
leafletDataStyle <- function(input, output, session, isoData) {
  values <- reactiveValues(
    customizeMarkers = NULL,
    groupingColumn = NULL,
    groups = NULL,
    opacity = NULL,
    radiusKm = NULL,
    colours = NULL
  )

  observeEvent(input$customizeMarkers, {
    values$customizeMarkers <- input$customizeMarkers
  })

  observe({
    req(isoData(), !is.null(isoData()$source))
    nonNumericColumns <-
      colnames(isoData())[!sapply(isoData(), is.numeric)]
    updateSelectInput(session,
                      "groupingColumn",
                      choices = nonNumericColumns,
                      selected = "source")
  })

  observeEvent(list(input$groupingColumn, isoData()), {
    req(isoData())
    values$groupingColumn <- input$groupingColumn
    values$groups <- isoData()[[input$groupingColumn]] %>%
      unique()

    values$opacity <-
      rep(input$opacity, length.out = length(values$groups))
    names(values$opacity) <- values$groups

    values$radiusKm <-
      rep(input$radiusKm, length.out = length(values$groups))
    names(values$radiusKm) <- values$groups

  })

  observe({
    req(values$groups)
    updateSelectInput(session, "selectedGroup", choices = values$groups)
  })


  observeEvent(c(input$dataColour, values$groups), {
    # colourPicker does not work correctly
    # start with some palette, selectInput for several palettes
    values$colours <-
      rep(input$dataColour, length.out = length(values$groups))
    names(values$colours) <- values$groups
  })

  observeEvent(input$selectedGroup, {
    updateSliderInput(session, "radiusKm", value = values$radiusKm[[input$selectedGroup]])
    updateSliderInput(session, "opacity", value = values$opacity[[input$selectedGroup]])
  })

  observeEvent(input$radiusKm, {
    values$radiusKm[[input$selectedGroup]] <- input$radiusKm
  })

  observeEvent(input$opacity, {
    values$opacity[[input$selectedGroup]] <- input$opacity
  })

  # observeEvent(c(input$dataShapes, values$groups), {
  #   values$shapes <- rep(input$dataShapes, length.out = length(values$groups)) %>%
  #     as.list()
  #   names(values$shapes) <- values$groups
  # })

  reactive({
    values
  })
}
