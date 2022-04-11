#' ui function of leaflet settings module
#'
#' @param id namespace
#' @param title title in tab
leafletSettingsUI <- function(id, title = "") {
  ns <- NS(id)

  tagList(
    tags$h2(title),
    selectInput(
      ns("LeafletType"),
      "Map type",
      choices = c(
        "Type 1" = "1",
        "Type 2" = "2",
        "Type 3" = "3",
        "Type 4" = "4",
        "Type 5" = "5",
        "Type 6" = "6",
        "Type 7" = "7"
      )
    ),
    fluidRow(column(6, checkboxInput(
      ns("includeScale"), "Scale"
    )),
    column(
      6,
      selectInput(
        ns("scalePosition"),
        label = NULL,
        choices = c("topright", "bottomright", "bottomleft", "topleft"),
        selected = "bottomright"
      )
    )),
    fluidRow(column(6, checkboxInput(
      ns("includeNorthArrow"), "North Arrow"
    )),
    column(
      6,
      selectInput(
        ns("northArrowPosition"),
        label = NULL,
        choices = c("topright", "bottomright", "bottomleft", "topleft"),
        selected = "bottomright"
      )
    )),
    fluidRow(column(6, checkboxInput(
      ns("includeLogo"), "Logo"
    )),
    column(
      6,
      selectInput(
        ns("logoPosition"),
        label = NULL,
        choices = c("topright", "bottomright", "bottomleft", "topleft"),
        selected = "topleft"
      )
    )),
    fluidRow(
      column(6,
             #tags$h4("Latitude"),
             numericInput(ns("centerLat"), "Latitude: Center", value = 50, min = -90, max = 90)
      ),
      column(6,
             #tags$h4("Longitude"),
             numericInput(ns("centerLng"), "Longitude: Center", value = 30, min = -180, max = 180)
      )
    ),
    sliderInput(ns("boundsLat"), "Latitude: South - North", value = c(15, 60), min = -90, max = 90),
    sliderInput(ns("boundsLng"), "Longitude: West - East", value = c(-15, 60), min = -180, max = 180)
    # alternative UI for lat/lng bounds:
    # fluidRow(
    #   column(6,
    #          numericInput(ns("boundNorth"), "Bound North",
    #                       value = 65, min = -90, max = 90)
    #   ),
    #   column(6,
    #          numericInput(ns("boundEast"), "Bound East",
    #                       value = 60, min = -180, max = 180)
    #   )
    # ),
    # fluidRow(
    #   column(6,
    #          numericInput(ns("boundSouth"), "Bound South",
    #                       value = 15, min = -90, max = 90)
    #   ),
    #   column(6,
    #          numericInput(ns("boundWest"), "Bound West",
    #                       value = -15, min = -180, max = 180)
    #   )
    # )
  )
}


#' server funtion of leaflet settings module
#'
#' @param input input
#' @param output output
#' @param session session
leafletSettings <- function(input, output, session) {
  values <- reactiveValues(pointRadius = 20000)

  observeEvent(input$map_zoom, {
    values$pointRadius <- (20000 * (4 / input$map_zoom) ^ 3)
  })

  observeEvent(input$LeafletType, {
    values$leafletType <- input$LeafletType
  })

  observe({
    values$scalePosition <-
      ifelse(input$includeScale, input$scalePosition, NA_character_)
  })

  observe({
    values$northArrowPosition <-
      ifelse(input$includeNorthArrow,
             input$northArrowPosition,
             NA_character_)
  })

  observe({
    values$logoPosition <-
      ifelse(input$includeLogo, input$logoPosition, NA_character_)
  })

  observe({
    values$center <-
      reactiveValues(lat = input$centerLat,
                     lng = input$centerLng)
  })

  observe({
    values$bounds <-
      reactiveValues(north = input$boundsLat[[2]],
                     south = input$boundsLat[[1]],
                     east = input$boundsLng[[2]],
                     west = input$boundsLng[[1]])
    # alternative output for lat/lng bounds:
    # reactiveValues(north = input$boundNorth,
    #                south = input$boundSouth,
    #                east = input$boundEast,
    #                west = input$boundWest)
  })

  reactive({values})
}
