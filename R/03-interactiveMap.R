#' ui function of interactiveMap module
#'
#' @param id namespace
#' @param title title in tab
#'
#' @export
interactiveMapUI <- function(id, title = ""){
  ns <- NS(id)

  tabPanel(
    title,
    id = id,
    value = id,
    div(
      class = "map-container",
      leafletOutput(ns("map"), width = "100%", height = "100%")
    ),
    conditionalPanel(
      condition = "$('#interactivemap-map').css('visibility') != 'hidden'",
      absolutePanel(
        tags$script(paste0(
          '
            $(document).on("shiny:visualchange", function(e) {
              let box = document.querySelector("#', ns('map'),'");
              let width = box.offsetWidth;
              let height = box.offsetHeight;
              Shiny.setInputValue("', ns('map_width'), '", width);
              Shiny.setInputValue("', ns('map_height'), '", height);
            });
            $(window).resize(function(e) {
              let box = document.querySelector("#', ns('map'),'");
              let width = box.offsetWidth;
              let height = box.offsetHeight;
              Shiny.setInputValue("', ns('map_width'), '", width);
              Shiny.setInputValue("', ns('map_height'), '", height);
            });
          '
        )),
        id = "controls", class = "panel panel-default", fixed = TRUE,
        draggable = TRUE, top = 110, left = "auto", right = 40, bottom = "auto",
        width = 330, height = "auto",
        h2("Statistics"),
        selectizeInput(ns("var1"), "Variable 1", choices = character(0),
                       options = list(allowEmptyOption = TRUE)),
        selectizeInput(ns("var2"), "Variable 2", choices = character(0),
                    options = list(allowEmptyOption = TRUE)),
        div(
          id = "stats-sidebar-container",
          sidebarPlotOutput(
            ns("plot1"),
            condition = paste0("input['", ns("var1"), "'] != ''")
          ),
          sidebarPlotOutput(
            ns("plot2"),
            condition = paste0("input['", ns("var2"), "'] != ''")
          ),
          sidebarPlotOutput(
            ns("plot3"),
            condition = paste0(
              "input['", ns("var1"), "'] != '' && input['", ns("var2"), "'] != ''"
            )
          )
        )
      )
    ),
    conditionalPanel(
      condition = "$('#interactivemap-map').css('visibility') != 'hidden'",
      absolutePanel(
        id = "controls", class = "panel panel-default", fixed = TRUE,
        draggable = TRUE, top = "auto", right = "auto", left = 40, bottom = 100,
        width = 330, height = "auto",
        leafletSettingsUI(ns("mapSettings"), "Map Settings"),
        tags$br(),
        leafletDataStyleUI(ns("dataStyle"), "Data Grouping"),
        tags$br(),
        leafletExportButton(ns("exportLeaflet"))
      )
    )
  )
}


#' server funtion of interactive map module
#'
#' @param input input
#' @param output output
#' @param session session
#' @param isoData data
#'
#' @export
interactiveMap <- function(input, output, session, isoData){
  ns <- session$ns
  leafletMap <- reactiveVal(leaflet())

  leafletValues <- callModule(leafletSettings, "mapSettings")
  dataStyle <- callModule(leafletDataStyle, "dataStyle", isoData = isoData)

  # Create the map ####

  # set map type
  observeEvent(leafletValues()$leafletType, {
    req(leafletValues()$leafletType)
    leafletMap(leafletMap() %>%
                 drawType(type = leafletValues()$leafletType)
    )
  })

  # add icons to map
  observeEvent(is.na(leafletValues()$scalePosition) |
                 is.na(leafletValues()$northArrowPosition) |
                 is.na(leafletValues()$logoPosition), {
                   leafletMap(leafletMap() %>%
                                drawIcons(scale = !is.na(leafletValues()$scalePosition),
                                          scalePosition = leafletValues()$scalePosition,
                                          northArrow = !is.na(leafletValues()$northArrowPosition),
                                          northArrowPosition = leafletValues()$northArrowPosition,
                                          logoPosition = leafletValues()$logoPosition
                                )
                   )
                 })

  # adjust map center
  observeEvent(leafletValues()$center, {
    req(leafletValues()$center)
    leafletMap(leafletMap() %>%
                 setView(lng = leafletValues()$center$lng,
                         lat = leafletValues()$center$lat,
                         zoom = input$map_zoom
                         ))
  })

  # adjust map bounds fit
  observeEvent(leafletValues()$bounds, {
    req(leafletValues()$bounds)
    #input$map_bounds
    leafletMap(leafletMap() %>%
                 fitBounds(lng1 = leafletValues()$bounds$west,
                           lng2 = leafletValues()$bounds$east,
                           lat1 = leafletValues()$bounds$south,
                           lat2 = leafletValues()$bounds$north
                 ))
  })

  # Render Map ####
  output$map <- renderLeaflet({
    leafletMap()
  })

  # Add Circles ####

  observe({
    req(isoData())

    leafletAddCircles(
        map = leafletProxy("map"),
        isoData = isoData(),
        style = dataStyle(),
        newZoom = input$map_zoom,
        mapBounds = leafletValues()$bounds
    )
  })


  # When map is clicked, show a popup with info
  observe({
    req(input$map_shape_click)
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event)) return()

    isolate({
      showIDPopup(isoData(), event$id, event$lat, event$lng)
    })
  })


  # Show Histograms and Scatterplot in Sidebar ####
  observe({
    req(isoData())
    numVars <- unlist(lapply(names(isoData()), function(x){
      if (is.integer(isoData()[[x]]) | is.numeric(isoData()[[x]]))
        x
      else
        NULL
    }))

    updateSelectInput(session, "var1", choices = c("", numVars))
    updateSelectInput(session, "var2", choices = c("", numVars))
  })

  var1 <- reactive({
    if (is.null(isoData()) | is.null(input$var1) | input$var1 == "")
      return(NULL)

    isoData()[[input$var1]]
  })

  var2 <- reactive({
    if (is.null(isoData()) | is.null(input$var2) | input$var2 == "")
      return(NULL)

    isoData()[[input$var2]]
  })

  callModule(leafletExport, "exportLeaflet", leafletMap = leafletMap,
             width = reactive(input$map_width), height = reactive(input$map_height))

  callModule(sidebarPlot, "plot1", x = var1, nameX = reactive(input$var1))
  callModule(sidebarPlot, "plot2", x = var2, nameX = reactive(input$var2))
  callModule(sidebarPlot, "plot3", x = var1, y = var2,
             nameX = reactive(input$var1), nameY = reactive(input$var2))
}


# Show a popup at the given location
showIDPopup <- function(dat, id, lat, lng) {
  selectedId <- dat[which(dat$id == id), ]

  popup <- paste(
    names(selectedId),
    unlist(lapply(selectedId, as.character)),
    sep = ": ",
    collapse = "</br>")

  leafletProxy("map") %>%
    addPopups(lng, lat, popup, layerId = id,
              options = popupOptions(maxHeight = 200))

}
