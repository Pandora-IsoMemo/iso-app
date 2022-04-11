# helper functions ####

#'  draw Interactive Map
#' @param isoData isoData data
#' @param zoom zoom
#' @param type map type
#' @param northArrow show north arrow?
#' @param northArrowPosition position of north arrow
#' @param scale show scale?
#' @param scalePosition position of scale
#' @param logoPosition character position of logo if selected, else NA
#' @param center where to center map (list of lat and lng)
#'
#' @export
draw <- function(isoData,
                 zoom = 5,
                 type = "1",
                 northArrow = FALSE,
                 northArrowPosition = "bottomright",
                 scale = FALSE,
                 scalePosition = "topleft",
                 logoPosition = NA,
                 center = NULL) {
  map <- leaflet() %>% drawType(type = type)
  map <-
    map %>% drawIcons(
      northArrow = northArrow,
      northArrowPosition = northArrowPosition,
      scale = scale,
      scalePosition = scalePosition,
      logoPosition = logoPosition
    )
  map
}


#' Draw Type of Interactive Map
#'
#' @inheritParams draw
#' @param map leaflet map
drawType <- function(map, type = "1") {
  if (type == "1") {
    mType <- "CartoDB.Positron"
  }
  if (type == "2") {
    mType <- "OpenStreetMap.Mapnik"
  }
  if (type == "3") {
    mType <- "OpenStreetMap.DE"
  }
  if (type == "4") {
    mType <- "OpenTopoMap"
  }
  if (type == "5") {
    mType <- "Stamen.TonerLite"
  }
  if (type == "5") {
    mType <-  "Esri"
  }
  if (type == "6") {
    mType <- "Esri.WorldTopoMap"
  }
  if (type == "7") {
    mType <-  "Esri.WorldImagery"
  }

  map <- map %>%
    addProviderTiles(mType)

  map
}



#' Draw Icons on Interactive Map
#'
#' @inheritParams draw
#' @param map leaflet map
drawIcons <- function(map,
                      northArrow = FALSE,
                      northArrowPosition = "bottomright",
                      scale = FALSE,
                      scalePosition = "topleft",
                      logoPosition = NA) {
  map <- map %>%
    clearControls() %>%
    removeScaleBar()

  if (!is.na(logoPosition)) {
    map <- map %>%
      addControl(
        tags$img(
          src = "https://isomemo.com/images/logo.jpg",
          width = "75",
          height = "50"
        ),
        position = logoPosition,
        className = ""
      )
  }

  if (northArrow &&
      (northArrowPosition %in% c("bottomright", "bottomleft"))) {
    if (scale) {
      map <- addScaleBar(map,
                         position = scalePosition,
                         options = scaleBarOptions())
    }

    if (northArrow) {
      map <- addControl(
        map,
        tags$img(
          src = "https://isomemodb.com/NorthArrow.png",
          width = "80",
          height = "80"
        ),
        position = northArrowPosition,
        className = ""
      )
    }
  } else {
    if (northArrow) {
      map <- addControl(
        map,
        tags$img(
          src = "https://isomemodb.com/NorthArrow.png",
          width = "80",
          height = "80"
        ),
        position = northArrowPosition,
        className = ""
      )
    }

    if (scale) {
      map <- addScaleBar(map,
                         position = scalePosition,
                         options = scaleBarOptions())
    }
  }

  map
}


#' Add Circles to existing Map
#'
#' @param map leaflet map
#' @param isoData isoData data
#' @param style reactive list of values to style the circles
#' @param newZoom numeric zoom factor used for lat/lng jitter and radius calculation
#' @param mapBounds bounds for the lat/lng frame
leafletAddCircles <-
  function(map, isoData, style, newZoom, mapBounds) {
    map <- map %>%
      clearShapes() %>%
      addRectangles(
        lng1 = mapBounds$west,
        lng2 = mapBounds$east,
        lat1 = mapBounds$south,
        lat2 = mapBounds$north,
        color = "blue",
        weight = 1,
        fillColor = "transparent"
      )

    if (!style$customizeMarkers) {
      # Add Circles relative to zoom
      new_zoom <- newZoom
      if (is.null(new_zoom))
        return()
      map <- isolate({
        addCirclesRelativeToZoom(map,
                                 isoData,
                                 newZoom = new_zoom,
                                 zoom = 4)
      })
    } else {
      # Add custom circles
      map <- map %>%
        clearShapes()

      map <- map %>%
        addCustomCircles(isoData = isoData,
                         style = style)

      # awesomeMarkers only works with little data:
      #
      # icons <- extractMarker(style)
      # dataForMarkers <- isoData[1:100, ]
      #
      # map %>%
      #   addAwesomeMarkers(lng = dataForMarkers$longitude,
      #                     lat = dataForMarkers$latitude,
      #                     icon = icons[dataForMarkers$source])
    }

    map
  }


extractMarker <- function(dataStyle) {
  awesomeIconNameList <- dataStyle$shapes
  icons <- lapply(awesomeIconNameList, function(shape) {
    makeAwesomeIcon(icon = shape,
                    library = "fa",
                    markerColor = "green")
  })
  names(icons) <- names(awesomeIconNameList)

  do.call(awesomeIconList, icons)
}


#' Add Custom Circles to existing Map
#'
#' @inheritParams leafletAddCircles
addCustomCircles <- function(map, isoData, style) {
  if (is.null(isoData$latitude) ||
      all(is.na(isoData$latitude)))
    return(map)

  isoData <- isoData[!is.na(isoData$longitude),]

  pal <- colorFactor(style$colours, isoData$source)

  map %>%
    addCircles(
      lng = isoData$longitude,
      lat = isoData$latitude,
      color = pal(isoData[[style$groupingColumn]]),
      radius = unname(style$radiusKm[isoData[[style$groupingColumn]]]) * 1000,
      stroke = FALSE,
      fillOpacity = unname(style$opacity[isoData[[style$groupingColumn]]])
    ) %>%
    addLegend(
      "topleft",
      pal = pal,
      values = isoData[[style$groupingColumn]],
      title = style$groupingColumn,
      layerId = "colorLegend"
    )
}


#' Add Circles to existing Map
#'
#' @inheritParams leafletAddCircles
#' @param zoom numeric default zoom factor used for lat/lng jitter and radius calculation
addCirclesRelativeToZoom <- function(map, isoData,
                                     newZoom, zoom = 5) {
  if (is.null(isoData$latitude) ||
      all(is.na(isoData$latitude)))
    return(map)

  isoData <- isoData[!is.na(isoData$longitude),]

  numColors <- length(unique(isoData$source))

  colors <- appColors(c("red", "green", "purple", "black"),
                      names = FALSE)[1:numColors]

  pal <- colorFactor(colors, isoData$Source)

  set.seed(20180213)
  isoData$Latitude_jit <-
    jitter(isoData$latitude, amount = 0.05 * (zoom / newZoom) ^ 2)
  isoData$Longitude_jit <-
    jitter(isoData$longitude, amount = 0.05 * (zoom / newZoom) ^ 2)

  map %>%
    addCircles(
      data = isoData,
      lat = ~ Latitude_jit,
      lng =  ~ Longitude_jit,
      layerId = ~ id,
      stroke = F,
      fillOpacity = 0.7,
      color = pal(isoData$source),
      fillColor = pal(isoData$source),
      radius = 20000 * (zoom / newZoom) ^ 3
    ) %>%
    addLegend(
      "topleft",
      pal = pal,
      values = isoData$source,
      title = "Database",
      layerId = "colorLegend"
    )

}
