#' @rawNamespace import(shiny, except = c(renderDataTable, dataTableOutput))
#' @importFrom animation saveGIF
#' @importFrom coda raftery.diag gelman.diag geweke.diag heidel.diag mcmc
#' @importFrom colourpicker colourInput
#' @importFrom rcarbon calibrate sampleDates
#' @importFrom DataTools cutAllLongStrings downUploadButtonUI downUploadButtonServer getMappingIds
#'  getDatabaseList getMappingTable getRemoteData importDataUI importDataServer
#'  tryCatchWithWarningsAndErrors updateNameEntryIfDuplicate
#' @importFrom dplyr arrange distinct filter group_by group_by_at left_join select summarise ungroup
#' @importFrom DT datatable renderDataTable dataTableOutput
#' @importFrom geometry convhulln inhulln
#' @importFrom ggplot2 ggplot theme theme_light coord_cartesian geom_point theme_light theme labs
#'  geom_errorbar aes_ element_blank element_text position_dodge aes geom_boxplot xlab ylab
#' @importFrom graphics axis filled.contour hist plot points title boxplot par image lines
#'  polygon text .filled.contour Axis box layout lcm plot.new plot.window rect legend strwidth text
#' @importFrom grDevices cm.colors colorRampPalette col2rgb chull dev.off jpeg png pdf pdfFonts
#'  recordPlot replayPlot rgb svg tiff
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom ks kde Hpi.diag Hpi
#' @importFrom leaflet addAwesomeMarkers addCircleMarkers addControl addLayersControl addLegend
#'  addMarkers addPopups addProviderTiles addRectangles addScaleBar addTiles awesomeIcons
#'  clearMarkerClusters clearGroup clearPopups colorFactor colorNumeric fitBounds icons
#'  layersControlOptions leaflet leafletProxy leafletOutput markerClusterOptions popupOptions
#'  removeControl removeScaleBar removeShape renderLeaflet scaleBarOptions setMaxBounds setView
#' @importFrom magick image_animate image_join image_read image_read_pdf image_read_svg image_write
#' @importFrom magrittr "%>%"
#' @importFrom maps map map.scale
#' @importFrom MASS mvrnorm kde2d
#' @importFrom mgcv gam gamm smoothCon s Predict.matrix rig te
#' @importFrom mice mice complete
#' @importFrom openxlsx read.xlsx write.xlsx
#' @importFrom pgdraw pgdraw
#' @importFrom raster raster getValues terrain writeRaster
#' @importFrom RColorBrewer brewer.pal
#' @importFrom Rfast spdinv Crossprod rmvnorm
#' @importFrom rgeos gCentroid
#' @importFrom rlang .data
#' @importFrom shinycssloaders withSpinner
#' @importFrom shinyjs alert runjs useShinyjs hide show
#' @importFrom shinyMatrix matrixInput updateMatrixInput
#' @importFrom shinyWidgets pickerInput updatePickerInput
#' @importFrom stats aggregate as.formula cor cov density dist dnorm dunif kmeans
#'  mahalanobis median model.matrix na.exclude na.omit pchisq  pf pnorm predict
#'  qnorm quantile rbeta residuals rgamma rnorm runif sd var
#' @importFrom sp point.in.polygon plot SpatialPoints
#' @importFrom stringi stri_escape_unicode
#' @importFrom utils available.packages compareVersion install.packages head
#'  packageVersion read.csv read.csv2 write.table installed.packages capture.output
#' @importFrom webshot is_phantomjs_installed
#' @importFrom zip zipr
NULL
