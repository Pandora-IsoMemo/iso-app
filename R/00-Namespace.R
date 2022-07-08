#' @rawNamespace import(shiny, except = c(renderDataTable, dataTableOutput))
#' @importFrom animation saveGIF
#' @importFrom coda raftery.diag gelman.diag geweke.diag heidel.diag mcmc
#' @importFrom colourpicker colourInput
#' @importFrom rcarbon calibrate sampleDates
#' @importFrom dplyr bind_rows everything group_by group_indices inner_join left_join select
#' @importFrom DT datatable renderDataTable dataTableOutput
#' @importFrom geometry convhulln inhulln
#' @importFrom ggplot2 ggplot theme theme_light coord_cartesian geom_point theme_light theme labs
#' geom_errorbar aes_ element_blank element_text position_dodge aes geom_boxplot xlab
#' @importFrom graphics axis filled.contour hist plot points title boxplot par image lines
#' polygon text .filled.contour Axis box layout lcm plot.new plot.window rect legend strwidth text
#' @importFrom grDevices colorRampPalette chull png pdf tiff svg dev.off recordPlot replayPlot
#' cm.colors pdfFonts
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom ks kde Hpi.diag Hpi
#' @importFrom leaflet addAwesomeMarkers awesomeIcons addCircles addLegend
#' addMarkers addPopups addRectangles addTiles clearMarkerClusters clearPopups colorFactor fitBounds
#' leaflet leafletProxy leafletOutput markerClusterOptions popupOptions removeControl
#' removeScaleBar removeShape renderLeaflet setMaxBounds setView addProviderTiles
#' addScaleBar scaleBarOptions addControl
#' @importFrom magrittr "%>%"
#' @importFrom maps map map.scale
#' @importFrom MASS mvrnorm kde2d
#' @importFrom mgcv gam gamm smoothCon s Predict.matrix rig te
#' @importFrom openxlsx read.xlsx write.xlsx
#' @importFrom raster raster getValues terrain writeRaster
#' @importFrom RColorBrewer brewer.pal
#' @importFrom readr guess_encoding
#' @importFrom readxl read_excel
#' @importFrom Rfast spdinv Crossprod rmvnorm
#' @importFrom rlang .data
#' @importFrom shinyjs alert reset runjs useShinyjs
#' @importFrom shinyMatrix matrixInput updateMatrixInput
#' @importFrom shinyWidgets pickerInput updatePickerInput
#' @importFrom stats aggregate as.formula dunif model.matrix na.exclude na.omit predict
#' quantile rgamma rnorm runif sd setNames mahalanobis pchisq cov pf rbeta
#' @importFrom sp point.in.polygon plot
#' @importFrom stats aggregate as.formula model.matrix na.exclude na.omit predict quantile dnorm qnorm residuals pnorm var
#' sd setNames cor density median kmeans
#' @importFrom stringi stri_escape_unicode
#' @importFrom templates tmpl
#' @importFrom utils available.packages compareVersion install.packages head
#' packageVersion read.csv read.csv2 write.table installed.packages capture.output download.file
NULL
