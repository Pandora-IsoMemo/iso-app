#' Augment Data
#'
#' @param data (data.frame) data containing columns "Latitude" and "Longitude"
#' @return (data.frame) augmented data
augmentData <- function(data) {
  if (!(
    max(data$Latitude) > 75 ||
    max(data$Longitude) > 150 ||
    min(data$Longitude) < -150 || min(data$Latitude) < -75
  )) {
    return(data)
  }

  ### data augmentation
  dataBottom  <- data
  dataTop  <- data
  dataLeft  <- data
  dataRight  <- data
  dataLeftBottom <- data
  dataLeftTop <- data
  dataRightBottom <- data
  dataRightTop <- data

  dataBottom$Latitude <- -90 - (dataBottom$Latitude + 90)
  dataBottom$Longitude <- dataBottom$Longitude + 180
  dataBottom$Longitude[dataBottom$Longitude >= 180] <- dataBottom$Longitude[dataBottom$Longitude >= 180] - 360

  dataTop$Latitude <- 90 + (90 - dataTop$Latitude)
  dataTop$Longitude <- dataTop$Longitude + 180
  dataTop$Longitude[dataTop$Longitude >= 180] <- dataTop$Longitude[dataTop$Longitude >= 180] - 360

  dataLeft$Longitude <- -180 - (-dataLeft$Longitude + 180)
  dataRight$Longitude <- 180 + (180 + dataRight$Longitude)

  dataLeftBottom$Longitude <- dataBottom$Longitude - 360
  dataLeftBottom$Latitude <- dataBottom$Latitude

  dataLeftTop$Longitude <- dataTop$Longitude - 360
  dataLeftTop$Latitude <- dataTop$Latitude

  dataRightBottom$Longitude <- dataBottom$Longitude + 360
  dataRightBottom$Latitude <- dataBottom$Latitude

  dataRightTop$Longitude <- dataTop$Longitude + 360
  dataRightTop$Latitude <- dataTop$Latitude

  data2 <- rbind(
    data,
    dataLeft,
    dataRight,
    dataBottom,
    dataTop,
    dataLeftBottom,
    dataLeftTop,
    dataRightBottom,
    dataRightTop
  )
  #data2 <- rbind(data, dataLeft, dataRight, dataBottom, dataTop)

  data2 <- data2[data2$Latitude > -90 & data2$Latitude < 90 &
                   data2$Longitude > -320 &
                   data2$Longitude < 320, ]

  if (length(unique(data$Site)) == nrow(data)) {
    data2$Site = 1:nrow(data2)
  }

  return(data2)
}

#' Shift Data To Default Restriction
#'
#' @param data (data.frame) data containing columns "Latitude" and "Longitude"
#' @return (data.frame) data containing columns "Latitude" and "Longitude" shifted to the default restriction
shiftDataToDefaultRestriction <- function(data) {
  # shift data such that it is in the range of -180 to 180 and -90 to 90
  data$Longitude[data$Longitude > 180] <- data$Longitude[data$Longitude > 180] - 360
  data$Longitude[data$Longitude < -180] <- data$Longitude[data$Longitude < -180] + 360
  data$Latitude[data$Latitude > 90] <- data$Latitude[data$Latitude > 90] - 180
  data$Latitude[data$Latitude < -90] <- data$Latitude[data$Latitude < -90] + 180

  return(data)
}

#' Center Data
#'
#' @param data (data.frame) data containing columns "Latitude" and "Longitude"
#' @param center (character) center to shift data to, either "Europe" or "Pacific"
#' @return (data.frame) data shifted to the center
centerData <- function(data, center = c("Europe", "Pacific")) {
  center <- match.arg(center)

  if (center == "Pacific") {
    data$Longitude[data$Longitude < 0] <- data$Longitude[data$Longitude < 0] + 360
    data$Longitude[data$Longitude > 360] <- data$Longitude[data$Longitude > 360] - 360
  }

  if (center == "Europe") {
    data$Longitude[data$Longitude < -180] <- data$Longitude[data$Longitude < -180] + 360
    data$Longitude[data$Longitude > 180] <- data$Longitude[data$Longitude > 180] - 360
  }

  return(data)
}

shiftLongitudesToPacific <- function(longitudes) {
  longitudesOrig <- longitudes
  longitudes[longitudesOrig < 0] <- longitudes[longitudesOrig < 0] + 180
  longitudes[longitudesOrig >= 0] <- (-180 + longitudes[longitudesOrig >= 0])

  longitudes
}
