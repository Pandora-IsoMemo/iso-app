unpackModel <- function(uploadedModel) {
  if (all(names(uploadedModel) %in% c("currentModel", "savedMaps"))) {
    # new format, version >= 23.05.4
    uploadedModel$currentModel
  } else {
    # old format
    uploadedModel
  }
}

unpackSavedMaps <- function(uploadedModel, currentSavedMaps) {
  if (all(names(uploadedModel) %in% c("currentModel", "savedMaps"))) {
    # new format, version >= 23.05.4
    uploadedModel$savedMaps %>%
      updateNameEntryIfDuplicate(oldList = currentSavedMaps,
                                 listType = "Saved map")
  } else {
    # old format, savedMaps were not included in downloads
    list()
  }
}
