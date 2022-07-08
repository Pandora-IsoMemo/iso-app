# Merge Via Command Module ----

#' Merge Via Command UI
#'
#' UI of the merge Via command module
#'
#' @param id id of module
mergeViaCommandUI <- function(id) {
  ns <- NS(id)

  tagList(
    tags$br(),
    # output for path to table1, table2
    # textAreaInput(
    #   ns("table1"),
    #   "table1",
    #   value = NULL,
    #   width = "100%"
    # ),
    # textAreaInput(
    #   ns("table2"),
    #   "table2",
    #   value = NULL,
    #   width = "100%"
    # ),
    tags$em(paste0("This is experimental and under development. No checks of column types implemented. App crashes easily.")),
    textAreaInput(
      ns("mergeCommand"),
      "Merge command",
      value = NULL,
      width = "100%"
    )
  )
}

#' Merge Command Server
#'
#' Server function of the merge Command module
#' @param id id of module
#' @param mergeList (list) list of data to be merged
mergeViaCommandServer <- function(id, mergeCommandAuto) {
  moduleServer(id,
               function(input, output, session) {
                 mergeCommandManual <- reactiveVal()

                 # update: mergeCommand ----
                 observeEvent(mergeCommandAuto(), {
                   updateTextAreaInput(session, "mergeCommand", value = mergeCommandAuto())
                 })

                 observeEvent(input$mergeCommand, {
                   req(input$mergeCommand)
                   mergeCommandManual(input$mergeCommand)
                 })

                 return(mergeCommandManual)
               })
}
