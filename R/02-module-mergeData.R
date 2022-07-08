# Merge Data Module ----

#' Merge Data UI
#'
#' UI of the merge data module
#'
#' @param id id of module
mergeDataUI <- function(id) {
  ns <- NS(id)

  tagList(
    tags$br(),
    selectInput(
      ns("tableX"),
      "Select tabel x",
      choices = NULL,
      width = "100%"
    ),
    selectInput(
      ns("tableY"),
      "Select tabel y",
      choices = NULL,
      width = "100%"
    ),
    conditionalPanel(condition = "input.useMergeViaCommand == false",
                     mergeViaUIUI(ns("mergerViaUI")),
                     ns = ns),
    conditionalPanel(
      condition = "input.useMergeViaCommand == true",
      mergeViaCommandUI(ns("mergerViaCommand")),
      ns = ns
    ),
    checkboxInput(ns("useMergeViaCommand"),
                  "Merge via command line"),
    actionButton(ns("applyMerge"), "Apply"),
    #actionButton(ns("addMerge"), "Add Table"),
    fluidRow(column(12,
                    dataTableOutput(ns(
                      "joinedData"
                    ))))
  )
}

#' Merge Data Server
#'
#' Server function of the merge data module
#' @param id id of module
#' @param mergeList (list) list of data to be merged
mergeDataServer <- function(id, mergeList) {
  moduleServer(id,
               function(input, output, session) {
                 tableId <- reactiveValues(tableX = NULL,
                                           tableY = NULL)

                 tableIds <- reactiveVal()

                 tableXData <- reactiveVal()
                 tableYData <- reactiveVal()

                 joinedData <- reactiveVal()

                 # update: table selection ----
                 observeEvent(mergeList(), {
                   req(length(mergeList()) > 0)

                   tableIds(extractTableIds(names(mergeList())))

                   tableChoices <- extractMergeChoices(mergeList())

                   updateSelectInput(session,
                                     "tableX",
                                     choices = tableChoices,
                                     selected = tableChoices[1])

                   updateSelectInput(session,
                                     "tableY",
                                     choices = tableChoices,
                                     selected = tableChoices[2])
                 })

                 observeEvent(input$tableX, {
                   tableXData(mergeList()[[input$tableX]]$dataImport)
                 })

                 observeEvent(input$tableY, {
                   tableYData(mergeList()[[input$tableY]]$dataImport)
                 })

                 mergeCommandAuto <-
                   mergeViaUIServer(
                     "mergerViaUI",
                     tableXData = tableXData,
                     tableYData = tableYData,
                     tableXId = reactive(tableIds()[input$tableX]),
                     tableYId = reactive(tableIds()[input$tableY])
                   )

                 mergeCommandManual <-
                   mergeViaCommandServer("mergerViaCommand", mergeCommandAuto)

                 # apply: mergeCommand ----
                 observeEvent(input$applyMerge, {
                   req(!is.null(mergeCommandManual()), input$applyMerge)

                   withProgress({
                     browser()
                     ## create data.frames to merge ----
                     for (i in c(input$tableX, input$tableY)) {
                       assign(tableIds()[i],
                              mergeList()[[i]]$dataImport)
                     }

                     ## match column types ----
                     columsToJoinString <- mergeCommandManual() %>%
                       gsub(pattern = ".*by = ", replacement = "") %>%
                       gsub(pattern = ")$", replacement = "")

                     columsToJoin <-
                       eval(parse(text = columsToJoinString))

                     xColNames <- names(columsToJoin)
                     yColNames <- unname(columsToJoin)

                     assign(
                       tableIds()[input$tableY],
                       matchColClasses(
                         df1 = get(tableIds()[input$tableX]),
                         df2 = get(tableIds()[input$tableY]),
                         xColNames = xColNames,
                         yColNames = yColNames,
                         df1Id = tableIds()[input$tableX]
                       )
                     )

                     ## merge data ----
                     joinedData <-
                       tryCatch({
                         eval(parse(text = mergeCommandManual()))
                       },
                       error = function(cond) {
                         alert(cond$message)
                         # Choose a return value in case of error
                         return(NULL)
                       },
                       warning = function(cond) {
                         alert(cond$message)
                         # Choose a return value in case of warning
                         return(NULL)
                       },
                       finally = NULL)
                     if (inherits(joinedData, "try-error")) {
                       alert("Could not merge data")
                       return()
                     }

                     if (!is.null(joinedData) &&
                         nrow(joinedData) > 100000)
                       alert(
                         paste0(
                           "Warning: Merged data is very large and has ",
                           nrow(joinedData),
                           "rows.",
                           "The app might be very slow or even crash."
                         )
                       )

                     joinedData(joinedData)
                   },
                   value = 0.75,
                   message = 'merging data ...')
                 })

                 output$joinedData <- renderDataTable({
                   req(joinedData())
                   DT::datatable(joinedData(),
                                 rownames = FALSE,
                                 options = list(scrollX = TRUE))
                 })

                 # return value for parent module: ----
                 return(joinedData)

               })
}


# Merge Data Helper Functions ----

## helpers: table selection ----
extractMergeChoices <- function(tableList) {
  tableChoices <- names(tableList)
  names(tableChoices) <-
    paste0(extractTableIds(tableChoices), " --- ", tableChoices)

  tableChoices
}


extractTableIds <- function(namesOfTables) {
  ids <- paste0("table", 1:length(namesOfTables))
  names(ids) <- namesOfTables

  ids
}

## helpers: class matching----
matchColClasses <-
  function(df1,
           df2,
           xColNames,
           yColNames,
           df1Id = "table1",
           isTest = FALSE) {
    colTypesX <- sapply(df1[, xColNames], class)
    colTypesY <- sapply(df2[, yColNames], class)

    isAllEqual <- equalColClasses(colTypesX,
                                  colTypesY,
                                  df1Id = df1Id,
                                  isTest = isTest)

    if (!isAllEqual) {
      for (i in 1:length(yColNames)) {
        class(df2[, yColNames[i]]) <- colTypesX[i]
      }
    }
    return(df2)
  }


equalColClasses <-
  function(colTypesX,
           colTypesY,
           df1Id = "table1",
           isTest = FALSE) {
    typeMismatch <- colTypesX != colTypesY

    if (any(typeMismatch)) {
      if (!isTest) {
        shinyjs::alert(
          paste0(
            "Warning: Column types not matching for: \n",
            extractJoinString(names(colTypesX)[typeMismatch],
                              names(colTypesY)[typeMismatch]),
            ". \n\n",
            "Using the type of ",
            df1Id,
            " for these columns."
          )
        )
      }
      return(FALSE)
    } else {
      return(TRUE)
    }
  }
