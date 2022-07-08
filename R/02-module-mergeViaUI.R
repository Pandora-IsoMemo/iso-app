# Merge Via UI Module ----

#' Merge Via UI UI
#'
#' UI of the merge via UI module
#'
#' @param id id of module
mergeViaUIUI <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(column(
      4,
      selectInput(
        ns("mergeOperation"),
        "Select operation",
        choices = c(
          "all rows in x and y" = "inner_join",
          "all rows in x" = "left_join",
          "all rows in y" = "right_join",
          "all rows in x or y" = "full_join"
        ),
        selected = "left_join"
      )
    ),
    column(
      8,
      checkboxInput(ns("addAllCommonColumns"), "Join on all common columns")
    )),
    fluidRow(column(
      6,
      selectInput(
        ns("columnsX"),
        "Select x columns",
        choices = NULL,
        multiple = TRUE
      )
    ),
    column(
      6,
      selectInput(
        ns("columnsY"),
        "Select y columns",
        choices = NULL,
        multiple = TRUE
      )
    ))
  )
}

#' Merge Via UI Server
#'
#' Server function of the merge via UI module
#' @param id id of module
#' @param mergeList (list) list of data to be merged
mergeViaUIServer <- function(id, tableXData, tableYData, tableXId, tableYId) {
  moduleServer(id,
               function(input, output, session) {
                 columnsToJoin <- reactiveValues(tableX = NULL,
                                                 tableY = NULL)
                 mergeCommandAuto <- reactiveVal()

                 # update: column selection ----
                 observeEvent(tableXData(), {
                   req(tableXData())
                   updateSelectInput(session, "columnsX",
                                     choices = colnames(tableXData()))
                 })

                 observeEvent(tableYData(), {
                   req(tableYData())
                   updateSelectInput(session, "columnsY",
                                     choices = colnames(tableYData()))
                 })

                 observeEvent(input$addAllCommonColumns, {
                   req(tableXData(), tableYData())
                   commonColumns <- intersect(colnames(tableXData()), colnames(tableYData()))

                   #req(commonColumns())
                   if (input$addAllCommonColumns) {
                     updateSelectInput(session, "columnsX",
                                       selected = commonColumns)
                     updateSelectInput(session, "columnsY",
                                       selected = commonColumns)
                   } else {
                     updateSelectInput(session, "columnsX",
                                       selected = list())
                     updateSelectInput(session, "columnsY",
                                       selected = list())
                   }
                 })

                 # create: mergeCommandAuto ----
                 observeEvent(list(input$columnsX, input$columnsY), {
                   req(input$columnsX, input$columnsY)

                   columnsToJoin$tableX <-
                     equalizeLength(input$columnsX, input$columnsY)$xColNames
                   columnsToJoin$tableY <-
                     equalizeLength(input$columnsX, input$columnsY)$yColNames

                   colJoinString <-
                     extractJoinString(columnsToJoin$tableX, columnsToJoin$tableY)

                   if (!is.null(colJoinString) &&
                       (tableXId() != tableYId())) {
                     mergeCommandAuto(
                       tmpl(
                         paste0(
                           c(
                             "{{ tableX }} %>% ",
                             "  {{ mergeOperation }}({{ tableY }},",
                             "    by = {{ colJoinString }})"
                           ),
                           collapse = ""
                         ),
                         tableX = tableXId(),
                         mergeOperation = input$mergeOperation,
                         tableY = tableYId(),
                         colJoinString = colJoinString
                       ) %>% as.character()
                     )
                   } else {
                     mergeCommandAuto("")

                     if (tableXId() == tableYId()) {
                       alert("Please choose two different table.")
                     }
                   }
                 })

                 # return value for parent module: ----
                 return(mergeCommandAuto)
               })
}


# Merge Via UI Helper Functions ----

## helpers: column selection ----
extractCommonColumns <- function(tableList, tableX, tableY) {
  colnamesX <- extractColNames(tableList[[tableX]])
  colnamesY <- extractColNames(tableList[[tableY]])

  intersect(colnamesX, colnamesY)
}


extractColNames <- function(tableListElement) {
  colnames(tableListElement$dataImport)
}


equalizeLength <- function(xColNames, yColNames) {
  minLength <- min(length(xColNames), length(yColNames))

  if (minLength == 0) {
    return(NULL)
  }

  list(xColNames = xColNames[1:minLength],
       yColNames = yColNames[1:minLength])
}


extractJoinString <- function(xColumns, yColumns) {
  xColumns <- paste0("\"", xColumns, "\"")
  yColumns <- paste0("\"", yColumns, "\"")

  res <- paste(xColumns, yColumns, sep = "=")
  res <- paste(res, collapse = ", ")
  paste0("c(", res, ")")
}
