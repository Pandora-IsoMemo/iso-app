#' Data import module
#'
#' Displays a button which opens a import dialog when clicked
#'
#' @param id id of module
#' @param label label of button
#' @rdname importData
#'
importDataUI <- function(id, label = "Import Data") {
  ns <- NS(id)
  actionButton(ns("openPopup"), label)
}

#' Server function for data import
#'
#' Backend for data import module
#'
#' @param id namespace id
#' @param rowNames (reactive) use this for rownames of imported data
#' @param colNames (reactive) use this for colnames of imported data
#' @param customWarningChecks list of reactive functions which will be executed after importing of data.
#'   functions need to return TRUE if check is successful or a character with a warning otherwise.
#' @param customErrorChecks list of reactive functions which will be executed after importing of data.
#'   functions need to return TRUE if check is successful or a character with a warning otherwise.
#'
importDataServer <- function(id,
                             rowNames = NULL,
                             colNames = NULL,
                             customWarningChecks = list(),
                             customErrorChecks = list()) {
  moduleServer(id,
               function(input, output, session) {
                 ns <- session$ns

                 values <- reactiveValues(
                   warnings = list(),
                   errors = list(),
                   fileName = NULL,
                   fileImportSuccess = NULL,
                   dataImport = NULL,
                   data = list()
                 )

                 valuesPreview <- reactiveValues(
                   warnings = list(),
                   errors = list(),
                   fileName = NULL,
                   fileImportSuccess = NULL,
                   dataImport = NULL,
                   data = list()
                 )

                 mergeList <- reactiveVal(list())

                 ckanFiles <- reactive({
                   getCKANFiles()
                 })

                 dataSource <- reactiveVal(NULL)

                 # select source server ----

                 observeEvent(input$openPopup, ignoreNULL = TRUE, {
                   reset("file")
                   values$warnings <- list()
                   values$errors <- list()
                   values$fileImportWarning <- NULL
                   values$fileImportSuccess <- NULL
                   values$dataImport <- NULL
                   values$data <- list()
                   dataSource(NULL)

                   showModal(importDataDialog(ns = ns))

                   titles <-
                     unlist(lapply(ckanFiles(), `[[`, "title"))
                   updateSelectInput(session, "ckanRecord", choices = titles)
                 })

                 observeEvent(input$tabImport, {
                   if (input$tabImport == "Merge Data") {
                     shinyjs::hide(ns("addData"), asis = TRUE)
                     shinyjs::hide(ns("accept"), asis = TRUE)
                     shinyjs::show(ns("mergeData"), asis = TRUE)
                   } else {
                     shinyjs::show(ns("addData"), asis = TRUE)
                     shinyjs::show(ns("accept"), asis = TRUE)
                     shinyjs::hide(ns("mergeData"), asis = TRUE)
                   }
                 })

                 ckanRecord <- reactive({
                   req(input$ckanRecord)
                   ckanFiles()[[input$ckanRecord]]
                 })

                 ckanResources <- reactive({
                   req(ckanRecord())

                   resources <- names(ckanRecord()$resources)
                   labels <-
                     unlist(lapply(ckanRecord()$resources, function(x) {
                       paste(x$name, " (", x$format, ")")
                     }))
                   setNames(resources, labels)
                 })

                 observeEvent(ckanResources(), {
                   choices <- ckanResources()
                   updateSelectizeInput(session, "ckanResource", choices = choices)
                 })

                 observe({
                   req(input$source == "ckan")
                   resource <-
                     ckanRecord()$resources[[input$ckanResource]]
                   req(resource)
                   dataSource(list(file = resource$url, filename = resource$url))
                 })

                 observeEvent(input$file, {
                   inFile <- input$file
                   filename <- inFile$name

                   if (is.null(inFile))
                     return()

                   dataSource(list(file = inFile$datapath, filename = filename))
                 })

                 observeEvent(input$url, {
                   req(input$url)
                   req(trimws(input$url) != "")

                   tmp <- tempfile()

                   res <-
                     try(download.file(input$url, destfile = tmp))
                   if (inherits(res, "try-error")) {
                     alert("Could not load remote file")
                     return()
                   }

                   dataSource(list(file = tmp, filename = input$url))
                 })

                 # specify file server ----

                 observeEvent(list(
                   dataSource(),
                   input$type,
                   input$colSep,
                   input$decSep,
                   input$rownames#,
                   #input$includeSd
                 ),
                 {
                   req(dataSource())

                   # reset values
                   valuesPreview$warnings <- list()
                   valuesPreview$errors <- list()
                   valuesPreview$fileImportWarning <- NULL
                   valuesPreview$fileImportSuccess <- NULL
                   valuesPreview$dataImport <- NULL
                   valuesPreview$data <- list()

                   withProgress({
                     # load first lines only
                     valuesPreview <- loadDataWrapper(
                       values = valuesPreview,
                       filepath = dataSource()$file,
                       filename = dataSource()$filename,
                       colNames = colNames,
                       type = input$type,
                       sep = input$colSep,
                       dec = input$decSep,
                       withRownames = isTRUE(input$rownames),
                       headOnly = TRUE,
                       customWarningChecks = customWarningChecks,
                       customErrorChecks = customErrorChecks
                     )
                   },
                   value = 0.75,
                   message = 'loading preview data ...')

                   if (length(valuesPreview$errors) > 0 ||
                       length(valuesPreview$warnings) > 0) {
                     shinyjs::disable(ns("addData"), asis = TRUE)
                     shinyjs::disable(ns("accept"), asis = TRUE)
                   } else {
                     shinyjs::enable(ns("addData"), asis = TRUE)
                     shinyjs::enable(ns("accept"), asis = TRUE)
                     valuesPreview$fileImportSuccess <-
                       "Data import successful"
                   }
                 })

                 output$warning <-
                   renderUI(tagList(lapply(valuesPreview$warnings, tags$p)))
                 output$error <-
                   renderUI(tagList(lapply(valuesPreview$errors, tags$p)))
                 output$success <-
                   renderText(valuesPreview$fileImportSuccess)

                 output$preview <- renderDataTable({
                   req(valuesPreview$dataImport)

                   previewData <-
                     cutAllLongStrings(valuesPreview$dataImport, cutAt = 20)
                   DT::datatable(
                     previewData,
                     filter = "none",
                     selection = "none",
                     rownames = FALSE,
                     options = list(
                       dom = "t",
                       ordering = FALSE,
                       scrollX = TRUE
                     )
                   )
                 })

                 ## button cancel ----
                 observeEvent(input$cancel, {
                   removeModal()
                 })

                 ## button accept ----
                 observeEvent(input$accept, {
                   removeModal()

                   withProgress({
                     # load full data set
                     values <- loadDataWrapper(
                       values = values,
                       filepath = dataSource()$file,
                       filename = dataSource()$filename,
                       colNames = colNames,
                       type = input$type,
                       sep = input$colSep,
                       dec = input$decSep,
                       withRownames = isTRUE(input$rownames),
                       headOnly = FALSE,
                       customWarningChecks = customWarningChecks,
                       customErrorChecks = customErrorChecks
                     )
                   },
                   value = 0.75,
                   message = 'importing full data ...')

                   values$data[[values$fileName]] <-
                     values$dataImport
                 })

                 ## button add data ----
                 observeEvent(input$addData, {
                   withProgress({
                     # load full data set
                     valuesToMerge <- loadDataWrapper(
                       values = values,
                       filepath = dataSource()$file,
                       filename = dataSource()$filename,
                       colNames = colNames,
                       type = input$type,
                       sep = input$colSep,
                       dec = input$decSep,
                       withRownames = isTRUE(input$rownames),
                       # set headOnly = TRUE to FALSE after developement
                       #  warning if data becomes too large
                       headOnly = FALSE,
                       customWarningChecks = customWarningChecks,
                       customErrorChecks = customErrorChecks
                     )

                     ### format column names for merger ----
                     colnames(valuesToMerge$dataImport) <- colnames(valuesToMerge$dataImport) %>%
                       formatColumnNames()
                   },
                   value = 0.75,
                   message = 'loading full data ...')

                   mergeList(c(
                     mergeList(),
                     setNames(
                       list(reactiveValuesToList(valuesToMerge)),
                       valuesToMerge$fileName
                     )
                   ))

                   shinyjs::disable(ns("addData"), asis = TRUE)
                 })

                 joinedData <- mergeDataServer("dataMerger", mergeList = mergeList)

                 ## button merge data ----
                 observeEvent(input$mergeData, {
                   removeModal()
                   #browser()
                   # what filename??
                   # values$errors <-
                   # values$warnings <-
                   # values$fileName <-

                   values$data[["mergedData"]] <- joinedData()
                 })

                 # return value for parent module: ----
                 reactive(values$data)
               })
}

# import data dialog ui
importDataDialog <- function(ns) {
  modalDialog(
    shinyjs::useShinyjs(),
    title = "Import Data",
    footer = tagList(
      actionButton(ns("accept"), "Accept"),
      actionButton(ns("addData"), "Add to Merge Data"),
      actionButton(ns("mergeData"), "Accept"),
      actionButton(ns("cancel"), "Cancel")
    ),
    tabsetPanel(
      id = ns("tabImport"),
      tabPanel("Select Data",
               selectDataTab(ns = ns)),
      tabPanel("Merge Data",
               mergeDataUI(ns("dataMerger")))
    )
  )
}

#' Select Data UI
#'
#' @param ns namespace
selectDataTab <- function(ns) {
  tagList(
    tags$br(),
    fluidRow(
      column(4,
             # select source UI ----
             selectInput(
               ns("source"),
               "Source",
               choices = c(
                 "Pandora Platform" = "ckan",
                 "File" = "file",
                 "URL" = "url"
               )
             )),
      column(
        8,
        conditionalPanel(
          condition = "input.source == 'ckan'",
          ns = ns,
          selectInput(ns("ckanRecord"), "Pandora dataset", choices = NULL),
          selectizeInput(ns("ckanResource"), "Pandora dataset resource", choices = NULL)
        ),
        conditionalPanel(condition = "input.source == 'file'",
                         ns = ns,
                         fileInput(ns("file"), "File")),
        conditionalPanel(condition = "input.source == 'url'",
                         ns = ns,
                         textInput(ns("url"), "URL"))
      )
    ),
    tags$hr(),
    # specify file UI ----
    fluidRow(column(
      4,
      selectInput(
        ns("type"),
        "File type",
        choices = c("xls(x)" = "xlsx", "csv", "ods", "txt"),
        selected = "xlsx"
      )
    ),
    column(
      8,
      conditionalPanel(
        condition = paste0("input.type == 'csv' || input.type == 'txt'"),
        fluidRow(column(
          width = 5,
          textInput(ns("colSep"), "column separator:", value = ",")
        ),
        column(
          width = 5,
          textInput(ns("decSep"), "decimal separator:", value = ".")
        )),
        ns = ns
      )
    )),
    checkboxInput(ns("rownames"), "First column contains rownames"),
    helpText("The first row in your file need to contain variable names."),
    div(class = "text-danger", uiOutput(ns("warning"))),
    div(class = "text-danger", uiOutput(ns("error"))),
    div(class = "text-success", textOutput(ns("success"))),
    tags$hr(),
    tags$h5("Preview Data"),
    fluidRow(column(12,
                    dataTableOutput(ns(
                      "preview"
                    ))))
  )
}

#' Load Data Wrapper
#'
#' @inheritParams importDataServer
#' @param values (list) list with import specifications
#' @param filepath url or path
#' @param filename url or file name
#' @param type (character) file type input
#' @param sep (character) column separator input
#' @param dec (character) decimal separator input
#' @param withRownames (logical) contains rownames input
#' @param headOnly (logical) load only head (first n rows) of file
loadDataWrapper <- function(values,
                            filepath,
                            filename,
                            colNames,
                            type,
                            sep,
                            dec,
                            withRownames,
                            headOnly,
                            customWarningChecks,
                            customErrorChecks) {
  df <- tryCatch(
    loadData(
      file = filepath,
      type = type,
      sep = sep,
      dec = dec,
      rownames = withRownames,
      headOnly = headOnly
    ),
    error = function(e) {
      values$errors <- c(values$errors, "Could not read in file.")
      NULL
    },
    warning = function(w) {
      values$warnings <- c(values$warnings, "Could not read in file.")
      NULL
    }
  )

  #attr(df, "includeSd") <- isTRUE(input$includeSd)

  ## set colnames
  if (!is.null(colNames)) {
    colnames(df) <- rep("", ncol(df))
    mini <- min(length(colNames()), ncol(df))
    colnames(df)[seq_len(mini)] <- colNames()[seq_len(mini)]
  }

  ## Import technically successful
  values$fileName <- filename
  values$dataImport <- as.data.frame(df)

  ## Import valid?
  lapply(customWarningChecks, function(fun) {
    res <- fun()(df)
    if (!isTRUE(res)) {
      values$warnings <- c(values$warnings, res)
    }
  })

  lapply(customErrorChecks, function(fun) {
    res <- fun()(df)
    if (!isTRUE(res)) {
      values$errors <- c(values$errors, res)
    }
  })

  values
}


loadData <-
  function(file,
           type,
           sep = ",",
           dec = ".",
           rownames = FALSE,
           headOnly = FALSE) {
    # if(type == "csv" | type == "txt"){
    #   codepages <- setNames(iconvlist(), iconvlist())
    #   x <- lapply(codepages, function(enc) try(suppressWarnings({read.csv(file,
    #                                                     fileEncoding=enc,
    #                                                     sep = sep, dec = dec,
    #                                                     stringsAsFactors = FALSE,
    #                                                     row.names = NULL,
    #                                                     nrows=3, header=TRUE)}),
    #                                            silent = TRUE)) # you get lots of errors/warning here
    #   x <- x[!sapply(x, function(y) class(y) %in% "try-error")]
    #   maybe_ok <- which(sapply(x, function(y) isTRUE(all.equal(dim(y)[1], c(3)))))
    #   if(length(maybe_ok) > 0){
    #     encTry <- names(maybe_ok[1])
    #   } else {
    #     encTry <- ""
    #   }
    # }
    encTry <- as.character(guess_encoding(file)[1, 1])
    if (type == "xlsx") {
      xlsSplit <- strsplit(file, split = "\\.")[[1]]
      if (xlsSplit[length(xlsSplit)] == "xls") {
        type <- "xls"
      }
    }
    data <- switch(
      type,
      csv = suppressWarnings({
        read.csv(
          file,
          sep = sep,
          dec = dec,
          stringsAsFactors = FALSE,
          row.names = NULL,
          fileEncoding = encTry,
          nrows = getNrow(headOnly, type)
        )
      }),
      txt = suppressWarnings({
        read.csv(
          file,
          sep = sep,
          dec = dec,
          stringsAsFactors = FALSE,
          row.names = NULL,
          fileEncoding = encTry,
          nrows = getNrow(headOnly, type)
        )
      }),
      xlsx = read.xlsx(file, rows = getNrow(headOnly, type)),
      xls = suppressWarnings({
        readxl::read_excel(file, n_max = getNrow(headOnly, type))
      }),
      ods = readODS::read_ods(file, range = getNrow(headOnly, type))
    )

    if (is.null(data))
      return(NULL)

    if (is.null(dim(data))) {
      stop("Could not determine dimensions of data")
      return(NULL)
    }

    if (any(dim(data) == 1)) {
      warning("Number of rows or columns equal to 1")
      return(NULL)
    }

    if (any(dim(data) == 0)) {
      stop("Number of rows or columns equal to 0")
      return(NULL)
    }

    if (rownames) {
      rn <- data[, 1]
      data <- data[, -1, drop = FALSE]
      rownames(data) <- rn
    }
    data <- convertNumeric(data)

    return(data)
  }


#' Cut All Strings
#'
#' @param df (data.frame) data.frame with character and non-character columns
#' @param cutAt (numeric) number of characters after which to cut the entries of an character-column
cutAllLongStrings <- function(df, cutAt = 50) {
  df <- lapply(df, function(z) {
    if (!is.character(z))
      return(z)

    cutStrings(charVec = z, cutAt = cutAt)
  }) %>%
    as.data.frame()

  dfColNames <- colnames(df) %>%
    cutStrings(cutAt = max(10, (cutAt - 3)))
  colnames(df) <- dfColNames

  df
}


#' Cut All Strings
#'
#' @param charVec (character) character vector
#' @param cutAt (numeric) number of characters after which to cut the entries of an character-column
cutStrings <- function(charVec, cutAt = 50) {
  if (any(nchar(charVec) > cutAt, na.rm = TRUE)) {
    index <- !is.na(charVec) & nchar(charVec) > cutAt
    charVec[index] <- paste0(substr(charVec[index], 1, cutAt), "...")
  }

  charVec
}


#' get nRow
#'
#' @param headOnly (logical) if TRUE, set maximal number of rows to n
#' @param type (character) file type
#' @param n (numeric) maximal number of rows if headOnly
getNrow <- function(headOnly, type, n = 3) {
  if (headOnly) {
    if (type == "xlsx")
      return(1:n)
    else
      if (type == "ods")
        return(paste0("A1:C", n))
    else
      return(n)
  } else {
    if (type %in% c("xlsx", "ods"))
      return(NULL)
    else
      if (type == "xls")
        return(Inf)
    else
      return(-999)
  }
}


#' Format Column Names
#'
#' Replaces all not alpha-numeric characters in the names of columns with a dot.
#'
#' @param vNames (character) names of the imported data's columns
#' @param isTest (logical) set TRUE if function is used in tests
formatColumnNames <- function(vNames, isTest = FALSE) {
  message <- NULL

  if (any(grepl("[^[:alnum:] | ^\\.]", vNames))) {
    if (!isTest) {
      message <- "Warning: One or more column names contain non-alphanumeric characters, name changed."
    }
    # replace non-alphanum characters with dot
    vNames <- gsub("[^[:alnum:] | ^\\.]", ".", vNames)
    # remove dots at the beginning of a column name
    vNames <- gsub("^\\.", "", vNames)
  }

  if (any(grepl("^[0-9]{1,}$", substr(vNames, 1, 1)))) {
    if (!isTest) {
      message <- paste(c(message,
                         "Warning: One or more column names begin with number, name changed."),
                       collapse = "\n\n")
    }
    # if name begins with a number paste x before name
    vNames[grepl("^[0-9]{1,}$", substr(vNames, 1, 1))] <-
      paste0("x", vNames[grepl("^[0-9]{1,}$", substr(vNames, 1, 1))])
  }

  if(!isTest && !is.null(message)) {
    shinyjs::alert(message)
  }

  return(vNames)
}
