#' ui function of modelResultsAssignR module
#'
#' @param id namespace
#' @param title title in tab
#'
#' @export
modelResultsAssignUI <- function(id, title = "") {
  ns <- NS(id)
  tabPanel(
    title,
    id = id,
    value = id,
    fluidRow(
      class = "modeling-content",
      sidebarPanel(
        width = 2,
        style = "position:fixed; width:14%; max-width:220px; overflow-y:auto; height:88%",
        downUploadButtonUI(ns("downUpload"), title = "Load a Model", label = "Upload / Download"),
        textAreaInput(ns("modelNotes"), label = NULL, placeholder = "Description ..."),
        tags$hr(),
        selectInput(ns("dataSource"),
          "Data source",
          choices = c(
            "Database" = "db",
            "Upload file" = "file",
            "Saved map" = "model"
          ),
          selected = "db"
        ),
        conditionalPanel(
          condition = "input.dataSource == 'file'",
          importDataUI(ns("importData"), "Import Data"),
          tags$hr(),
          ns = ns
        ),
        conditionalPanel(
          condition = "input.dataSource != 'model'",
          ns = ns,
          selectInput(
            inputId = ns("IndependentX"),
            label = "Dependent variable:",
            choices = NULL
          ),
          selectInput(
            inputId = ns("numVars"),
            label = "Numeric x-variables:",
            choices = NULL,
            multiple = TRUE
          ),
          selectInput(
            inputId = ns("catVars"),
            label = "Categorical x-variables:",
            choices = NULL,
            multiple = TRUE
          ),
          selectInput(
            inputId = ns("numVarsUnc"),
            label = "Numeric x-variables uncertainty (optional):",
            choices = NULL,
            multiple = TRUE
          ),
          selectInput(
            inputId = ns("catVarsUnc"),
            label = "Categorical x-variables uncertainty (optional):",
            choices = NULL,
            multiple = TRUE
          )
        ),
        checkboxInput(inputId = ns("imputeMissings"), label = "Impute missings (Multiple imputation)", value = TRUE),
        sliderInput(
          inputId = ns("Iter"),
          label = "Number of MCMC iterations",
          min = 100, max = 100000, value = 3000, step = 100
        ),
        sliderInput(
          inputId = ns("burnin"), label = "Number of burnin iterations",
          value = 1000, min = 100, max = 10000, step = 100
        ),
        sliderInput(
          inputId = ns("nChains"), label = "Number of MCMC chains",
          value = 2, min = 1, max = 16, step = 1
        ),
        sliderInput(
          inputId = ns("thinning"), label = "MCMC thinning (keep every x-th sample)",
          value = 10, min = 1, max = 20, step = 1
        ),
        actionButton(ns("start"), "Start")
      ),
      mainPanel(
        width = 8,
        selectInput(ns("estType"),
                    "Estimation type",
                    choices = c(
                      "Mean" = "mean",
                      "SD" = "sd",
                      "Quantile" = "quantile"
                    ),
                    selected = "mean"
        ),
        conditionalPanel(
          condition = "input.estType == 'quantile'",
          ns = ns,
          sliderInput(ns("quantile"),
                      "Quantile",
                      min = 0.001,
                      max = 0.999,
                      step = 0.001,
                      value = 0.95
          )
        ),
        radioButtons(
          inputId = ns("aggType"),
          label = "Aggregate by:",
          choices = c(
            "Individual / row" = "single",
            "Category" = "cat",
            "All" = "all"
          )
        ),
        conditionalPanel(
          condition = "input.aggType == 'single'",
          ns = ns,
          checkboxInput(ns("showData"),
                        "Show original data",
                        value = TRUE
          )
        ),
        conditionalPanel(
          condition = "input.aggType == 'cat'",
          ns = ns,
          selectInput(
            inputId = ns("catAgg"),
            label = "Category variable:",
            choices = NULL
          )
        ),
        radioButtons(ns("predType"), "Prediction type", choices = c(
          "Model data" = "1",
          "Prediction data" = "2"
        )),
        conditionalPanel(
          condition = "input.predType == '2'",
          ns = ns,
          importDataUI(ns("localData"), "Import Prediction Data"),
          checkboxInput(ns("other"),
                        "Add column",
                        value = FALSE
          )
        ),
        conditionalPanel(
          condition = "input.other == true && input.predType == '2'",
          ns = ns,
          numericInput(ns("prior1"),
                       label = "Beta-Prior 1: probability of another assignment",
                       min = 0, max = 100, value = 2, step = 0.1
          ),
          numericInput(ns("prior2"),
                       label = "Beta-Prior 2: probability of another assignment",
                       min = 0, max = 100, value = 2, step = 0.1
          )
        ),
        fluidRow(
          column(8,
                 DT::dataTableOutput(ns("dataTable")),
          )),
        tags$br(),
        tags$br(),
        tags$hr(),
        modelDiagButton(ns("modelDiag")),
        dataExportButton(ns("exportData"))
      ),
    )
  )
}

#' server function of model Results module
#'
#' @param input input
#' @param output output
#' @param session session
#' @param isoData data
#'
#' @export
modelResultsAssign <- function(input, output, session, isoData) {
  ## Import Data ----
  importedDat <- importDataServer("importData")

  fileImport <- reactiveVal(NULL)
  observe({
    # reset model
    Model(NULL)
    if (length(importedDat()) == 0 ||  is.null(importedDat()[[1]])) fileImport(NULL)

    req(length(importedDat()) > 0, !is.null(importedDat()[[1]]))
    data <- importedDat()[[1]]
    valid <- validateImport(data, showModal = TRUE)

    if (!valid){
      showNotification("Import is not valid.")
      fileImport(NULL)
    } else {
      fileImport(data)
    }
  }) %>% bindEvent(importedDat())

  Model <- reactiveVal(NULL)

  data <- reactiveVal()
  observe({
    activeData <- switch(input$dataSource,
                         db = isoData(),
                         file = fileImport()
    )

    # reset model
    Model(NULL)
    data(activeData)
  })

  # MODEL DOWN- / UPLOAD ----
  uploadedData <- downUploadButtonServer(
    "downUpload",
    dat = data,
    inputs = input,
    model = reactive(list(currentModel = Model(), savedMaps = savedMaps())),
    rPackageName = "MpiIsoApp",
    githubRepo = "iso-app",
    subFolder = "AssignR",
    helpHTML = getHelp(id = "assign"),
    modelNotes = reactive(input$modelNotes),
    compressionLevel = 1)

  observe(priority = 100, {
    # reset model
    Model(NULL)
    ## update data ----
    data(uploadedData$data)
  }) %>%
    bindEvent(uploadedData$data)

  observe(priority = 50, {
    ## reset input of model notes
    updateTextAreaInput(session, "modelNotes", value = "")

    ## update inputs ----
    inputIDs <- names(uploadedData$inputs)
    inputIDs <- inputIDs[inputIDs %in% names(input)]

    for (i in 1:length(inputIDs)) {
      session$sendInputMessage(inputIDs[i],  list(value = uploadedData$inputs[[inputIDs[i]]]) )
    }
  }) %>%
    bindEvent(uploadedData$inputs)

  observe(priority = 10, {
    ## update model ----
    Model(unpackModel(uploadedData$model))

    uploadedSavedMaps <- unpackSavedMaps(uploadedData$model, currentSavedMaps = savedMaps())
    savedMaps(c(savedMaps(), uploadedSavedMaps))
  }) %>%
    bindEvent(uploadedData$model)

  # RUN MODEL ----
  observeEvent(input$start, {
    Model(NULL)

    data <- data()
    if (!is.null(data) && (notEmpty(input$catVars) || notEmpty(input$numVars))) {
      dataAssignR <- data[, c(input$IndependentX, input$numVars, input$catVars), drop = F]

      dataAssignR <- dataAssignR %>%
        addUncertainty(type = "numeric", data = data, vars = input$numVars, varsUnc = input$numVarsUnc)
      if (is.null(dataAssignR)) return(NULL)

      dataAssignR <- dataAssignR %>%
        addUncertainty(type = "categorical", data = data, vars = input$catVars, varsUnc = input$catVarsUnc)
      if (is.null(dataAssignR)) return(NULL)

      if(input$imputeMissings & any(is.na(dataAssignR))){
        dataAssignR <- dataAssignR %>%
          imputeMissingValues(numVars = input$numVars, catVars = input$catVars,
                              numVarsUnc = input$numVarsUnc, catVarsUnc = input$catVarsUnc)
      } else {
        dataAssignR <- dataAssignR %>%
          na.omit()
      }

      dataAssignR[, input$catVars] <- trimws(dataAssignR[, input$catVars])
      if (is.null(input$IndependentX) || (is.null(input$numVars) && is.null(input$catVars))) {
        alert("Please specify dependent and at least one numeric or categorical variable")
        return(NULL)
      }
      y <- trimws(dataAssignR[, input$IndependentX])
      cats <- sort(unique(y))
      value <- 0
      models <- lapply(cats, function(x) {
        modelCat <- x
        XNUM <- dataAssignR[, input$numVars, drop = F]
        if (!is.null(input$numVarsUnc) && input$numVarsUnc != "") {
          xUncNUM <- dataAssignR[, input$numVarsUnc, drop = F]
        } else {
          xUncNUM <- NULL
        }
        XCAT <- dataAssignR[, input$catVars, drop = FALSE]
        XCAT <- XCAT[, sapply(XCAT, function(y) length(unique(y))) > 1, drop = FALSE]
        if(NCOL(XCAT) > 0){
          XCAT <- model.matrix(as.formula(paste0("~ ", paste(input$catVars, collapse = "+"), " - 1")), data = dataAssignR)
          if (!is.null(input$catVarsUnc) && input$catVarsUnc != "") {
            XCAT <- lapply(1:length(input$catVars), function(z) model.matrix(as.formula(paste0("~ ", z, " - 1")), data = dataAssignR))
            xUncCAT <- XCAT
            xUncCAT <- lapply(1:length(XCAT), function(z) {
              xUncCAT[[z]][1:nrow(xUncCAT[[z]]), ] <- dataAssignR[, input$numVarsUnc[z], drop = F]
            })
            #XCAT <- do.call("cbind", XCAT)
            xUncCAT <- do.call("cbind", xUncCAT)
          } else {
            xUncCAT <- NULL
          }
        } else {
          XCAT <- NULL
          xUncCAT <- NULL
        }

        yCat <- as.numeric(dataAssignR[, input$IndependentX] == modelCat)
        model <- modelAssignRMC(
          XNUM = XNUM, XCAT = XCAT, y = yCat, xUncCAT = xUncCAT, xUncNUM = xUncNUM, iter = input$Iter, burnin = input$burnin,
          nChains = input$nChains, thinning = input$thinning, cat = modelCat
        )
        value <- match(modelCat, cats) / (length(cats))
        model
      })
      names(models) <- cats
      X <- lapply(cats, function(x) {
        XNUM <- dataAssignR[, input$numVars, drop = F]

        XCAT <- dataAssignR[, input$catVars, drop = FALSE]
        XCAT <- XCAT[, sapply(XCAT, function(x) length(unique(x))) > 1, drop = FALSE]
        if(NCOL(XCAT) > 0){
        XCAT <- model.matrix(as.formula(paste0("~ ", paste(input$catVars, collapse = "+"), " - 1")), data = dataAssignR)
        } else {
          XCAT <- NULL
        }
        if(NCOL(XNUM) > 0){
          if(!is.null(XCAT)){
            X <- as.matrix(cbind(rep(1, length(y)), (XNUM - matrix(models[[x]]$mRe, nrow = nrow(XNUM), ncol = ncol(XNUM), byrow = TRUE)) /
                                   matrix(models[[x]]$sRe, nrow = nrow(XNUM), ncol = ncol(XNUM), byrow = TRUE), XCAT))
          } else {
            X <- as.matrix(cbind(rep(1, length(y)), (XNUM - matrix(models[[x]]$mRe, nrow = nrow(XNUM), ncol = ncol(XNUM), byrow = TRUE)) /
                                   matrix(models[[x]]$sRe, nrow = nrow(XNUM), ncol = ncol(XNUM), byrow = TRUE)))
          }
        } else {
          if(!is.null(XCAT)){
            X <- as.matrix(cbind(rep(1, length(y)), XCAT))

          } else {
            X <- as.matrix(cbind(rep(1, length(y))))

          }
        }
        X
      })
      names(X) <- cats
      predictions <- lapply(cats, function(x) {
        preds <- (resp(sapply(1:nrow(models[[x]]$beta), function(y) X[[x]] %*% models[[x]]$beta[y, ])))
        preds
      })

      predictions <- normalizePredictions(predictions)
      names(predictions) <- cats

      Model(list(models = models, predictions = predictions, data = dataAssignR, X = X))
    }
  })

  observe({
    validate(validInput(Model()))
  })

  predNew <- reactive({
    validate(validInput(Model()))
    req(length(importedData()) > 0)
    dataPred <- importedData()[[1]]
    if ((!is.null(input$numVars) && input$numVars %in% names(dataPred)) || (!is.null(input$catVars) && input$catVars %in% names(dataPred))) {
      dataPred <- dataPred[, c(input$catVars, input$numVars), drop = FALSE]
      dataPred[, input$catVars] <- trimws(dataPred[, input$catVars])
      dataPred <- na.omit(dataPred)
      models <- Model()$models
      cats <- names(models)
      if (!is.null(input$catVars)) {
        for (i in length(input$catVars)) {
          dataPred[, input$catVars[i]] <- as.factor(dataPred[, input$catVars[i]])
          levels(dataPred[, input$catVars[i]]) <- unique(c(levels(dataPred[, input$catVars[i]]), unique(Model()$data[, input$catVars[i]])))
        }
      }
      X <- lapply(cats, function(x) {
        XNUM <- dataPred[, input$numVars, drop = F]

        XCAT <- dataPred[, input$catVars, drop = FALSE]
        XCAT <- XCAT[, sapply(XCAT, function(x) length(unique(x))) > 1, drop = FALSE]
        if(NCOL(XCAT) > 0){
          XCAT <- model.matrix(as.formula(paste0("~ ", paste(input$catVars, collapse = "+"), " - 1")), data = dataPred)
        } else {
          XCAT <- NULL
        }
        if(NCOL(XNUM) > 0){
          if(!is.null(XCAT)){
            X <- as.matrix(cbind(rep(1, NROW(dataPred)), (XNUM - matrix(models[[x]]$mRe, nrow = nrow(XNUM), ncol = ncol(XNUM), byrow = TRUE)) /
                                   matrix(models[[x]]$sRe, nrow = nrow(XNUM), ncol = ncol(XNUM), byrow = TRUE), XCAT))
          } else {
            X <- as.matrix(cbind(rep(1, NROW(dataPred)), (XNUM - matrix(models[[x]]$mRe, nrow = nrow(XNUM), ncol = ncol(XNUM), byrow = TRUE)) /
                                   matrix(models[[x]]$sRe, nrow = nrow(XNUM), ncol = ncol(XNUM), byrow = TRUE)))
          }
        } else {
          if(!is.null(XCAT)){
            X <- as.matrix(cbind(rep(1, NROW(dataPred)), XCAT))

          } else {
            X <- as.matrix(cbind(rep(1, NROW(dataPred))))

          }
        }
        X
      })
      names(X) <- cats
      predictions <- lapply(cats, function(x) {
        preds <- (resp(sapply(1:nrow(models[[x]]$beta), function(y) X[[x]] %*% models[[x]]$beta[y, ])))
        preds
      })
      predictions <- normalizePredictions(predictions)
      names(predictions) <- cats
    } else {
      predictions <- NULL
      X <- NULL
    }
    list(predictions = predictions, data = dataPred, X = X)
  })

  output$dataTable <- DT::renderDataTable({
    validate(validInput(Model()))
    estimate <- dataFun()()
    datTable(estimate, columns = colnames(estimate))
  },
  options = list(scrollX = TRUE))

  dataFun <- reactive({
    validate(validInput(Model()))
    function() {
      if (input$predType == "2" && length(importedData()) > 0 && !is.null(predNew())) {
        predictions <- predNew()$predictions
        data <- predNew()$data
        p <- ncol(Model()$X[[1]]) - 1
        n <- nrow(Model()$X[[1]])

        if (input$other) {
          md <- mahalanobis(
            predNew()$X[[1]][, -1, drop = F],
            colMeans(Model()$X[[1]][, -1, drop = F]),
            cov(Model()$X[[1]][, -1, drop = F]) +
              diag(1E-4,
                   nrow = p,
                   ncol = p
              )
          )

          outsideProb <- pf((n * md * (n - p)) / (p * (n - 1) * (n + 1)), df1 = p, df2 = n - p)
          pr1 <- input$prior1
          pr2 <- input$prior2
          otherPred <- sapply(1:ncol(predictions[[1]]), function(y) {
            samp <- rbeta(1, pr1, pr2)
            1 - ((1 - samp) * (1 - outsideProb)) / ((1 - samp) * (1 - outsideProb) + samp * outsideProb)
          })

          predictions <- lapply(1:length(predictions), function(x) {
            predictions[[x]] / (1 / (1.0001 - (Reduce("+", c(predictions, list(otherLocation = otherPred))) - 1)))
          })
          names(predictions) <- names(predNew()$predictions)
          predictions <- c(predictions, list(otherLocation = otherPred))
        }

        if (!is.null(input$catAgg) && input$aggType == "cat" && input$catAgg != "") {
          data1 <- importedData()[[1]][, c(input$catAgg), drop = F]
        }
      } else {
        predictions <- Model()$predictions
        data <- Model()$data
        if (!is.null(input$catAgg) && input$catAgg != "" && input$aggType == "cat") {
          data1 <- data()[, c(input$catAgg), drop = F]
        }
      }
      if (input$estType == "mean") {
        estimate <- as.data.frame(round(do.call("cbind", lapply(predictions, rowMeans)), 3))
      }
      if (input$estType == "sd") {
        estimate <- as.data.frame(round(do.call("cbind", lapply(predictions, function(z) apply(z, 1, sd))), 3))
      }
      if (input$estType == "quantile") {
        estimate <- as.data.frame(round(do.call("cbind", lapply(predictions, function(z) apply(z, 1, quantile, input$quantile))), 3))
      }
      if (input$aggType == "cat") {
        if(length(unique(data[, input$catAgg])) < 2){
          stop("At least two different categories needed in category variable!")
        }
        if (!is.null(input$catAgg) && input$catAgg != "") {
          data2 <- data
          data1$rnames <- rownames(data1)
          data2$rnames <- rownames(data2)
          data2[, input$catAgg] <- NULL
          data2 <- merge(x = data2, y = data1, by = "rnames", all.x = TRUE)[, input$catAgg, drop = FALSE]
          data2 <- trimws(unlist(data2))
          estimate <- sapply(predictions, function(x) {
            estimate <- x
            estimate <- data.frame(estimate, data2)
            estimateSplit <- split(estimate, estimate[, NCOL(estimate)])
            estimate <- sapply(estimateSplit, function(y) {
              y <- y[, -NCOL(y), drop = F]
              if (input$estType == "mean") {
                estimate <- mean(exp(rowMeans(log(y))))
              }
              if (input$estType == "sd") {
                estimate <- sd(exp(rowMeans(log(y))))
              }
              if (input$estType == "quantile") {
                estimate <- quantile(exp(rowMeans(log(y))), input$quantile)
              }
              estimate
            })
          })
          if (input$estType == "mean") {
            estimate <- estimate / rowSums(estimate)
          }
          estimate <- data.frame(category = rownames(estimate), (round(estimate, 3)))
        }
      }
      if (input$aggType == "all") {
        estimate <- sapply(predictions, function(x) {
          if (input$estType == "mean") {
            estimate <- mean(exp(rowMeans(log(x))))
          }
          if (input$estType == "sd") {
            estimate <- sd(exp(rowMeans(log(x))))
          }
          if (input$estType == "quantile") {
            estimate <- quantile(exp(rowMeans(log(x))), input$quantile)
          }
          estimate
        })
        if (input$estType == "mean") {
          estimate <- estimate / sum(estimate)
        }
        estimate <- round(estimate, 3)
        estimate <- as.data.frame(estimate %>% t())
      }
      if (input$showData & input$aggType == "single") {
        estimate <- cbind(estimate, data)
      }
      return(estimate)
    }
  })

  ## Import Prediction Data ----
  importedData <- importDataServer("localData")

  callModule(modelDiagnostics, "modelDiag", model = Model, choice = TRUE)
  callModule(dataExport, "exportData", data = dataFun, filename = "modelData")

  observe(priority = 75, {
    allVars <- names(data())
    updateSelectInput(session, "IndependentX",  choices = c("", allVars))
    updateSelectInput(session, "numVars", choices = c("", allVars))
    updateSelectInput(session, "catVars", choices = c("", allVars))
    updateSelectInput(session, "catAgg", choices = c("", allVars))
    updateSelectInput(session, "numVarsUnc", choices = c("", allVars))
    updateSelectInput(session, "catVarsUnc", choices = c("", allVars))
  }) %>%
    bindEvent(data())
}

## Helper functions ----

addUncertainty <- function(dataAssignR, type, data, vars, varsUnc) {
  if (notEmpty(varsUnc)){
    if (length(varsUnc) == length(vars)){
      dataAssignR <- cbind(dataAssignR, data[, c(varsUnc), drop = F])
    } else {
      alert(sprintf("Number of %s uncertainty variables must equal %s variables", type))
      dataAssignR <- NULL
    }
  }

  dataAssignR
}

imputeMissingValues <- function(dataAssignR, numVars, catVars, numVarsUnc, catVarsUnc) {
  if(notEmpty(catVars)){
    for(i in catVars){
      if(class(dataAssignR[, i]) == "character"){
        dataAssignR[, i] <- factor(dataAssignR[, i])
      }
    }
  }
  imputed_Data <- mice(dataAssignR, m=10, maxit = 50, seed = 500, printFlag = FALSE)
  completed <- complete(imputed_Data, "all")
  new_data <- dataAssignR

  if(notEmpty(numVars)){
    for (i in 1:length(numVars)){
      new_data[, numVars[i]] = rowMeans(sapply(1:length(completed), function(x) completed[[x]][,numVars[i]]))
    }
  }
  if(notEmpty(numVarsUnc)){
    new_data[, numVarsUnc][is.na(new_data[, numVarsUnc])] <- 0
    for (i in 1:length(numVarsUnc)){
      new_data[, numVarsUnc[i]] = new_data[, numVarsUnc[i]] + apply(sapply(1:length(completed), function(x) completed[[x]][,numVars[i]]),1,sd)
    }
  }

  if(notEmpty(catVars)){
    for (j in 1:length(catVars)){
      new_data[, catVars[j]] <- apply(sapply(1:length(completed), function(x) completed[[x]][,catVars[j]]), 1, getMode)
    }
  }
  if(notEmpty(catVarsUnc)){
    new_data[, catVarsUnc][is.na(new_data[, catVarsUnc])] <- 0
    for (j in 1:length(catVarsUnc)){
      new_data[, catVarsUnc[j]] <- new_data[, catVarsUnc[i]] + rowMeans(sapply(1:length(completed), function(x) completed[[x]][,catVarsUnc[i]]))
    }
  }

  new_data
}

notEmpty <- function(inputColumns) {
  !is.null(inputColumns) && length(inputColumns) > 0 && all(inputColumns != "")
}

getMode <- function(x){
  names(sort(-table(x)))[1]
}
