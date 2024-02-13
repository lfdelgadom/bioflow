#' example_module UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_example_module_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$h1('Demo Template!'),
    actionButton(ns('show'), label = "Show Data"),
    DT::DTOutput(ns('preview')),
    selectInput(ns('trait'), label = "Trait:", choices = list()),
    actionButton(ns('calc'), label = "Calculate the Mean!")
  )
}

#' example_module Server Functions
#'
#' @noRd
mod_example_module_server <- function(id, data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    ts <- as.numeric(Sys.time())

    observeEvent(input$show, {
      obj <- data()
      df  <- obj$data$pheno

      output$preview <- DT::renderDT({
        DT::datatable(df, options = list(scrollX = TRUE))
      })

      updateSelectInput(session, inputId = 'trait', choices = colnames(df))
    })

    observeEvent(input$calc, {
      obj <- data()
      df  <- obj$data$pheno

      x   <- df[, input$trait] + 1
      avg <- mean(x, na.rm = TRUE)
      se  <- sd(x, na.rm = TRUE) / sqrt(length(x))
      msg <- paste('Mean value for', input$trait, 'is', avg, 'se =', se)

      shinyWidgets::show_alert(title = 'Results', type = 'info', text = msg)

      result <- data.frame(module = 'demo',
                           analysisId = ts,
                           trait = input$trait,
                           environment = 'all',
                           parameter = 'mean',
                           method = 'sd/sqrt(n)',
                           value = avg,
                           stdError = se)

      obj$metrics <- rbind(obj$metrics, result)
      obj$status  <- rbind(obj$status, data.frame(module = 'demo', analysisId = ts))

      data(obj)
    })

  })
}

## To be copied in the UI
# mod_example_module_ui("example_module_1")

## To be copied in the server
# mod_example_module_server("example_module_1")
