
# Server modules ----------------------------------------------------------

loadTableModule <- function(id, included) {
  moduleServer(id, function(input, output, session) {
    # Set value for table ------------------------------------

    # Make a reactiveValues to store the data; downstream functions will use whatever is stored in here ("duelling values", see https://stackoverflow.com/questions/29716868/r-shiny-how-to-get-an-reactive-data-frame-updated-each-time-pressing-an-actionb)
    v <- reactiveValues(thistable = NULL)

    # Either get codelist from codelist maker ...
    observeEvent(input$get_codelist, {
      v$thistable <- included()
    })

    # ... or import from file.
    observeEvent(input$import_codelist, {
      inFile <- input$import_codelist
      if (is.null(inFile)) {
        return(NULL)
      }
      v$thistable <- rio::import(inFile$datapath)
    })
    return(reactive(v$thistable))
  })
}



joinRenderTableModule <- function(id, thistable, othertable, matchcolumn) {
  moduleServer(id, function(input, output, session) {
    # Join tables and identify matches ----------------------------------------

    thistable_joined <- reactive({
      validate(need(length(intersect(names(thistable()), names(othertable()))) > 0 | is.null(thistable()) | is.null(othertable()), "Tables need at least one matching column"))
      validate(need(matchcolumn() %in% names(thistable()) | is.null(thistable()) | is.null(othertable()), "Loading"))

      if (!is.null(thistable()) & !is.null(othertable())) {
        match <- thistable()[[matchcolumn()]] %in% othertable()[[matchcolumn()]]
        cbind(thistable(), match)
      } else {
        thistable()
      }
    })

    # Render
    output$compTable <- DT::renderDataTable({ # Need to use DT::renderDataTable, not from shiny::renderDataTable, when rendering DT::datatable()
      temp <- DT::datatable(thistable_joined(),
        class = 'nowrap display',
        extensions = "Buttons",
        options = list(pageLength = 20, scrollX = TRUE, dom = "Bfrtip", buttons = I("colvis"))
      )

      if (!is.null(othertable()) & !is.null(thistable()) & ("match" %in% colnames(thistable_joined()))) {
        temp |> DT::formatStyle("match", target = "row", backgroundColor = DT::styleEqual(c(TRUE, FALSE), c("LightGreen", "LightCoral")))
      } else {
        temp
      }
    })
  })
}



# UI modules ----------------------------------------------------------------------

loadTableModuleUI <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    fluidRow(
      column(3,
        style = "margin-top: 25px; margin-bottom: -25px",
        fileInput(ns("import_codelist"), label = NULL)
      ),
      column(3,
        style = "margin-top: 25px; margin-bottom: -25px",
        actionButton(ns("get_codelist"), "from codelist maker")
      )
    )
  )
}

joinRenderTableModuleUI <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    DT::dataTableOutput(ns("compTable"))
  )
}
