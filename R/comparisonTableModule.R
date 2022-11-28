# For this to be modularised it probably needs to look something like this:
#https://shiny.rstudio.com/articles/communicate-bet-modules.html


loadTableModule <- function(id, included) {
    moduleServer(id,function(input, output, session) {
            # Join tables and identify matches ----------------------------------------
            
            # Set values for left and right tables ------------------------------------
            
            # Make a reactiveValues to store the data; downstream functions will use whatever is stored in here ("duelling values", see https://stackoverflow.com/questions/29716868/r-shiny-how-to-get-an-reactive-data-frame-updated-each-time-pressing-an-actionb)
            v <- reactiveValues(thistable = NULL)
            
            #Either get codelist from codelist maker ...
            observeEvent(input$get_codelist, {
                v$thistable <- included()
            })
            
            #... or import from file.
            observeEvent(input$import_codelist, {
                inFile <- input$import_codelist
                if (is.null(inFile))
                    return(NULL)
                v$thistable <- rio::import(inFile$datapath)
            })
            return(reactive(v$thistable))
            
        })
}

matchcolTableModule <- function(id, lefttable, righttable) {
    moduleServer(id, function(input, output, session) {
            #Make dynamically updating UI for picking the column to be matched on
            output$matchcolumn <- renderUI({
                selectInput("matchcolumn", label = "Match on", intersect(names(lefttable()), names(righttable())),
                            NULL)
            })
        })}


joinRenderTableModule <- function(id, thistable, othertable) {
    moduleServer(id, function(input, output, session) {
            # Join tables and identify matches ----------------------------------------
            
            thistable_joined <- reactive({
                validate(need(length(intersect(names(thistable()), names(othertable())))>0 | is.null(thistable()) | is.null(othertable()), "Tables need at least one matching column"))
                #validate(need(input$matchcolumn %in% names(thistable()) | is.null(thistable()) | is.null(othertable()) , "Loading"))
                
                if (!is.null(thistable()) & !is.null(othertable())) {
                    thistable() |>
                        dplyr::mutate(match=ifelse(tolower(!!dplyr::sym(input$matchcolumn)) %in% tolower(othertable()[[input$matchcolumn]]),
                                                   "yes",
                                                   "no"))
                } else {thistable()}
            })
            
            # Pick which columns should be displayed  -------------------
            
            # #Make dynamically updating UI for picking the columns to be displayed
            # output$selectUI <- renderUI({ 
            #     selectInput("selectUI", label = "Display", names(thistable_joined()), multiple = TRUE)
            # })
            # 
            # #Display all columns if nothing is selected
            # displaycolumns <- reactive({
            #     if (!is.null(input$selectUI)) {
            #         input$selectUI
            #     } else { names(thistable_joined())}
            # })
            # Render the tables -------------------------------------------------------
            
            dtoptions2 <- list(pageLength = 20, scrollX = TRUE)
            
            output$compTable <- DT::renderDataTable({ #Need to use DT::renderDataTable, not from shiny::renderDataTable, when rendering DT::datatable()
                temp <- DT::datatable(thistable_joined(), options = dtoptions2) 
                
                if (!is.null(othertable()) & !is.null(thistable()) & ("match" %in% colnames(thistable_joined()))) {
                    temp |> DT::formatStyle("match", target = "row", backgroundColor = DT::styleEqual(c("yes", "no"), c("LightGreen","LightCoral")), "white-space"="nowrap")
                } else { temp}
            })
            
        }
    )
}

loadTableModuleUI <- function(id) {
    ns <- shiny::NS(id)
    tagList(
        fluidRow(
            column(3, style = "margin-top: 25px;",
                   fileInput(ns("import_codelist"), label=NULL)),
            column(3, style = "margin-top: 25px;",
                   actionButton(ns("get_codelist"), "from codelist maker"))
        )
    )}

matchcolModuleUI <- function(id) {
    ns <- shiny::NS(id)
    htmlOutput(ns("matchcolumn"))
    }


joinRenderTableModuleUI <- function(id) {
    ns <- shiny::NS(id)
    tagList(
        DT::dataTableOutput(ns("compTable"))
    )}

