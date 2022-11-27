compTableModule <- function(id, included) {
    moduleServer(
        id,
        function(input, output, session) {
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
            
            # Join tables and identify matches ----------------------------------------
            
            # #Make dynamically updating UI for picking the column to be matched on
            # output$matchcolumn <- renderUI({ 
            #     selectInput("matchcolumn", label = "Match on", intersect(names(v$lefttable), names(v$righttable)),
            #                 NULL)
            # })
            
            thistable_joined <- reactive({
                # validate(need(length(intersect(names(v$lefttable), names(v$righttable)))>0 | is.null(v$lefttable) | is.null(v$righttable), "Tables need at least one matching column"))
                # validate(need(input$matchcolumn %in% names(v$lefttable) | is.null(v$lefttable) | is.null(v$righttable) , "Loading"))
                # 
                # if (!is.null(v$lefttable) & !is.null(v$righttable)) {
                #     v$lefttable |> 
                #         dplyr::mutate(match=ifelse(tolower(!!dplyr::sym(input$matchcolumn)) %in% tolower(v$righttable[[input$matchcolumn]]),
                #                                    "yes",
                #                                    "no"))
                # } else {v$lefttable}
                v$thistable
            })
            
            # Pick which columns should be displayed  -------------------
            
            #Make dynamically updating UI for picking the columns to be displayed
            output$selectUI <- renderUI({ 
                selectInput("selectUI", label = "Display", names(thistable_joined()), multiple = TRUE)
            })
            
            #Display all columns if nothing is selected
            displaycolumns <- reactive({
                if (!is.null(input$selectUI)) {
                    input$selectUI
                } else { names(thistable_joined())}
            })
            
            # Render the tables -------------------------------------------------------
            
            dtoptions2 <- list(pageLength = 20, scrollX = TRUE)
            
            output$compTable <- DT::renderDataTable({ #Need to use DT::renderDataTable, not from shiny::renderDataTable, when rendering DT::datatable()
                temp <- DT::datatable(thistable_joined()[,displaycolumns(), drop=FALSE], options = dtoptions2) 
                
                # if (!is.null(v$righttable) & !is.null(v$lefttable) & ("match" %in% colnames(lefttable_joined()[,displaycolumns_left(), drop=FALSE]))) {
                #     temp |> DT::formatStyle("match", target = "row", backgroundColor = DT::styleEqual(c("yes", "no"), c("LightGreen","LightCoral")), "white-space"="nowrap")
                # } else { temp}
                temp
            })
            
        }
    )
}

compTableModuleUI <- function(id) {
    ns <- shiny::NS(id)
    tagList(
        column(6,
               fluidRow(
                   column(3, style = "margin-top: 25px;",
                          fileInput(ns("import_codelist"), label=NULL)),
                   column(3, style = "margin-top: 25px;",
                          actionButton(ns("get_codelist"), "from codelist maker")),
                   column(3,
                          htmlOutput(ns("selectUI"))),
                   # column(3,
                   #        htmlOutput("matchcolumn")),
                   
               ),
               DT::dataTableOutput(ns("compTable"))
        )
    )
}
