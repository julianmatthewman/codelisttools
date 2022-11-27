compTableModule <- function(id, thistable, othertable, matchcolumn) {
    moduleServer(
        id,
        function(input, output, session) {
            # Join tables and identify matches ----------------------------------------
            
            table_joined <- reactive({
                validate(need(length(intersect(names(thistable()), names(othertable())))>0 | is.null(thistable()) | is.null(othertable()), "Tables need at least one matching column"))
                validate(need(matchcolumn() %in% names(thistable()) | is.null(thistable()) | is.null(othertable()) , "Loading"))
                
                if (!is.null(thistable()) & !is.null(othertable())) {
                    thistable() |> 
                        dplyr::mutate(match=ifelse(tolower(!!dplyr::sym(matchcolumn())) %in% tolower(othertable()[[matchcolumn()]]),
                                                   "yes",
                                                   "no"))
                } else {thistable()}
            })
            
            #Display all columns if nothing is selected
            displaycolumns <- reactive({
                names(table_joined())
            })
            
            
            output$compTable <- reactive({ #Need to use DT::renderDataTable, not from shiny::renderDataTable, when rendering DT::datatable()
                temp <- DT::datatable(table_joined()[,displaycolumns(), drop=FALSE], 
                                      options = list(pageLength = 20, scrollX = TRUE)) 
                
                if (!is.null(thistable()) & !is.null(othertable()) & ("match" %in% colnames(table_joined()[,displaycolumns(), drop=FALSE]))) {
                    temp |> DT::formatStyle(
                        columns = "match",
                        target = "row",
                        backgroundColor = DT::styleEqual(c("yes", "no"), c("LightGreen","LightCoral")),
                        "white-space"="nowrap"
                    )
                } else {temp}
            })
        }
    )
}

compTableModuleUI <- function(id) {
    ns <- shiny::NS(id)
    tagList(
        DT::dataTableOutput(ns("compTable"))
    )
}
