#' The Codelist Tools Shiny App
#'
#' @import shiny
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
library(shiny)

myApp <- function(...) {
    
    # This shiny app is managed as an R package as described here:
    # https://mastering-shiny.org/scaling-packaging.html#deploying-your-app-package
    # In RStudio press Cmd/Ctrl + Shift + L to run devtools::load_all(), then run the app with myApp()
    # To deploy call rsconnect::deployApp()
    
    
    #Import all browsers in the "in" folder
    paths <- dir("in", full.names = TRUE)
    browsers <- purrr::map(paths, \(x) rio::import(x, colClasses=c("character"))) |> 
        purrr::set_names(basename(tools::file_path_sans_ext(paths)))
    
    #product <- rio::import("/Users/Julian/Documents/GitHub/2021_SkinEpiExtract/codelists/product.dta")
    
    # UI ----------------------------------------------------------------------
    
    ui <- fluidPage(
        
        navbarPage("Codelist tools",
                   
                   tabPanel("Codelist Maker",
                            # Sidebar with inputs
                            sidebarLayout(
                                sidebarPanel(
                                    width=3,
                                    tags$style(type='text/css', '#searchterms {white-space: pre-wrap;}'),
                                    tags$style(type='text/css', '#exclusionterms {white-space: pre-wrap;}'),
                                    tags$style(type='text/css', '#cols {white-space: pre-wrap;}'),
                                    
                                    selectInput("select_codebrowser",
                                                "Select browser",
                                                names(browsers),
                                                "ICD10_Edition5_GB_20160401"),
                                    fileInput("import_codebrowser", label=NULL),
                                    
                                    hr(),
                                    
                                    textAreaInput("searchterms",
                                                  "Searchterms",
                                                  "diabetes",
                                                  resize = "vertical"),
                                    textAreaInput("exclusionterms",
                                                  "Exclusionterms",
                                                  "insipidus",
                                                  resize = "vertical"),
                                    htmlOutput("select_search_cols"),
                                    checkboxInput("termset_search_method",
                                                  label = tags$span(
                                                      "Termset search method", 
                                                      tags$i(
                                                          class = "glyphicon glyphicon-info-sign", 
                                                          style = "color:#0072B2;",
                                                          title = 'For search rules see "About" Tab'))),
                                    verbatimTextOutput("randomstrings"),
                                    
                                    hr(),
                                    
                                    downloadButton("downloadData", "Download"),
                                    hr(),
                                    
                                    htmlOutput("select_display_cols"),
                                    
                                    
                                    
                                ),
                                
                                # Main panel with outputs
                                mainPanel(
                                    width = 9,
                                    tags$head(tags$style("#termsearched  {white-space: nowrap;  }"),
                                              tags$style("#excluded  {white-space: nowrap;  }"),
                                              tags$style("#included  {white-space: nowrap;  }"),
                                              tags$style("#descendants  {white-space: nowrap;  }"),
                                              tags$style(HTML("#withborder  {border: 4px solid black;}"))),
                                    
                                    
                                    fluidRow(id="withborder",
                                             h4("Initial codelist"),
                                             DT::dataTableOutput("termsearched"),
                                    ),
                                    fluidRow(id="withborder",
                                             h4("Excluded"),
                                             DT::dataTableOutput("excluded"),
                                    ),
                                    fluidRow(id="withborder",
                                             h4("Final codelist"),
                                             DT::dataTableOutput("included"),
                                    ),
                                    
                                    hr(),
                                    fluidRow(id="withborder", h4("Checks")),
                                    fluidRow(id="withborder",
                                             column(4, h4("Unmatched descendants")),
                                             column(4, checkboxInput("descendant_matching", "Enable descendant searching, on column:")),
                                             column(4, style = "margin-top: 5px;", htmlOutput("select_code_cols")),
                                             DT::dataTableOutput("descendants"),
                                    ),
                                    fluidRow(id="withborder",
                                             column(4, h4("Cross-tabulation")),
                                             column(4, checkboxInput("crosstab", "Enable cross-tabulation, on column:")),
                                             column(4, style = "margin-top: 5px;", htmlOutput("select_check_cols")),
                                             tableOutput("checks"),
                                    ),
                                )
                            )
                   ),
                   tabPanel("Codelist Comparison", 
                            fluidRow(
                                column(6,
                                       fluidRow(id="withborder", 
                                           column(9, loadTableModuleUI("left")),
                                           column(3, htmlOutput("matchcolumn"), style = "margin-bottom: -25px;")
                                       ),
                                       fluidRow(id="withborder", joinRenderTableModuleUI("left"))
                                ),
                                column(6,
                                       fluidRow(id="withborder", loadTableModuleUI("right")),
                                       fluidRow(id="withborder", joinRenderTableModuleUI("right"))
                                )
                            )
                   ),
                   tabPanel("About",
                            fluidRow(
                                column(6,
                                       htmltools::includeMarkdown("docs/codelist_maker_README.md")),
                                column(6,
                                       htmltools::includeMarkdown("docs/codelist_comparison_README.md"))
                            )
                   )
        )
    )
    
    
    
    # SERVER ------------------------------------------------------------------
    
    server <- function(input, output) {
        options(shiny.maxRequestSize=100*1024^2)
        
        #//////////////////////////////////////////////////////////////////////////    
        # 1. CODELIST MAKER -------------------------------------------------------
        #//////////////////////////////////////////////////////////////////////////  
        
        
        # Select browser ----------------------------------------------------------
        
        # Make a reactiveValues to store the data; downstream functions will use whatever is stored in here ("duelling values", see https://stackoverflow.com/questions/29716868/r-shiny-how-to-get-an-reactive-data-frame-updated-each-time-pressing-an-actionb)
        codebrowser <- reactiveValues(data = NULL)
        
        #Either select built in browser ...
        observeEvent(input$select_codebrowser, {
            codebrowser$data <- browsers[[input$select_codebrowser]]
        })
        
        #... or import from file.
        observeEvent(input$import_codebrowser, {
            inFile <- input$import_codebrowser
            if (is.null(inFile))
                return(NULL)
            codebrowser$data <- rio::import(inFile$datapath, colClasses=c("character"))
        })
        
        
        # Make dynamic UIs to pick columns ----------------------------------------
        
        
        #Make dynamically updating UI for picking the columns to search in
        output$select_search_cols <- renderUI({ 
            selectInput("cols", "Select column to search in", names(codebrowser$data),
                        ifelse("DESCRIPTION" %in% names(codebrowser$data), "DESCRIPTION", 
                               ifelse("productname" %in% names(codebrowser$data), "productname", 
                                      ifelse("readterm" %in% names(codebrowser$data), "readterm", names(codebrowser$data)[[1]]))),
                        multiple = FALSE)
        })
        
        #Make dynamically updating UI for picking the columns to display
        output$select_display_cols <- renderUI({ 
            selectInput("displaycolumns", "Select columns to display", names(codebrowser$data), multiple = TRUE)
        })
        
        #Make dynamically updating UI for picking the column to check
        output$select_check_cols <- renderUI({ 
            selectInput("checkcol", label=NULL, names(codebrowser$data),
                        ifelse("USAGE" %in% names(codebrowser$data), "USAGE", names(codebrowser$data)[[1]]))
        })
        
        #Make dynamically updating UI for picking the column to match for descendants
        output$select_code_cols <- renderUI({ 
            selectInput("codecol", label=NULL, names(codebrowser$data),
                        ifelse("CODE" %in% names(codebrowser$data), "CODE", names(codebrowser$data)[[1]]))
        })
        
        
        # Get values from input ---------------------------------------------------
        
        termset_search_method <- reactive(input$termset_search_method)
        descendant_matching <- reactive(input$descendant_matching)
        crosstab <- reactive(input$crosstab)
        
        #Make vectors from the inputs
        searchterms <- reactive(unlist(strsplit(input$searchterms,"\n")))  |> debounce(2000)
        exclusionterms <- reactive(unlist(strsplit(input$exclusionterms,"\n")))  |> debounce(2000)
        checkcol <- reactive(input$checkcol)
        codecol <- reactive(input$codecol)
        
        cols <- reactive(input$cols)
        
        displaycolumns <- reactive({
            if (!is.null(input$displaycolumns)) {
                input$displaycolumns
            } else { names(codebrowser$data)}
        })
        
        
        # Make the Tables ---------------------------------------------------------
        
        termsearched <- reactive({
            validate(need(cols() %in% names(codebrowser$data), "Loading")) # need to validate to avoid flashing error message, see: https://stackoverflow.com/questions/52378000/temporary-shiny-loading-error-filter-impl
                codebrowser$data |> 
                dplyr::filter(termsearch(eval(dplyr::sym(cols())), searchterms(), termset_search_method()))
        })
        
        excluded <- reactive({
                termsearched() |> 
                dplyr::filter(termsearch(eval(dplyr::sym(cols())), exclusionterms(), termset_search_method()) &
                                  !(tolower(eval(dplyr::sym(cols()))) %in% tolower(searchterms()))) # This is so exact matches are never excluded. The term [heart failure] always matches "Heart failure" even if [heart] were excluded.
        })
        
        included <- reactive({
            dplyr::setdiff(termsearched(), excluded())
        })
        
        checks <- reactive({
            validate(need(crosstab() == TRUE, message = FALSE)) 
            included() |> 
                dplyr::group_by(!!! dplyr::syms(checkcol())) |> 
                dplyr::tally() |> 
                dplyr::arrange(desc(n))
        })
        
        descendants <- reactive({
            validate(
                need(codecol() %in% names(codebrowser$data), "Loading"),
                need(codecol() %in% names(included()), "Loading"), # need to validate to avoid flashing error message
                need(searchterms(), "No searchterms provided"),
                need(nrow(included())>0, "Nothing included"),
                need(descendant_matching() == TRUE, message = FALSE)
            )
            temp <- dplyr::filter(codebrowser$data, stringr::str_starts(eval(dplyr::sym(codecol())), paste(included()[[codecol()]], collapse = "|"))) 
            if(length(exclusionterms())>0) temp <- dplyr::filter(temp, !termsearch(eval(dplyr::sym(cols())), exclusionterms(), termset_search_method()))
            dplyr::setdiff(temp, included())
        })
        
        
        # Make extra highlighted Tables -------------------------------------------
        
        #Make a function to highlight text (to be used in str_replace_all)
        highlight_yellow <- function(x) {paste0("<span style='background-color:yellow;'>", x, "</span>")}
        highlight_green <- function(x) {paste0("<span style='background-color:LightGreen;'>", x, "</span>")}
        
        
        
        #Make included table with highlighted words
        termsearched_highlighted <- reactive({
            # Transform searchterms so they are in this form: term1|term2|term3|...
            searchterms_highlightable <- searchterms() |> 
                strsplit(split = " ") |> 
                purrr::map(~paste(.x, collapse = "|")) |> 
                unlist()
            
            termsearched() |>
                dplyr::mutate(
                    dplyr::across(dplyr::any_of(input$cols),
                                  ~ stringr::str_replace_all(.x,
                                                             stringr::regex(paste(searchterms_highlightable, collapse="|"), ignore_case = TRUE),
                                                             highlight_green
                                  )
                    )
                )
        })
        
        #Make excluded table with highlighted words
        excluded_highlighted <- reactive({
            excluded() |>
                dplyr::mutate(
                    dplyr::across(dplyr::any_of(input$cols),
                                  ~ stringr::str_replace_all(.x,
                                                             stringr::regex(paste(exclusionterms(), collapse="|"), ignore_case = TRUE),
                                                             highlight_yellow
                                  )
                    )
                )
        })
        
        
        # Render Text and Tables -------------------------------------------------------
        
        # Set table options    
        dtoptions <- list(pageLength = 5, scrollX = TRUE)
        # Set which tables to display
        
        #Render tables
        output$termsearched <- DT::renderDataTable({ 
            termsearched_highlighted()[,displaycolumns(), drop=FALSE] 
        }, escape = FALSE, options = dtoptions)
        
        output$excluded <- DT::renderDataTable({ 
            excluded_highlighted()[,displaycolumns(), drop=FALSE]
        }, escape = FALSE, options = dtoptions) #Need escape = FALSE if including HTML formatting
        
        output$included <- DT::renderDataTable({ 
            included()[,displaycolumns(), drop=FALSE]
        }, options = dtoptions)
        
        #Render descendants
        output$descendants <- DT::renderDataTable({ 
            descendants()[,displaycolumns(), drop=FALSE]
        }, options = dtoptions)
        
        #Render extra table containing checks
        output$checks <- renderTable(
            checks()
        )
        
        
        
        #Print values
        output$searchterms<-renderPrint({
            cat("Searchterms:\n")
            print(searchterms())
        }
        )
        output$exclusionterms<-renderPrint({
            cat("Exclusionterms:\n")
            print(exclusionterms())
        }
        )
        output$cols<-renderPrint({
            cat("Searched in:\n")
            print(cols())
        }
        )
        
        
        # Downloadable csvs of final codelist and terms --------------------------------------
        
        # Make table of searchterms
        termtable <- reactive({
            searchterms <- searchterms()
            exclusionterms <- exclusionterms()
            searched_in_column <- cols()
            searchmethod <- ifelse(termset_search_method()==TRUE, 
                                   "Term sets search method, see 10.1371/journal.pone.0212291",
                                   "termsearch <- function(lookup, terms) {stringr::str_detect(lookup, stringr::regex(paste(terms, collapse = '|'), ignore_case = TRUE))}; initial <- dplyr::filter(DATA, termsearch(COLUMN, SEARCHTERMS)); excluded <- dplyr::filter(initial, termsearch(COLUMN, EXCLUSIONTERMS); final <- dplyr::setdiff(inital, excluded)")
            n <- max(length(searchterms), length(exclusionterms), length(searched_in_column), length(searchmethod))
            length(searchterms) <- n                      
            length(exclusionterms) <- n   
            length(searched_in_column) <- n  
            length(searchmethod) <- n  
            cbind(searchterms,exclusionterms, searched_in_column, searchmethod)
        })
        
        
        # Make random string to include in filename
        randomstrings <- reactive({termtable() #Make random strings that update whenever either of the tables updates 
            included()
            stringi::stri_rand_strings(1, 6)})
        
        # Zip codelist and terms and provide via download button
        output$downloadData <- downloadHandler(
            filename = function() {paste("codelist","-", randomstrings(), "-", Sys.Date(), ".zip", sep = "")},
            content = function(filename) {
                tmpdir <- tempdir()
                setwd(tempdir())
                print(tempdir())
                
                fs <- c("codelist.csv", "terms.csv", "excluded.csv")
                utils::write.csv(included(), "codelist.csv", row.names=FALSE, quote = FALSE, na = "")
                utils::write.csv(termtable(), "terms.csv", row.names=FALSE, quote = FALSE, na = "")
                utils::write.csv(excluded(), "excluded.csv", row.names=FALSE, quote = FALSE, na = "")
                
                utils::zip(zipfile=filename, files=fs)
            },
            contentType = "application/zip"
        )
        
        
        #//////////////////////////////////////////////////////////////////////////
        # 2. CODELIST COMPARISON ##################################################
        #//////////////////////////////////////////////////////////////////////////

        #Make dynamically updating UI for picking the column to be matched on
        output$matchcolumn <- renderUI({
            selectInput("matchcolumn", label = "Match on", intersect(names(lefttable()), names(righttable())),
                        NULL)
        })
        matchcolumn <- reactive(input$matchcolumn)
        
        #Loading, joining, and rendering of tables is handled via modules
        lefttable <- loadTableModule("left", reactive(included()))
        righttable <- loadTableModule("right", reactive(included()))
        joinRenderTableModule("left", reactive(lefttable()), reactive(righttable()), reactive(matchcolumn()))
        joinRenderTableModule("right", reactive(righttable()), reactive(lefttable()), reactive(matchcolumn()))
        
        
    }
    
    # Run the application 
    shinyApp(ui = ui, server = server)
    
}
