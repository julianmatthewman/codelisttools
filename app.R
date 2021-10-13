library(shiny)
library(rio)
library(DT)
library(bslib)
library(haven)
library(tidyverse)


# Source all functions from the "R" folder
sapply(list.files("R", full.names = TRUE) ,source, .GlobalEnv)

#Import all browsers in the "in" folder
paths <- dir("in", full.names = TRUE)
browsers <- map(paths, import) %>% set_names(basename(tools::file_path_sans_ext(paths)))

#product <- import("/Users/Julian/Documents/GitHub/2021_SkinEpiExtract/codelists/product.dta")

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
                                            "product"),
                                fileInput("import_codebrowser", label=NULL),
                            
                                
                                hr(),
                                
                                textInput("searchterms",
                                          label = tags$span(
                                              "Searchterms", 
                                              tags$i(
                                                  class = "glyphicon glyphicon-info-sign", 
                                                  style = "color:#0072B2;",
                                                  title = "Enter terms to search for separated by a semicolon (;) without spaces. E.g.: methotrexate;ciclosporin;azathioprin. To require that all off multiple terms are present in one entry, wrap terms in (?=.*term1)(?=.*term2).*?. E.g.: (?=.*hip)(?=.*fracture).*$ searches for entries that contain both hip and fracture.")),
                                              "methotrexate"),
                                textInput("exclusionterms",
                                          "Exclusionterms",
                                          "injection"),
                                htmlOutput("select_search_cols"),
                                verbatimTextOutput("searchterms"),
                                verbatimTextOutput("exclusionterms"),
                                verbatimTextOutput("cols"),
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
                                          tags$style(HTML("#withborder  {border: 4px solid black;}"))),

                                
                                fluidRow(id="withborder",
                                         h4("Initial codelist"),
                                         dataTableOutput("termsearched"),
                                ),
                                fluidRow(id="withborder",
                                         h4("Excluded"),
                                         dataTableOutput("excluded"),
                                ),
                                fluidRow(id="withborder",
                                         h4("Final codelist"),
                                         dataTableOutput("included"),
                                ),
                                fluidRow(h3("Checks"),
                                         htmlOutput("select_check_cols"),
                                         tableOutput("checks"),
                                ),
                            )
                        )
               ),
               tabPanel("Codelist Comparison", 
                        fluidRow(
                            column(6,
                                   fluidRow(
                                       column(3,
                                              fileInput("import_codelist_left", label=NULL)),
                                       column(3,
                                              actionButton("get_codelist_left", "Use from codelist maker")),
                                       column(3,
                                              htmlOutput("selectUI_left")),
                                       column(3,
                                              htmlOutput("matchcolumn")),
                                   ),
                                dataTableOutput("lefttable")
                            ),
                            column(6,
                                   fluidRow(
                                       column(4,
                                              fileInput("import_codelist_right", label=NULL)),
                                       column(4,
                                              actionButton("get_codelist_right", "Use from codelist maker")),
                                       column(4,
                                              htmlOutput("selectUI_right")
                                       ),
                                   ),
                                   dataTableOutput("righttable")
                            )
                        ),
                        fluidRow(
                            dataTableOutput("joined")
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
        codebrowser$data <- import(inFile$datapath)
    })
    

# Make dynamic UIs to pick columns ----------------------------------------

    
    #Make dynamically updating UI for picking the columns to search in
    output$select_search_cols <- renderUI({ 
        selectInput("cols", "Select columns to search in", names(codebrowser$data),
                    ifelse("productname" %in% names(codebrowser$data), "productname", 
                           ifelse("readterm" %in% names(codebrowser$data), "readterm", names(codebrowser$data)[[1]])),
                    multiple = TRUE)
    })
    
    #Make dynamically updating UI for picking the columns to display
    output$select_display_cols <- renderUI({ 
        selectInput("displaycolumns", "Select columns to display", names(codebrowser$data), multiple = TRUE)
    })
    
    #Make dynamically updating UI for picking the column to check
    output$select_check_cols <- renderUI({ 
        selectInput("checkcol", "Select column to check",names(codebrowser$data),
                    ifelse("bnftext" %in% names(codebrowser$data), "bnftext", names(codebrowser$data)[[1]]))
    })
  
    
# Get values from input ---------------------------------------------------

    #Make vectors from the inputs
    searchterms <- reactive(unlist(strsplit(input$searchterms,";")))  %>% debounce(2000)
    exclusionterms <- reactive(unlist(strsplit(input$exclusionterms,";")))  %>% debounce(2000)
    checkcol <- reactive(input$checkcol)
    
    cols <- reactive(input$cols)

    displaycolumns <- reactive({
        if (!is.null(input$displaycolumns)) {
            input$displaycolumns
        } else { names(codebrowser$data)}
    })
    
    

# Make the Tables ---------------------------------------------------------

    termsearched <- reactive({
        validate(need(cols() %in% names(codebrowser$data), "Loading")) # need to validate to avoid flashing error message, see: https://stackoverflow.com/questions/52378000/temporary-shiny-loading-error-filter-impl
        termsearch(
        .data = codebrowser$data,
        .cols = cols(),
        .searchterms = searchterms())
    })
    
    excluded <- reactive({
        termsearch(
        .data = termsearched(),
        .cols = cols(),
        .searchterms = exclusionterms())
    })
    
    included <- reactive(
        setdiff(termsearched(), excluded())
        )
    
    checks <- reactive(
        included() %>% 
        group_by(!!! syms(checkcol())) %>% 
        tally() %>% 
        arrange(desc(n))
    )
    

# Make extra highlighted Tables -------------------------------------------
    
    #Make a function to highlight text (to be used in str_replace_all)
    highlight_yellow <- function(x) {paste0("<span style='background-color:yellow;'>", x, "</span>")}
    highlight_green <- function(x) {paste0("<span style='background-color:LightGreen;'>", x, "</span>")}
    
    
    #Make included table with highlighted words
    termsearched_highlighted <- reactive({
        termsearched() %>%
            mutate(
                across(any_of(input$cols),
                       ~ str_replace_all(.x,
                                         regex(paste(searchterms(), collapse="|"), ignore_case = TRUE),
                                         highlight_green
                       )
                )
            )
    })
    
    #Make excluded table with highlighted words
    excluded_highlighted <- reactive({
        excluded() %>%
            mutate(
                across(any_of(input$cols),
                       ~ str_replace_all(.x,
                                         regex(paste(exclusionterms(), collapse="|"), ignore_case = TRUE),
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
    output$termsearched <- renderDataTable({ 
        termsearched_highlighted()[,displaycolumns(), drop=FALSE] 
        }, escape = FALSE, options = dtoptions)
    
    output$excluded <- renderDataTable({ 
        excluded_highlighted()[,displaycolumns(), drop=FALSE]
    }, escape = FALSE, options = dtoptions) #Need escape = FALSE if including HTML formatting
    
    output$included <- renderDataTable({ 
        included()[,displaycolumns(), drop=FALSE]
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
        cols <- cols()
        n <- max(length(searchterms), length(exclusionterms), length(cols))
        length(searchterms) <- n                      
        length(exclusionterms) <- n   
        length(cols) <- n  
        cbind(searchterms,exclusionterms,cols)
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
            
            fs <- c("codelist.csv", "terms.csv")
            write.csv(included(), "codelist.csv", row.names=FALSE, na = "")
            write.csv(termtable(), "terms.csv", row.names=FALSE, na = "")

            zip(zipfile=filename, files=fs)
        },
        contentType = "application/zip"
    )
    
    
#//////////////////////////////////////////////////////////////////////////
# 2. CODELIST COMPARISON ##################################################
#//////////////////////////////////////////////////////////////////////////

# Set values for left and right tables ------------------------------------
    
    # Make a reactiveValues to store the data; downstream functions will use whatever is stored in here ("duelling values", see https://stackoverflow.com/questions/29716868/r-shiny-how-to-get-an-reactive-data-frame-updated-each-time-pressing-an-actionb)
    v <- reactiveValues(lefttable = NULL, righttable = NULL)
    
    #Either get codelist from codelist maker ...
    observeEvent(input$get_codelist_left, {
        v$lefttable <- included()
    })
    observeEvent(input$get_codelist_right, {
        v$righttable <- included()
    })
    
    #... or import from file.
    observeEvent(input$import_codelist_left, {
        inFile <- input$import_codelist_left
        if (is.null(inFile))
            return(NULL)
        v$lefttable <- import(inFile$datapath)
    })  
    observeEvent(input$import_codelist_right, {
        inFile <- input$import_codelist_right
        if (is.null(inFile))
            return(NULL)
        v$righttable <- import(inFile$datapath)
    })  
    

# Join tables and identify matches ----------------------------------------

    #Make dynamically updating UI for picking the column to be matched on
    output$matchcolumn <- renderUI({ 
        selectInput("matchcolumn", label = "Match on", intersect(names(v$lefttable), names(v$righttable)),
                    NULL)
    })
    
    lefttable_joined <- reactive({
      validate(need(length(intersect(names(v$lefttable), names(v$righttable)))>0 | is.null(v$lefttable) | is.null(v$righttable), "Tables need at least one matching column"))
      validate(need(input$matchcolumn %in% names(v$lefttable) | is.null(v$lefttable) | is.null(v$righttable) , "Loading"))

        if (!is.null(v$lefttable) & !is.null(v$righttable)) {
            v$lefttable %>% 
                mutate(match=ifelse(!!sym(input$matchcolumn) %in% v$righttable[[input$matchcolumn]],
                                    "yes",
                                    "no"))
        } else {v$lefttable}
    })
    righttable_joined <- reactive({
      validate(need(length(intersect(names(v$lefttable), names(v$righttable)))>0 | is.null(v$lefttable) | is.null(v$righttable), "Tables need at least one matching column"))
      validate(need(input$matchcolumn %in% names(v$lefttable) | is.null(v$lefttable) | is.null(v$righttable) , "Loading"))
      
        if (!is.null(v$righttable) & !is.null(v$lefttable)) {
            v$righttable %>% 
                mutate(match=ifelse(!!sym(input$matchcolumn) %in% v$lefttable[[input$matchcolumn]],
                                    "yes",
                                    "no"))
        } else {v$righttable}
    })
    

    
    
# Pick which columns should be displayed  -------------------

    #Make dynamically updating UI for picking the columns to be displayed
    output$selectUI_left <- renderUI({ 
        selectInput("selectUI_left", label = "Display", names(lefttable_joined()), multiple = TRUE)
    })
    output$selectUI_right <- renderUI({ 
        selectInput("selectUI_right", label = "Display", names(righttable_joined()), multiple = TRUE)
    })
    
    #Display all columns if nothing is selected
    displaycolumns_left <- reactive({
        if (!is.null(input$selectUI_left)) {
            input$selectUI_left
        } else { names(lefttable_joined())}
    })
    displaycolumns_right <- reactive({
        if (!is.null(input$selectUI_right)) {
            input$selectUI_right
        } else { names(righttable_joined())}
    })
    



# Render the tables -------------------------------------------------------

    dtoptions2 <- list(pageLength = 20, scrollX = TRUE)
    
    output$lefttable <- renderDataTable({ 
        temp <- datatable(lefttable_joined()[,displaycolumns_left(), drop=FALSE], options = dtoptions2) 
        
        if (!is.null(v$righttable) & !is.null(v$lefttable) & ("match" %in% colnames(lefttable_joined()[,displaycolumns_left(), drop=FALSE]))) {
            temp %>% formatStyle("match", target = "row", backgroundColor = styleEqual(c("yes", "no"), c("LightGreen","LightCoral")), "white-space"="nowrap")
        } else { temp}
    })
    output$righttable <- renderDataTable({ 
        temp <- datatable(righttable_joined()[,displaycolumns_right(), drop=FALSE], options = dtoptions2) 
        
        if (!is.null(v$righttable) & !is.null(v$lefttable) & ("match" %in% colnames(righttable_joined()[,displaycolumns_left(), drop=FALSE]))) {
            temp %>% formatStyle("match", target = "row", backgroundColor = styleEqual(c("yes", "no"), c("LightGreen","LightCoral")), "white-space"="nowrap")
        } else { temp}
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
