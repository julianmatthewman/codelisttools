library(shiny)
library(rio)
library(bslib)
library(haven)
library(tidyverse)


# Source all functions from the "R" folder
sapply(list.files("R", full.names = TRUE) ,source, .GlobalEnv)

#Import CPRD product browser
product <- import("in/product.csv")

# UI ----------------------------------------------------------------------

ui <- fluidPage(

    # Application title
    titlePanel("CPRD Codelist maker"),

    # Sidebar with inputs
    sidebarLayout(
        sidebarPanel(
            style = "position:fixed;width:23%;",
            width=3,
            tags$style(type='text/css', '#searchterms {white-space: pre-wrap;}'),
            tags$style(type='text/css', '#exclusionterms {white-space: pre-wrap;}'),
            tags$style(type='text/css', '#cols {white-space: pre-wrap;}'),
            
            
            textInput("searchterms",
                      "Searchterms",
                      "methotrexate"),
            textInput("exclusionterms",
                      "Exclusionterms",
                      "injection"),
            selectInput("cols", 
                        "Select columns to search in",
                        names(product), 
                        "productname",
                        multiple = TRUE),
            verbatimTextOutput("searchterms"),
            verbatimTextOutput("exclusionterms"),
            verbatimTextOutput("cols"),
            verbatimTextOutput("randomstrings"),
            
            hr(),
            downloadButton("downloadData", "Download"),
            hr(),
            selectInput("displaycolumns", "Select columns to display", names(product), multiple = TRUE)
            
            
        ),

        # Main panel with outputs
        mainPanel(
            width = 9,
            tags$head(tags$style("#termsearched  {white-space: nowrap;  }"),
                      tags$style("#excluded  {white-space: nowrap;  }"),
                      tags$style("#included  {white-space: nowrap;  }"),
                      tags$style(HTML("#withborder  {border: 4px solid black;}"))),
            selectInput("displaycolumns", "Select columns to display", names(product), multiple = TRUE),
            

            fluidRow(id="withborder",
                     h3("Termsearched"),
                dataTableOutput("termsearched"),
            ),
            fluidRow(id="withborder",
                     h3("Excluded"),
                     dataTableOutput("excluded"),
            ),
            fluidRow(id="withborder",
                     h3("Included"),
                     dataTableOutput("included"),
            ),
            fluidRow(h3("Checks"),
                     selectInput("checkcol", 
                                 "Select column to check",
                                 names(product), 
                                 "bnftext"),
                     tableOutput("checks"),
            ),
        )
    )
)


# SERVER ------------------------------------------------------------------

server <- function(input, output) {

# Get values from input ---------------------------------------------------

    #Make vectors from the inputs
    searchterms <- reactive(unlist(strsplit(input$searchterms,";")))
    exclusionterms <- reactive(unlist(strsplit(input$exclusionterms,";")))
    cols <- reactive(input$cols)
    

# Make the Tables ---------------------------------------------------------

    termsearched <- reactive(
        termsearch(
        .data = product,
        .cols = cols(),
        .searchterms = searchterms())
    )
    
    excluded <- reactive(
        termsearch(
        .data = termsearched(),
        .cols = cols(),
        .searchterms = exclusionterms())
    )
    
    included <- reactive(
        setdiff(termsearched(), excluded())
        )
    
    checks <- reactive(
        included() %>% 
        group_by(!! sym(input$checkcol)) %>% 
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
                across(everything(),
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
                across(everything(),
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
    displaycolumns <- reactive({
        if (!is.null(input$displaycolumns)) {
            input$displaycolumns
        } else { names(product)}
    })
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
    

# Downloadable csv of final codelist and terms --------------------------------------
    
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


    # Downloadable csv of selected dataset ----
    randomstrings <- reactive({termtable() #Make random strings that update whenever either of the tables updates 
                              included()
        stringi::stri_rand_strings(1, 6)})
    
    output$downloadData <- downloadHandler(
        filename = function() {paste("codelist","-", randomstrings(), "-", Sys.Date(), ".zip", sep = "")},
        content = function(filename) {
            tmpdir <- tempdir()
            setwd(tempdir())
            print(tempdir())
            
            fs <- c("codelist.csv", "terms.csv")
            write.csv(included(), "codelist.csv", row.names=FALSE)
            write.csv(termtable(), "terms.csv", row.names=FALSE)

            zip(zipfile=filename, files=fs)
        },
        contentType = "application/zip"
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
