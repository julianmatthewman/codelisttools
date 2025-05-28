#' The Codelist Tools Shiny App
#'
library(shiny)
library(markdown) #https://forum.posit.co/t/warning-error-in-loadnamespace-there-is-no-package-called-markdown/121671/4
loadSupport()
# Run tests with testthat::test_dir("tests")
# Deploy via https://connect.posit.cloud/


  # Import all browsers in the "in" folder
  paths <- dir("in", full.names = TRUE)
  browsers <- purrr::map(paths, \(x) rio::import(x, colClasses = c("character"))) |>
    purrr::set_names(basename(tools::file_path_sans_ext(paths)))

  # UI ----------------------------------------------------------------------

  ui <- fluidPage(
    navbarPage(
      "Codelist tools",
      tabPanel(
        "Codelist Maker",
        # Sidebar with inputs
        sidebarLayout(
          sidebarPanel(
            width = 3,
            tags$style(
              type = "text/css",
              "#searchterms {white-space: pre-wrap;}"
            ),
            tags$style(
              type = "text/css",
              "#exclusionterms {white-space: pre-wrap;}"
            ),
            tags$style(type = "text/css", "#cols {white-space: pre-wrap;}"),
            selectInput(
              "select_codebrowser",
              "Select browser",
              names(browsers),
              "ICD10_Edition5_GB_20160401"
            ),
            fileInput("import_codebrowser", label = NULL),
            hr(),
            textAreaInput(
              "searchterms",
              "Searchterms",
              "diabetes",
              resize = "vertical"
            ),
            textAreaInput(
              "exclusionterms",
              "Exclusionterms",
              "insipidus",
              resize = "vertical"
            ),
            fileInput(
              "import_search_terms",
              "Import Search Terms",
              accept = c(".json", ".csv")
            ),
            htmlOutput("select_search_cols"),
            checkboxInput(
              "termset_search_method",
              label = tags$span(
                "Termset search method",
                tags$i(
                  class = "glyphicon glyphicon-info-sign",
                  style = "color:#0072B2;",
                  title = 'For search rules see "About" Tab'
                )
              )
            ),
            verbatimTextOutput("randomstrings"),
            hr(),
            downloadButton("downloadData", "Download"),
            hr(),
            htmlOutput("select_display_cols"),
          ),

          # Main panel with outputs
          mainPanel(
            width = 9,
            tags$head(
              tags$style("#termsearched  {white-space: nowrap;  }"),
              tags$style("#excluded  {white-space: nowrap;  }"),
              tags$style("#included  {white-space: nowrap;  }"),
              tags$style("#descendants  {white-space: nowrap;  }"),
              tags$style(HTML("#withborder  {border: 4px solid black;}"))
            ),
            fluidRow(
              id = "withborder",
              h4("Initial codelist"),
              DT::dataTableOutput("termsearched"),
            ),
            fluidRow(
              id = "withborder",
              h4("Excluded"),
              DT::dataTableOutput("excluded"),
            ),
            fluidRow(
              id = "withborder",
              h4("Final codelist"),
              DT::dataTableOutput("included"),
            ),
            hr(),
            fluidRow(id = "withborder", h4("Checks")),
            fluidRow(
              id = "withborder",
              column(4, h4("Unmatched descendants")),
              column(
                4,
                checkboxInput(
                  "descendant_matching",
                  "Enable descendant searching, on column:"
                )
              ),
              column(
                4,
                style = "margin-top: 5px;",
                htmlOutput("select_code_cols")
              ),
              DT::dataTableOutput("descendants"),
            ),
            fluidRow(
              id = "withborder",
              column(4, h4("Cross-tabulation")),
              column(
                4,
                checkboxInput("crosstab", "Enable cross-tabulation, on column:")
              ),
              column(
                4,
                style = "margin-top: 5px;",
                htmlOutput("select_check_cols")
              ),
              tableOutput("checks"),
            ),
          )
        )
      ),
      tabPanel(
        "Codelist Comparison",
        fluidRow(
          column(
            6,
            fluidRow(
              id = "withborder",
              column(9, loadTableModuleUI("left")),
              column(
                3,
                htmlOutput("matchcolumn"),
                style = "margin-bottom: -15px;"
              )
            ),
            fluidRow(id = "withborder", joinRenderTableModuleUI("left"))
          ),
          column(
            6,
            fluidRow(id = "withborder", loadTableModuleUI("right")),
            fluidRow(id = "withborder", joinRenderTableModuleUI("right"))
          )
        )
      ),
      tabPanel(
        "Word frequency",
        fluidRow(
          column(
            12,
            fluidRow(
              id = "withborder",
              column(6, loadTableModuleUI("cl_for_word_freq")),
              column(
                6,
                htmlOutput("select_search_cols_word_freq"),
                style = "margin-bottom: -15px;"
              )
            )
          )
        ),
        fluidRow(
          column(
            3,
            id = "withborder",
            h4("Terms"),
            verbatimTextOutput("wordfreqterms")
          ),
          column(
            3,
            id = "withborder",
            h4("Monograms"),
            DT::dataTableOutput("monograms")
          ),
          column(
            3,
            id = "withborder",
            h4("Bigrams"),
            DT::dataTableOutput("bigrams")
          ),
          column(
            3,
            id = "withborder",
            h4("Trigrams"),
            DT::dataTableOutput("trigrams")
          )
        )
      ),
      tabPanel(
        "Categorisation",
        sidebarLayout(
          sidebarPanel(
            fluidRow(
              fileInput("cat_import_codelist", label = "Upload or get codelist"),
              div(style = "margin-top: -15px"),
              actionButton("cat_get_codelist", "from codelist maker"),
              div(style = "margin-bottom: 15px"),
              htmlOutput("select_search_cols_categorisationTable"),
              radioButtons(
                "model_choice",
                "Choose model:",
                choices = list(
                  "None" = "none",
                  "Gemini (API required)" = "gemini",
                  "Local (Ollama)" = "ollama"
                ),
                selected = "none"
              ),
              conditionalPanel(
                condition = "input.model_choice == 'ollama'",
                textInput("ollama_model_name", "Ollama model name:")
              ),
              conditionalPanel(
                condition = "input.model_choice == 'gemini'",
                #add text output that says "API key required"
                helpText(
                  "To run a Gemini model, an API key is required. Generate an API key at https://aistudio.google.com/app/apikey and save it as GOOGLE_API_KEY=<your_api_key> in your .Renviron file. Restart R to use the API key."
                )
              ),
              uiOutput("category_checkboxes"),
              textInput("new_category", "Add New Category"),
              actionButton("add_category", "Add Category"),
              actionButton("classify", "Classify Codes")
            )
          ),
          mainPanel(
            id = "withborder",
            DT::dataTableOutput("categorisationTable"),
            DT::dataTableOutput("categorisationTableClassified")
          )
        )
      ),
      tabPanel(
        "About",
        fluidRow(
          column(
            6,
            htmltools::includeMarkdown("docs/codelist_maker_README.md")
          ),
          column(
            6,
            htmltools::includeMarkdown("docs/codelist_comparison_README.md"),
            htmltools::includeMarkdown("docs/word_frequency_README.md")
          )
        )
      )
    )
  )



  # SERVER ------------------------------------------------------------------

  server <- function(input, output, session) {
    options(shiny.maxRequestSize = 100 * 1024^2)

    # //////////////////////////////////////////////////////////////////////////
    # 1. CODELIST MAKER -------------------------------------------------------
    # //////////////////////////////////////////////////////////////////////////

    # Select browser ----------------------------------------------------------

    # Make a reactiveValues to store the data; downstream functions will use whatever is stored in here ("duelling values", see https://stackoverflow.com/questions/29716868/r-shiny-how-to-get-an-reactive-data-frame-updated-each-time-pressing-an-actionb)
    codebrowser <- reactiveValues(data = NULL)

    # Either select built in browser ...
    observeEvent(input$select_codebrowser, {
      codebrowser$data <- browsers[[input$select_codebrowser]]
    })

    # ... or import from file.
    observeEvent(input$import_codebrowser, {
      inFile <- input$import_codebrowser
      if (is.null(inFile)) {
        return(NULL)
      }
      codebrowser$data <- rio::import(
        inFile$datapath,
        colClasses = c("character")
      )
    })

    # Select search terms ----------------------------------------------------------

    search_browser <- reactiveValues(
      includeTerms = NULL,
      excludeTerms = NULL
    )

    observeEvent(input$import_search_terms, {
      req(input$import_search_terms)

      file_path <- input$import_search_terms$datapath
      file_ext <- tools::file_ext(input$import_search_terms$name)

      if (file_ext == "json") {
        # Read and parse JSON file
        json_data <- jsonlite::fromJSON(input$import_search_terms$datapath)

        # Store in reactive values
        search_browser$includeTerms <- json_data$includeTerms
        search_browser$excludeTerms <- json_data$excludeTerms

        # Convert terms to a single string, separated by new lines
        include_terms_text <- paste(json_data$includeTerms, collapse = "\n")
        exclude_terms_text <- paste(json_data$excludeTerms, collapse = "\n")
      } else if (file_ext == "csv") {
        suppressWarnings({
          csv_data <- readr::read_csv(
            file_path,
            col_select = c(searchterms, exclusionterms),
            show_col_types = FALSE
          )
        })

        csvsearchterms <- csv_data$searchterms[
          !is.na(csv_data$searchterms) & csv_data$searchterms != ""
        ]
        csvexcludeterms <- csv_data$exclusionterms[
          !is.na(csv_data$exclusionterms) & csv_data$exclusionterms != ""
        ]

        search_browser$includeTerms <- csvsearchterms
        search_browser$excludeTerms <- csvexcludeterms

        # Convert terms to a single string, separated by new lines
        include_terms_text <- paste(csvsearchterms, collapse = "\n")
        exclude_terms_text <- paste(csvexcludeterms, collapse = "\n")
      } else {
        showNotification(
          "Unsupported file format. Please upload JSON or CSV.",
          type = "error"
        )
        return()
      }

      # Update the textAreaInput fields dynamically
      updateTextAreaInput(session, "searchterms", value = include_terms_text)
      updateTextAreaInput(session, "exclusionterms", value = exclude_terms_text)
    })

    # Make dynamic UIs to pick columns ----------------------------------------

    # Make dynamically updating UI for picking the columns to search in
    output$select_search_cols <- renderUI({
      selectInput(
        "cols",
        "Select column to search in",
        names(codebrowser$data),
        ifelse(
          "DESCRIPTION" %in% names(codebrowser$data),
          "DESCRIPTION",
          ifelse(
            "productname" %in% names(codebrowser$data),
            "productname",
            ifelse(
              "readterm" %in% names(codebrowser$data),
              "readterm",
              names(codebrowser$data)[[1]]
            )
          )
        ),
        multiple = FALSE
      )
    })

    # Make dynamically updating UI for picking the columns to display
    output$select_display_cols <- renderUI({
      selectInput(
        "displaycolumns",
        "Select columns to display",
        names(codebrowser$data),
        multiple = TRUE
      )
    })

    # Make dynamically updating UI for picking the column to check
    output$select_check_cols <- renderUI({
      selectInput(
        "checkcol",
        label = NULL,
        names(codebrowser$data),
        ifelse(
          "USAGE" %in% names(codebrowser$data),
          "USAGE",
          names(codebrowser$data)[[1]]
        )
      )
    })

    # Make dynamically updating UI for picking the column to match for descendants
    output$select_code_cols <- renderUI({
      selectInput(
        "codecol",
        label = NULL,
        names(codebrowser$data),
        ifelse(
          "CODE" %in% names(codebrowser$data),
          "CODE",
          names(codebrowser$data)[[1]]
        )
      )
    })

    # Get values from input ---------------------------------------------------

    termset_search_method <- reactive(input$termset_search_method)
    descendant_matching <- reactive(input$descendant_matching)
    crosstab <- reactive(input$crosstab)

    # Make vectors from the inputs
    searchterms <- reactive(unlist(strsplit(input$searchterms, "\n"))) |>
      debounce(2000)
    exclusionterms <- reactive(unlist(strsplit(input$exclusionterms, "\n"))) |>
      debounce(2000)
    checkcol <- reactive(input$checkcol)
    codecol <- reactive(input$codecol)

    cols <- reactive(input$cols)

    displaycolumns <- reactive({
      if (!is.null(input$displaycolumns)) {
        input$displaycolumns
      } else {
        names(codebrowser$data)
      }
    })

    # Make the Tables ---------------------------------------------------------

    termsearched <- reactive({
      validate(need(cols() %in% names(codebrowser$data), "Loading")) # need to validate to avoid flashing error message, see: https://stackoverflow.com/questions/52378000/temporary-shiny-loading-error-filter-impl
      codebrowser$data |>
        dplyr::filter(termsearch(
          eval(dplyr::sym(cols())),
          searchterms(),
          termset_search_method()
        ))
    })

    excluded <- reactive({
      termsearched() |>
        dplyr::filter(
          termsearch(
            eval(dplyr::sym(cols())),
            exclusionterms(),
            termset_search_method()
          ) &
            !(tolower(eval(dplyr::sym(cols()))) %in% tolower(searchterms()))
        ) # This is so exact matches are never excluded. The term [heart failure] always matches "Heart failure" even if [heart] were excluded.
    })

    included <- reactive({
      dplyr::setdiff(termsearched(), excluded())
    })

    checks <- reactive({
      validate(need(crosstab() == TRUE, message = FALSE))
      included() |>
        dplyr::group_by(!!!dplyr::syms(checkcol())) |>
        dplyr::tally() |>
        dplyr::arrange(desc(n))
    })

    descendants <- reactive({
      validate(
        need(codecol() %in% names(codebrowser$data), "Loading"),
        need(codecol() %in% names(included()), "Loading"), # need to validate to avoid flashing error message
        need(searchterms(), "No searchterms provided"),
        need(nrow(included()) > 0, "Nothing included"),
        need(descendant_matching() == TRUE, message = FALSE)
      )
      temp <- dplyr::filter(
        codebrowser$data,
        stringr::str_starts(
          eval(dplyr::sym(codecol())),
          paste(included()[[codecol()]], collapse = "|")
        )
      )
      if (length(exclusionterms()) > 0)
        temp <- dplyr::filter(
          temp,
          !termsearch(
            eval(dplyr::sym(cols())),
            exclusionterms(),
            termset_search_method()
          )
        )
      dplyr::setdiff(temp, included())
    })

    # Make extra highlighted Tables -------------------------------------------

    # Make a function to highlight text (to be used in str_replace_all)
    highlight_yellow <- function(x) {
      paste0("<span style='background-color:yellow;'>", x, "</span>")
    }
    highlight_green <- function(x) {
      paste0("<span style='background-color:LightGreen;'>", x, "</span>")
    }

    # Make included table with highlighted words
    termsearched_highlighted <- reactive({
      # Transform searchterms so they are in this form: term1|term2|term3|...
      searchterms_highlightable <- searchterms() |>
        strsplit(split = " ") |>
        purrr::map(~ paste(.x, collapse = "|")) |>
        unlist()

      termsearched() |>
        dplyr::mutate(
          dplyr::across(
            dplyr::any_of(input$cols),
            ~ stringr::str_replace_all(
              .x,
              stringr::regex(
                paste(searchterms_highlightable, collapse = "|"),
                ignore_case = TRUE
              ),
              highlight_green
            )
          )
        )
    })

    # Make excluded table with highlighted words
    excluded_highlighted <- reactive({
      excluded() |>
        dplyr::mutate(
          dplyr::across(
            dplyr::any_of(input$cols),
            ~ stringr::str_replace_all(
              .x,
              stringr::regex(
                paste(exclusionterms(), collapse = "|"),
                ignore_case = TRUE
              ),
              highlight_yellow
            )
          )
        )
    })

    # Render Text and Tables -------------------------------------------------------

    # Set table options
    dtoptions <- list(pageLength = 5, scrollX = TRUE)
    # Set which tables to display

    # Render tables
    output$termsearched <- DT::renderDataTable(
      {
        termsearched_highlighted()[, displaycolumns(), drop = FALSE]
      },
      escape = FALSE,
      options = dtoptions
    )

    output$excluded <- DT::renderDataTable(
      {
        excluded_highlighted()[, displaycolumns(), drop = FALSE]
      },
      escape = FALSE,
      options = dtoptions
    ) # Need escape = FALSE if including HTML formatting

    output$included <- DT::renderDataTable(
      {
        included()[, displaycolumns(), drop = FALSE]
      },
      options = dtoptions
    )

    # Render descendants
    output$descendants <- DT::renderDataTable(
      {
        descendants()[, displaycolumns(), drop = FALSE]
      },
      options = dtoptions
    )

    # Render extra table containing checks
    output$checks <- renderTable(
      checks()
    )

    # Print values
    output$searchterms <- renderPrint({
      cat("Searchterms:\n")
      print(searchterms())
    })
    output$exclusionterms <- renderPrint({
      cat("Exclusionterms:\n")
      print(exclusionterms())
    })
    output$cols <- renderPrint({
      cat("Searched in:\n")
      print(cols())
    })

    # Downloadable csvs of final codelist and terms --------------------------------------

    # Make table of searchterms
    termtable <- reactive({
      searchterms <- searchterms()
      exclusionterms <- exclusionterms()
      searched_in_column <- cols()
      searchmethod <- ifelse(
        termset_search_method() == TRUE,
        "Term sets search method, see 10.1371/journal.pone.0212291",
        "termsearch <- function(lookup, terms) {stringr::str_detect(lookup, stringr::regex(paste(terms, collapse = '|'), ignore_case = TRUE))}; initial <- dplyr::filter(DATA, termsearch(COLUMN, SEARCHTERMS)); excluded <- dplyr::filter(initial, termsearch(COLUMN, EXCLUSIONTERMS); final <- dplyr::setdiff(inital, excluded)"
      )
      n <- max(
        length(searchterms),
        length(exclusionterms),
        length(searched_in_column),
        length(searchmethod)
      )
      length(searchterms) <- n
      length(exclusionterms) <- n
      length(searched_in_column) <- n
      length(searchmethod) <- n
      cbind(searchterms, exclusionterms, searched_in_column, searchmethod)
    })

    # Make random string to include in filename
    randomstrings <- reactive({
      termtable() # Make random strings that update whenever either of the tables updates
      included()
      stringi::stri_rand_strings(1, 6)
    })

    # Zip codelist and terms and provide via download button
    output$downloadData <- downloadHandler(
      filename = function() {
        paste(
          "codelist",
          "-",
          randomstrings(),
          "-",
          Sys.Date(),
          ".zip",
          sep = ""
        )
      },
      content = function(filename) {
        tmpdir <- tempdir()
        setwd(tempdir())
        print(tempdir())

        fs <- c("codelist.csv", "terms.csv", "excluded.csv")
        utils::write.csv(
          included(),
          "codelist.csv",
          row.names = FALSE,
          quote = FALSE,
          na = ""
        )
        utils::write.csv(
          termtable(),
          "terms.csv",
          row.names = FALSE,
          quote = FALSE,
          na = ""
        )
        utils::write.csv(
          excluded(),
          "excluded.csv",
          row.names = FALSE,
          quote = FALSE,
          na = ""
        )

        utils::zip(zipfile = filename, files = fs)
      },
      contentType = "application/zip"
    )

    # //////////////////////////////////////////////////////////////////////////
    # 2. CODELIST COMPARISON ##################################################
    # //////////////////////////////////////////////////////////////////////////

    # Make dynamically updating UI for picking the column to be matched on
    output$matchcolumn <- renderUI({
      selectInput(
        "matchcolumn_in",
        label = "Match on",
        intersect(names(lefttable()), names(righttable())),
        NULL
      )
    })
    matchcolumn <- reactive(input$matchcolumn_in)

    # Loading, joining, and rendering of tables is handled via modules
    lefttable <- loadTableModule("left", reactive(included()))
    righttable <- loadTableModule("right", reactive(included()))
    joinRenderTableModule(
      "left",
      reactive(lefttable()),
      reactive(righttable()),
      reactive(matchcolumn())
    )
    joinRenderTableModule(
      "right",
      reactive(righttable()),
      reactive(lefttable()),
      reactive(matchcolumn())
    )

    # //////////////////////////////////////////////////////////////////////////
    # 3. WORD FREQUENCY -------------------------------------------------------
    # //////////////////////////////////////////////////////////////////////////

    cl_for_word_freq <- loadTableModule(
      "cl_for_word_freq",
      reactive(included())
    )

    # Make dynamically updating UI for picking the columns to search in
    output$select_search_cols_word_freq <- renderUI({
      selectInput(
        "wordfreqcol",
        "Select column with terms",
        names(cl_for_word_freq()),
        names(cl_for_word_freq())[[1]],
        multiple = FALSE
      )
    })
    wordfreqcol <- reactive(input$wordfreqcol)
    wordfreqterms <- reactive(cl_for_word_freq()[[wordfreqcol()]])

    output$wordfreqterms <- renderPrint({
      print(wordfreqterms())
    })

    # Function to make n-grams
    ngramer <- function(x, n) {
      codelist_clean <- tolower(x)
      codelist_clean <- gsub("[[:punct:]]", " ", codelist_clean)
      stop_words <- tm::stopwords("en")
      codelist_clean <- tm::removeWords(codelist_clean, stop_words)
      df <- data.frame(text = codelist_clean, stringsAsFactors = FALSE)
      ngram <- df |>
        tidytext::unnest_tokens(term, text, token = "ngrams", n = n) |>
        dplyr::count(term, sort = TRUE) |>
        dplyr::filter(term != "")
      return(ngram)
    }

    # Make n-grams
    monograms <- reactive({
      ngramer(wordfreqterms(), 1)
    })
    bigrams <- reactive({
      ngramer(wordfreqterms(), 2)
    })
    trigrams <- reactive({
      ngramer(wordfreqterms(), 3)
    })

    # Render n-grams
    output$monograms <- DT::renderDataTable(
      {
        monograms()[drop = FALSE]
      },
      options = dtoptions
    )
    output$bigrams <- DT::renderDataTable(
      {
        bigrams()[drop = FALSE]
      },
      options = dtoptions
    )
    output$trigrams <- DT::renderDataTable(
      {
        trigrams()[drop = FALSE]
      },
      options = dtoptions
    )

    # //////////////////////////////////////////////////////////////////////////
    # 4. CATEGORISATION -------------------------------------------------------
    # //////////////////////////////////////////////////////////////////////////

    # Initialize reactive values for categories
    categories <- reactiveVal(c(
      "Diagnosis",
      "Administration",
      "Personal history",
      "Family history",
      "Symptom",
      "Negation"
    ))

    # Render the checkbox group UI
    output$category_checkboxes <- renderUI({
      checkboxGroupInput(
        "selected_categories",
        "Select categories for classification:",
        choices = categories(),
        selected = categories()
      )
    })

    # Add new category handler
    observeEvent(input$add_category, {
      if (input$new_category != "" && !(input$new_category %in% categories())) {
        # Update the reactive categories list
        new_cats <- c(categories(), input$new_category)
        categories(new_cats)

        # Clear the input field
        updateTextInput(session, "new_category", value = "")
      }
    })

    # Initialize chat models
    gemini_chat <- NULL
    ollama_chat <- NULL

    # Load Table (since the table is either uploaded, from the codelist maker or from the classification results, don't use the module here)
    # Set value for table; Make a reactiveVal to store the data; call the function with a single argument to set the value 
    categorisationTable <- reactiveVal(NULL)

    # Either get codelist from codelist maker ...
    observeEvent(input$cat_get_codelist, {
      categorisationTable(included()) # Sets the value to the codelist from the codelist maker
    })

    # ... or import from file.
    observeEvent(input$cat_import_codelist, {
      inFile <- input$cat_import_codelist
      if (is.null(inFile)) {
        return(NULL)
      }
      categorisationTable(rio::import(inFile$datapath) |> 
          dplyr::mutate(dplyr::across(dplyr::everything(), as.character))) # Sets the value to the codelist from the upload
    })

    output$categorisationTable <- DT::renderDataTable({
      DT::datatable(
        categorisationTable(),
        class = 'nowrap display',
        extensions = "Buttons",
        options = list(
          pageLength = 20,
          scrollX = TRUE,
          dom = "Bfrtip",
          buttons = I("colvis")
        )
      )
    })

    # Update the column selection
    output$select_search_cols_categorisationTable <- renderUI({
      selectInput(
        "search_col_categorisationTable",
        "Select column to classify",
        names(categorisationTable()),
        names(categorisationTable())[[1]],
        multiple = FALSE
      )
    })
    search_cols_categorisationTable <- reactive(
      input$search_col_categorisationTable
    )

    # ... or perform classification
    observeEvent(input$classify, {
      req(input$search_col_categorisationTable)
      req(categorisationTable())
      req(input$model_choice)

      # Show notification if no model is selected
      if (input$model_choice == "none") {
        showNotification("No classification model selected", type = "warning")
        return()
      }

      # Get the model to use
      model_type <- input$model_choice

      # Initialize the appropriate chat model if needed
      if (model_type == "gemini" && is.null(gemini_chat)) {
        gemini_chat <- ellmer::chat_google_gemini(
          # model = "gemini-2.0-flash",
          system_prompt = "Classify clinical codes into clinically meaningful categories."
        )
      } else if (model_type == "ollama" && is.null(ollama_chat)) {
        req(input$ollama_model_name)
        tryCatch(
          {
            ollama_chat <- ellmer::chat_ollama(
              model = input$ollama_model_name,
              system_prompt = "Classify clinical codes into clinically meaningful categories."
            )
          },
          error = function(e) {
            showNotification(
              paste("Error initializing Ollama model:", e$message),
              type = "error"
            )
            return()
          }
        )
      }

      # Get the active chat model
      active_chat <- if (model_type == "gemini") gemini_chat else ollama_chat

      # Make sure we have a valid chat model
      if (is.null(active_chat)) {
        showNotification("Failed to initialize chat model", type = "error")
        return()
      }

      # Classify the term
      temp <- categorisationTable()
      tempcol <- input$search_col_categorisationTable
      terms <- temp[[tempcol]]
      result <- active_chat$chat_structured(
        terms,
        type = ellmer::type_array(
          items = ellmer::type_object(
            name = ellmer::type_string(),
            category = ellmer::type_enum(
              "Category",
              values = input$selected_categories
            ),
            explanation = ellmer::type_string()
          )
        )
      )

      # Update the reactive data
      categorisationTable(temp |> dplyr::bind_cols(result)) # Sets the value to the codelist with classification results
      showNotification("Classification completed", type="message")
    })
  }

  # Run the application
  shinyApp(ui = ui, server = server)