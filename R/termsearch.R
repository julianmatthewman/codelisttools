#' Search for entries in a lookup using searchterms
#'
#' @param lookup A vector of strings which should be searched in
#' @param searchterms A vector of strings to search with
#'
#' @return Those elements of lookup that match the searchterms
#' @details Uses the search strategy as described in: Williams, R. et al. (2019) ‘Term sets: A transparent and reproducible representation of clinical code sets’, PLOS ONE. Edited by I. Olier, 14(2), p. e0212291. Available at: https://doi.org/10.1371/journal.pone.0212291.

#' @export
# Search function

termsearch <- function(lookup, searchterms, termset_search_method=FALSE) {
    if(termset_search_method==TRUE & length(searchterms)>0) searchterms <- process_terms(searchterms)
    stringr::str_detect(lookup, stringr::regex(paste(searchterms, collapse = '|'), ignore_case = TRUE))
}


#' Process searchterms so termset search rules apply
#'
#' @param .searchterms
#' @return
process_terms <- function(.searchterms) {
    #Split into quoted and unquoted words
    quoted <- .searchterms[grepl('"(?:\\.|[^"\\])*"', .searchterms)]
    unquoted <- .searchterms[!grepl('"(?:\\.|[^"\\])*"', .searchterms)]
    
    #Remove quotes from quoted searchterms
    quoted <- quoted |>
        stringr::str_sub(start = 2, end = -2)
    
    # Transform unquoted searchterms so they are in this form: (?=.*(\\bterm1(\\b|\\s)))))(?=.*(\\bterm2(\\b|\\s)))))
    unquoted <- unquoted |>
        stringr::str_replace_all("\\*", "\\.*") |>
        strsplit(split = " ") |>
        purrr::map(~paste0("(?=.*(\\b",
                           paste(.x, collapse = "(\\b|\\s)))(?=.*(\\b"),
                           "(\\b|\\s)))")) |>
        unlist()
    
    # Merge quoted and unquoted back together
    c(quoted, unquoted)
}