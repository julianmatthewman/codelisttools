#' Search for entries in a lookup using searchterms
#'
#' @param lookup A vector of strings which should be searched in
#' @param searchterms A vector of strings to search with
#'
#' @return Those elements of lookup that match the searchterms
#' @details Uses the search strategy as described in: Williams, R. et al. (2019) ‘Term sets: A transparent and reproducible representation of clinical code sets’, PLOS ONE. Edited by I. Olier, 14(2), p. e0212291. Available at: https://doi.org/10.1371/journal.pone.0212291.

#' @export
termsearch <- function(lookup, searchterms) {
    stringr::str_detect(lookup, stringr::regex(paste(process_terms(searchterms), collapse = '|'), ignore_case = TRUE))
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
    quoted <- quoted %>%
        stringr::str_sub(start = 2, end = -2)
    
    # Transform unquoted searchterms so they are in this form: (?=.*(\\bterm1\\b))(?=.*(\\bterm2\\b))
    unquoted <- unquoted %>%
        stringr::str_replace_all("\\*", "\\.*") %>%
        strsplit(split = " ") %>%
        purrr::map(~paste0("(?=.*(\\b",
                           paste(.x, collapse = "\\b))(?=.*(\\b"),
                           "\\b))")) %>%
        unlist()
    
    # Merge quoted and unquoted back together
    c(quoted, unquoted)
}


termsearch_cols <- function(.data, .cols, .searchterms) {
    
    # Filter .data to rows where if in any of the .cols ...
    filter(.data, if_any(c(!!! syms(.cols)), ~ termsearch(.x, .searchterms))
    )
}