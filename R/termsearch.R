#' Search for entries in a lookup using searchterms
#'
#' @param lookup A vector of strings which should be searched in
#' @param searchterms A vector of strings to search with
#'
#' @return Those elements of lookup that match the searchterms
#' @export
termsearch <- function(lookup, searchterms) {
    stringr::str_detect(lookup, stringr::regex(paste(searchterms, collapse = '|'), ignore_case = TRUE))
}
