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
        str_sub(start = 2, end = -2)
    
    # Transform unquoted searchterms so they are in this form: (?=.*term1)(?=.*term2).*?
    unquoted <- unquoted %>% 
        strsplit(split = " ") %>% 
        map(~paste0("(?=.*", paste(.x, collapse = ")(?=.*"), ").*?")) %>% 
        unlist()
    
    # Merge quoted and unquoted back together
    c(quoted, unquoted)
}





#' Codesearch
#' @description Uses searchterms to find entires in a source table (such as a a medical product/code browser)
#' @param .data A data.frame; the data to be searched
#' @param .cols The columns of data in which to search
#' @param .searchterms A character vector of terms to search for
#'
#' @return A data.frame containing the .data filtered using the .searchterms
#'
#' @examples
#' \dontrun{
#' #Search using searchterms (will return a table with all rows where in either of the columns there is a match for either of the searchterms)
#' codesearch(
#'     .data = iris,
#'     .cols = c("Species", "Sepal.Length"),
#'     .searchterms = c("setosa", "5.1"))

termsearch <- function(.data, .cols, .searchterms) {
    
    # Filter .data to rows where if in any of the .cols ...
    filter(.data, if_any(c(!!! syms(.cols)),
                         # ... an item matches one of the .searchterms.
                         ~ str_detect(.x, regex(paste(process_terms(.searchterms), collapse = '|'), ignore_case = TRUE))
    )
    )
}