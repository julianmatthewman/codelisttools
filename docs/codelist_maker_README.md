## Codelist maker

### About

Create a codelist specifying the following (enter terms separated by a new line) :

-   **Searchterms**: Search for the occurence of these terms
-   **Exclusionterms**: Search for the occurence of these terms in the entries found via the initial search
-   **Column**: Specify which column should be searched

You will see three tables:

1.  The table of entries found in the source data using the searchterms
2.  The table of excluded entries found in the prior table using the exclusionterms
3.  The table containing the final codelist, which is the inital table, without the excluded entries

### Checks

Below, optional checks can be enabled. **Unmatched descendants** will show any not included entries that, within the selected column, are the same or start with any of the included entries.

-   In hierarchical coding systems, such as ICD or Read, unmatched descendants can be found. For example, if code "E10" is included, all codes will be shown that start with "E10", such as "E10.1", "E10.2", etc., that aren't already included in the final codelist.

-   If multiple coding systems are present, e.g. SNOMED codes mapped to BNF chapters, entries with the same code or within the same chapter can be found. For example, if "Carvedilol" with the BNF chapter 20400080 is included, then "Eucardic" (a brand name for Carvedilol), which has the same BNF chapter, will be shown.

**Cross-tabulation** shows a contingency table on a specified column.

### Search Rules

#### Default

By default, terms are matched exactly and case-insensitively. 
-   **Regular expressions (regex)** are supported. For example the term _(?=.*fracture)(?=.*hip)_ matches "Hip Fracture" and "Fracture of Hip".

The underlying code takes the DATA and keeps only rows where in the specified COLUMN at least one of the SEARCHTERMS is found and none of the EXCLUSIONTERMS are present. This is implemented using R and dplyr, and can be used in a standalone R script if needed:

```         
termsearch <- function(lookup, terms) {
    stringr::str_detect(lookup, stringr::regex(paste(terms, collapse = '|'), ignore_case = TRUE))
}

initial <- dplyr::filter(DATA, termsearch(COLUMN, SEARCHTERMS))
excluded <- dplyr::filter(initial, termsearch(COLUMN, EXCLUSIONTERMS))
final <- dplyr::setdiff(initial, excluded)
```

#### Termset search method

If "Termset search method" is selected, search rules as devised by Williams, R. et al. (2019) are used (this is not optimised for speed here and therefore may be slow when searching large lookup datasets, and the implementation of the search strategy is more complex):

-   **Case insensitive.** The term _fracture_ matches "Shoulder fracture" and "Fracture of shoulder".
-   **Words are matched in any order.** The term _shoulder fracture_ matches "Shoulder fracture" and "Fracture of shoulder".
-   **All words must be present.** The term _type 2 diabetes_ matches "Diabetes, type 2" and "History of type 2 diabetes", but not "Type 1 diabetes".
-   **Use quotes to match exactly.** The term _"type 2 diabetes"_ matches "Type 2 diabetes" and "History of type 2 diabetes" but not "Diabetes, type 2".
-   **Wildcards allow partial word searching.** The term _diabet\*_ matches "Diabetes" and "Diabetic patient".
-   **Exact matches are never excluded.** The term _heart failure_ always matches "Heart failure" even if [heart] were excluded.

> Williams, R. et al. (2019) 'Term sets: A transparent and reproducible representation of clinical code sets', PLOS ONE. Edited by I. Olier, 14(2), p. e0212291. Available at: <https://doi.org/10.1371/journal.pone.0212291>.

### Downloading the codelist

Press the download button to download a zip archive containing:

-   a .csv file of the final codelist
-   a .csv file containing the searchterms, exclusionterms, columns, and search rules used
-   a .csv of the excluded codes
