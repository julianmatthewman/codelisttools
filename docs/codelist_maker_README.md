## Codelist maker

### About

Create a codelist specifying the following :

* **Searchterms**: Search for the occurence of these terms 
* **Exclusionterms**: Search for the occurence of these terms in the entries found via the initial search
* **Columns**: Specify which columns should be searched


The underlying function takes the data and keeps only rows where in any of the specified column at least one of the searchterms is found. Using dplyr it looks something like this:

`dplyr::filter(DATA, if_any(COLUMNS), ~ str_detect(.x, regex(paste(SEARCHTERMS, collapse = '|'), ignore_case = TRUE))`

You will see three tables:

1. The table of entries found in the source data using the searchterms
2. The table of excluded entries found in the prior table using the exclusionterms
3. The table containing the final codelist, which is the inital table, without the excluded entries


### Entering terms

Enter terms separated by a semicolon (;) without spaces. E.g.: methotrexate;ciclosporin;azathioprin. To require that all off multiple terms are present in one entry, wrap terms in `(?=.*term1)(?=.*term2).*?`. E.g.: `(?=.*hip)(?=.*fracture).*$` searches for entries that contain both hip and fracture (in any order).

### Downloading the codelist

Press the download button to download a zip archive containing:

* a .csv file of the final codelist
* a .csv file containing the searchterms, exclusionterms, and columns