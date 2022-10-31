---
bibliography: references.bib
---

## Codelist maker

### About

Create a codelist specifying the following :

-   **Searchterms**: Search for the occurence of these terms
-   **Exclusionterms**: Search for the occurence of these terms in the entries found via the initial search
-   **Columns**: Specify which columns should be searched

You will see three tables:

1.  The table of entries found in the source data using the searchterms
2.  The table of excluded entries found in the prior table using the exclusionterms
3.  The table containing the final codelist, which is the inital table, without the excluded entries

### Search Rules

Uses search rules as devised by Williams, R. et al. (2019):

-   **Case insensitive.** The term [fracture] matches "Shoulder fracture" and "Fracture of shoulder".
-   **Words are matched in any order.** The term [shoulder fracture] matches "Shoulder fracture" and "Fracture of shoulder".
-   **All words must be present.** The term [type 2diabetes] matches "Diabetes, type 2" and "History of type 2 diabetes", but not "Type 1 diabetes".
-   **Use quotes to match exactly.** The term ["type 2diabetes"] matches "Type 2diabetes" and "History of type 2diabetes" but not "Diabetes, type 2".
-   **Wildcards allow partial word searching.** The term [diabet\*] matches "Diabetes" and "Diabetic patient".
-   **Exact matches are never excluded.** The term [heart failure] always matches "Heart failure" even if [heart] were excluded.

> Williams, R. et al. (2019) 'Term sets: A transparent and reproducible representation of clinical code sets', PLOS ONE. Edited by I. Olier, 14(2), p. e0212291. Available at: <https://doi.org/10.1371/journal.pone.0212291>.

### Entering terms

Enter terms separated by a semicolon (;) without spaces. E.g.: methotrexate;ciclosporin;azathioprin.

### Downloading the codelist

Press the download button to download a zip archive containing:

-   a .csv file of the final codelist
-   a .csv file containing the searchterms, exclusionterms, and columns
