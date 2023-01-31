
test_that("Case insensitive", {
    expect_equal(termsearch(c("diabetes", "Diabetes"),
                            c("diabetes"),
                            termset_search_method=TRUE),
                 c(TRUE, TRUE))
    expect_equal(termsearch(c("diabetes", "Diabetes"),
                            c("Diabetes"),
                            termset_search_method=TRUE),
                 c(TRUE, TRUE))
    expect_equal(termsearch(c("diabetes", "Diabetes"),
                            c("Diabetes", "diabetes"),
                            termset_search_method=TRUE),
                 c(TRUE, TRUE))
})

test_that("Words are matched in any order", {
    expect_equal(termsearch(c("Shoulder fracture", "Fracture of shoulder"),
                            c("shoulder fracture"),
                            termset_search_method=TRUE),
                 c(TRUE, TRUE))
    expect_equal(termsearch(c("Shoulder fracture", "Fracture of shoulder"),
                            c("shoulder fracture", "fracture shoulder"),
                            termset_search_method=TRUE),
                 c(TRUE, TRUE))
})

test_that("All words must be present", {
    expect_equal(termsearch(c("Diabetes type 2", "History of type 2 diabetes", "Type 1 diabetes"),
                            c("type 2 diabetes"),
                            termset_search_method=TRUE),
                 c(TRUE, TRUE, FALSE))
    expect_equal(termsearch(c("Diabetes type 2", "History of type 2 diabetes", "Type 1 diabetes"),
                            c("diabetes history", "diabetes type"),
                            termset_search_method=TRUE),
                 c(TRUE, TRUE, TRUE))
})

test_that("Use quotes to match exactly", {
    expect_equal(termsearch(c("Type 2 diabetes", "History of type 2 diabetes", "Diabetes, type 2"),
                            c('"type 2 diabetes"'),
                            termset_search_method=TRUE),
                 c(TRUE, TRUE, FALSE))
    expect_equal(termsearch(c("Type 2 diabetes"),
                            c('type 2 "diabetes"'),
                            termset_search_method=TRUE),
                 c(FALSE))
})

test_that("Wildcards allow partial word searching", {
    expect_equal(termsearch(c("Diabetes", "Diabetic patient"),
                            c("diabet*"),
                            termset_search_method=TRUE),
                 c(TRUE, TRUE))
    expect_equal(termsearch(c("Diabetes", "Diabetic patient", "Patient is diabetic"),
                            c("diabetic pat*"),
                            termset_search_method=TRUE),
                 c(FALSE, TRUE, TRUE))
    expect_equal(termsearch(c("Diabetes", "Diabetic patient", "Patient is diabetic"),
                            c("diabet*", "pat*"),
                            termset_search_method=TRUE),
                 c(TRUE, TRUE, TRUE))
})

test_that("Detects non-aphanumeric characters", {
    expect_equal(termsearch(c("H/O Diabetes"),
                            c("h/o"),
                            termset_search_method=TRUE),
                 c(TRUE))
    expect_equal(termsearch(c("HO: Diabetes"), #The colon ":" character is not part of a word within boundaries https://stackoverflow.com/questions/4134605/regex-and-the-colon
                            c("ho:"),
                            termset_search_method=TRUE),
                 c(TRUE))
    expect_equal(termsearch(c("H/O: Diabetes"),
                            c("h/o:"),
                            termset_search_method=TRUE),
                 c(TRUE))
    expect_equal(termsearch(c("H/O: Diabetes", "HO: Diabetes", "H/O: Diabetes"),
                            c("h/o"),
                            termset_search_method=TRUE),
                 c(TRUE, FALSE, TRUE))
})
