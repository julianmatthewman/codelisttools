
test_that("Case insensitive", {
    expect_equal(termsearch(c("diabetes", "Diabetes"),
                            process_terms(c("diabetes"))),
                 c(TRUE, TRUE))
    expect_equal(termsearch(c("diabetes", "Diabetes"),
                            process_terms(c("Diabetes"))),
                 c(TRUE, TRUE))
    expect_equal(termsearch(c("diabetes", "Diabetes"),
                            process_terms(c("Diabetes", "diabetes"))),
                 c(TRUE, TRUE))
})

test_that("Words are matched in any order", {
    expect_equal(termsearch(c("Shoulder fracture", "Fracture of shoulder"),
                            process_terms(c("shoulder fracture"))),
                 c(TRUE, TRUE))
    expect_equal(termsearch(c("Shoulder fracture", "Fracture of shoulder"),
                            process_terms(c("shoulder fracture", "fracture shoulder"))),
                 c(TRUE, TRUE))
})

test_that("All words must be present", {
    expect_equal(termsearch(c("Diabetes type 2", "History of type 2 diabetes", "Type 1 diabetes"),
                            process_terms(c("type 2 diabetes"))),
                 c(TRUE, TRUE, FALSE))
    expect_equal(termsearch(c("Diabetes type 2", "History of type 2 diabetes", "Type 1 diabetes"),
                            process_terms(c("diabetes history", "diabetes type"))),
                 c(TRUE, TRUE, TRUE))
})

test_that("Use quotes to match exactly", {
    expect_equal(termsearch(c("Type 2 diabetes", "History of type 2 diabetes", "Diabetes, type 2"),
                            process_terms(c('"type 2 diabetes"'))),
                 c(TRUE, TRUE, FALSE))
    expect_equal(termsearch(c("Type 2 diabetes"),
                            process_terms(c('type 2 "diabetes"'))),
                 c(FALSE))
})

test_that("Wildcards allow partial word searching", {
    expect_equal(termsearch(c("Diabetes", "Diabetic patient"),
                            process_terms(c("diabet*"))),
                 c(TRUE, TRUE))
    expect_equal(termsearch(c("Diabetes", "Diabetic patient", "Patient is diabetic"),
                            process_terms(c("diabetic pat*"))),
                 c(FALSE, TRUE, TRUE))
    expect_equal(termsearch(c("Diabetes", "Diabetic patient", "Patient is diabetic"),
                            process_terms(c("diabet*", "pat*"))),
                 c(TRUE, TRUE, TRUE))
})

test_that("Detects non-aphanumeric characters", {
    expect_equal(termsearch(c("H/O Diabetes"),
                            process_terms(c("h/o"))),
                 c(TRUE))
    expect_equal(termsearch(c("HO: Diabetes"), #The colon ":" character is not part of a word within boundaries https://stackoverflow.com/questions/4134605/regex-and-the-colon
                            process_terms(c("ho:"))),
                 c(TRUE))
    expect_equal(termsearch(c("H/O: Diabetes"),
                            process_terms(c("h/o:"))),
                 c(TRUE))
    expect_equal(termsearch(c("H/O: Diabetes", "HO: Diabetes", "H/O: Diabetes"),
                            process_terms(c("h/o"))),
                 c(TRUE, FALSE, TRUE))
})
