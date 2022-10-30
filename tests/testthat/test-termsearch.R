
test_that("Case insensitive", {
    expect_equal(termsearch(c("diabetes", "Diabetes"),
                            c("diabetes")),
                 c(TRUE, TRUE))
    expect_equal(termsearch(c("diabetes", "Diabetes"),
                            c("Diabetes")),
                 c(TRUE, TRUE))
    expect_equal(termsearch(c("diabetes", "Diabetes"),
                            c("Diabetes", "diabetes")),
                 c(TRUE, TRUE))
})

test_that("Words are matched in any order", {
    expect_equal(termsearch(c("Shoulder fracture", "Fracture of shoulder"),
                            c("shoulder fracture")),
                 c(TRUE, TRUE))
    expect_equal(termsearch(c("Shoulder fracture", "Fracture of shoulder"),
                            c("shoulder fracture", "fracture shoulder")),
                 c(TRUE, TRUE))
})

test_that("All words must be present", {
    expect_equal(termsearch(c("Diabetes type 2", "History of type 2 diabetes", "Type 1 diabetes"),
                            c("type 2 diabetes")),
                 c(TRUE, TRUE, FALSE))
    expect_equal(termsearch(c("Diabetes type 2", "History of type 2 diabetes", "Type 1 diabetes"),
                            c("diabetes history", "diabetes type")),
                 c(TRUE, TRUE, TRUE))
})

test_that("Use quotes to match exactly", {
    expect_equal(termsearch(c("Type 2 diabetes", "History of type 2 diabetes", "Diabetes, type 2"),
                            c('"type 2 diabetes"')),
                 c(TRUE, TRUE, FALSE))
    expect_equal(termsearch(c("Type 2 diabetes"),
                            c('type 2 "diabetes"')),
                 c(FALSE))
})

test_that("Wildcards allow partial word searching", {
    expect_equal(termsearch(c("Diabetes", "Diabetic patient"),
                            c("diabet*")),
                 c(TRUE, TRUE))
    expect_equal(termsearch(c("Diabetes", "Diabetic patient", "Patient is diabetic"),
                            c("diabetic pat*")),
                 c(FALSE, TRUE, TRUE))
    expect_equal(termsearch(c("Diabetes", "Diabetic patient", "Patient is diabetic"),
                            c("diabet*", "pat*")),
                 c(TRUE, TRUE, TRUE))
})
