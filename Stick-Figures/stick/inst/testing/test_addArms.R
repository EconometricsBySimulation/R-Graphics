# Date of last change: 2014-02-11
# Last changed by: ccampbell
# 
# Original author: ccampbell
###############################################################################

context("adding arms")

###############################################################################

test_that("addArms", {
    
    a1 <- addArms(arms = "neutral", linecol = 2)
    expect_that(a1, equals(matrix(c(25, 50, 75,
            55, 55, 55), nrow = 2, byrow = TRUE)))
    
    a2 <- addArms(arms = "hip", linecol = 4)
    expect_that(a2, equals(matrix(c(48, 37, 50, 63, 51,
            40, 47, 55, 49, 62), nrow = 2, byrow = TRUE)))
    
    a3 <- addArms(arms = "wave", linecol = 5)
    expect_that(a3, equals(matrix(c(33, 38, 50, 63, 52,
            78, 60, 55, 47, 40), nrow = 2, byrow = TRUE)))
    
    a4 <- addArms(shcol = 2)
    expect_that(a4, equals(matrix(c(35, 50, 65,
            35, 55, 35), nrow = 2, byrow = TRUE)))
    
    a5 <- addArms(arms = "up", linecol = 3, shcol = 3)
    expect_that(a5, equals(matrix(c(30, 50, 70,
            65, 55, 65), nrow = 2, byrow = TRUE)))
    
    a6 <- addArms(arms = "neutral", shcol = 4)
    expect_that(a6, equals(matrix(c(25, 50, 75,
            55, 55, 55), nrow = 2, byrow = TRUE)))
    }
)

