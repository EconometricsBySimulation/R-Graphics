# Date of last change: 2014-05-01
# Last changed by: ccampbell
# 
# Original author: ccampbell
###############################################################################

context("adding arms")

###############################################################################

test_that("addArms", {
    
    dir.create("test-stick", showWarnings = FALSE)
    
    png(file = "test-stick/test-addArms-01.png", height = 400, width = 400, res = 72)
    
    plot(0:1, 0:1, type = "n")
    
    a1 <- addArms(arms = "neutral", linecol = 2)
    expect_that(a1, equals(matrix(c(25, 50, 75,
            55, 55, 55), nrow = 2, byrow = TRUE)))
    
    a2 <- addArms(arms = "hip", linecol = 4)
    expect_that(a2, equals(matrix(c(48, 37, 50, 63, 51,
            40, 47, 55, 49, 62), nrow = 2, byrow = TRUE)))
    
    a3 <- addArms(arms = "wave", linecol = 5)
    expect_that(a3, equals(matrix(c(33, 38, 50, 63, 52,
            78, 60, 55, 47, 40), nrow = 2, byrow = TRUE)))
    
    dev.off()
    
    png(file = "test-stick/test-addArms-02.png", height = 400, width = 400, res = 72)
    
    plot(0:1, 0:1, type = "n")
    
    a4 <- addArms(shcol = 2)
    expect_that(a4, equals(matrix(c(35, 50, 65,
            35, 55, 35), nrow = 2, byrow = TRUE)))
    
    a5 <- addArms(arms = "up", linecol = 3, shcol = 3)
    expect_that(a5, equals(matrix(c(30, 50, 70,
            65, 55, 65), nrow = 2, byrow = TRUE)))
    
    a6 <- addArms(arms = "neutral", shcol = 4)
    expect_that(a6, equals(matrix(c(25, 50, 75,
            55, 55, 55), nrow = 2, byrow = TRUE)))
    
    dev.off()
    
    if (require(visualTest)) {
        
        finger1 <- c(27L, 28L, 27L, 5L, 3L, 15L, 31L, 26L, 12L, 3L, 10L, 26L, 29L, 
            19L, 3L, 3L, 27L, 28L, 25L, 25L)
        
        finger2 <- c(20L, 4L, 23L, 26L, 28L, 8L, 17L, 29L, 14L, 3L, 8L, 10L, 3L, 
            3L, 3L, 8L, 10L, 3L, 12L, 29L, 19L, 7L, 27L, 24L, 26L, 5L, 19L, 4L)
        
        expect_that(isSimilar(file = "test-stick/test-addArms-01.png", 
            fingerprint = finger1), is_true())
        
        expect_that(isSimilar(file = "test-stick/test-addArms-02.png", 
            fingerprint = finger2), is_true())
    }
})

