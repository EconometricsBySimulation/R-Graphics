# Date of last change: 2014-05-01
# Last changed by: ccampbell
# 
# Original author: ccampbell
###############################################################################

context("draw stick")

###############################################################################

test_that("drawStick", {
    
    dir.create("test-stick", showWarnings = FALSE)
    
    png(file = "test-stick/test-drawStick-01.png", height = 400, width = 400, res = 72)
    
    plot(0:1, 0:1, type = "n")
    
    a1 <- drawStick()
    expect_that(names(a1), equals(c("head", "arms", "legs")))
    
    a2 <- drawStick(0.25 ,0, gender = "female", face = "annoyed", 
        arms = "up", legs = "apart", clcol = 3)
    
    dev.off()
    
    l2 <- list(head = matrix(c(46, 76, 2, 2, 
            54, 76, 2, 1, 
            46, 66, 55, 68, 
            0, 0, Inf, 0), nrow = 4, byrow = TRUE), 
        arms = matrix(c(30, 50, 70, 
            65, 55, 65), nrow = 2, byrow = TRUE), 
        legs = matrix(c(35, 50, 65, 
            5, 35, 5), nrow = 2, byrow = TRUE))
    
    expect_that(a2, equals(l2))
    
    if (require(visualTest)) {
        
        finger1 <- c(6L, 3L, 3L, 25L, 3L, 4L, 14L, 3L, 6L, 3L, 4L, 25L, 7L, 3L, 
            15L, 31L, 17L, 6L, 3L, 12L, 3L, 10L, 5L, 6L, 15L, 29L, 19L, 3L, 
            3L, 27L, 6L, 3L, 6L, 3L, 3L, 15L, 3L, 12L, 16L, 3L, 5L)
        
        expect_that(isSimilar(file = "test-stick/test-drawStick-01.png", 
            fingerprint = finger1), is_true())
    }
})

