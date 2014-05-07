# Date of last change: 2014-04-01
# Last changed by: ccampbell
# 
# Original author: ccampbell
###############################################################################

context("plot stick")

###############################################################################


test_that("plotStick", {
    
    a0 <- try(plotStick(), silent = TRUE)
    
    expect_that(paste(a0), equals("Error in plotStick() : x is missing\n"))
    
    dir.create("test-stick", showWarnings = FALSE)
    
    png(file = "test-stick/test-plotStick-01.png", height = 400, width = 400, res = 72)
    
    plotStick(1:3, rep(1, 3), gender = "female", face = "annoyed", 
        arms = "up", legs = "apart", clcol = 3, cex = 2)
    
    points(1:3, rep(1, 3), cex = 2, col = "red")
    points(1:3, rep(1, 3), cex = 2, pch = 4, col = "red")
    
    dev.off()
    
    png(file = "test-stick/test-plotStick-02.png", height = 400, width = 400, res = 72)
    
    plotStick(x = 1:10, y = c(1:5, 5:1))
    
    dev.off()
    
    png(file = "test-stick/test-plotStick-03.png", height = 400, width = 400, res = 72)
    
    plotStick(x = 1:10, y = sin(1:10), hatcol = 2, shcol = rainbow(10), 
        clcol = 1:10, gender = c("male", "female"), 
        arms = c("down", "neutral", "up", "hip", "wave"))
    
    dev.off()
    
    png(file = "test-stick/test-plotStick-04.png", height = 400, width = 400, res = 72)
    
    plotStick(x = 1:10, y = rep(1, 10), hatcol = rainbow(10), 
        hat = c("fedora", "beanie", "bill", "shapka"), cex = 3)
    
    dev.off()
    
    if (require(visualTest)) {
        
        finger1 <- c(19L, 26L, 28L, 25L, 25L, 27L, 15L, 3L, 6L, 5L, 13L, 3L, 11L, 
            7L, 6L, 3L, 11L, 27L, 27L, 25L, 28L, 26L, 21L)
        
        finger2 <- c(19L, 27L, 28L, 27L, 27L, 26L, 26L, 13L, 3L, 11L, 26L, 26L, 
            27L, 27L, 26L, 29L, 17L)
        
        finger3 <- c(19L, 26L, 28L, 26L, 24L, 3L, 6L, 20L, 28L, 12L, 3L, 10L, 28L, 
            22L, 4L, 5L, 22L, 26L, 28L, 24L, 23L, 4L)
        
        finger4 <- c(9L, 5L, 5L, 27L, 27L, 4L, 21L, 5L, 5L, 3L, 14L, 19L, 6L, 8L, 
            10L, 9L, 4L, 5L, 6L, 3L, 6L, 4L, 5L, 9L, 9L, 8L, 7L, 17L, 16L, 
            3L, 5L, 4L, 21L, 5L, 23L, 27L, 8L, 4L, 9L)
        
        expect_that(isSimilar(file = "test-stick/test-plotStick-01.png", 
            fingerprint = finger1), is_true())
        
        expect_that(isSimilar(file = "test-stick/test-plotStick-02.png", 
            fingerprint = finger2), is_true())
            
        expect_that(isSimilar(file = "test-stick/test-plotStick-03.png", 
            fingerprint = finger3), is_true())
            
        expect_that(isSimilar(file = "test-stick/test-plotStick-04.png", 
            fingerprint = finger4), is_true())
    }
})

