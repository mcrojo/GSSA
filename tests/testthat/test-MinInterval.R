context("Minimum in interval")
test_that("Minimum of the function must be in the given interval", {
f<- function(x) x^2/2-2*x+3
test<-gsoptim(f,c(-10,10),1e-12)
expect_match(test@min<=test@int.upper & test@int.lower<=test@min, "TRUE") 
})
